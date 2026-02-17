##########################
## S4 class definition and create function
##########################

#' Galaxy session object
#'
#' An S4 class used to carry state across a pipe‑based workflow against a
#' Galaxy instance.
#'
#' @slot history_name Default name to give to a new history.
#' @slot history_id Encoded ID of the history on the server.
#' @slot input_dataset_id Encoded ID of the last uploaded input dataset.
#' @slot inputs A list of tool/workflow inputs to be applied on the next call.
#' @slot invocation_id Encoded ID of the last workflow invocation.
#' @slot output_dataset_ids Character vector of encoded output dataset IDs.
#' @slot state One of `"new"`, `"pending"`, `"success"` or `"error"`.
#' @slot galaxy_url Base URL of the Galaxy instance.
#' @exportClass Galaxy
setClass(
  "Galaxy",
  slots = list(
    history_name = "character",
    history_id = "character",
    input_dataset_id = "character",
    inputs = "list",
    invocation_id = "character",
    output_dataset_ids = "character",
    state = "character",
    galaxy_url = "character"
  ),
  prototype = list(
    history_name = "R API request",
    history_id = NA_character_,
    input_dataset_id = NA_character_,
    inputs = list(),
    invocation_id = NA_character_,
    output_dataset_ids = character(0),
    state = "new",
    galaxy_url = NA_character_
  )
)

setValidity("Galaxy", function(object) {
  allowed_states <- c("new", "pending", "success", "error")

  if (!object@state %in% allowed_states) {
    return(
      paste(
        "state must be one of:",
        paste(allowed_states, collapse = ", ")
      )
    )
  }

  if (object@state != "new" && !nzchar(object@galaxy_url)) {
    return("galaxy_url must be set once the Galaxy object is initialized.")
  }

  TRUE
})

#' Create a Galaxy session object
#'
#' Constructor for a `Galaxy` S4 object used for pipe‑based
#' workflows. The returned object carries identifiers such as `history_id`,
#' `input_dataset_id` and `invocation_id` through subsequent calls.
#'
#' @param history_name Character. Default name to give to a new history,
#'   stored in the object and used by `galaxy_initialize()` if you don’t
#'   override it.
#' @param galaxy_url Character. Base URL of the Galaxy instance. If the
#'   environment variable `GALAXY_URL` is set, it takes precedence.
#'
#' @return A `Galaxy` object in state `"new"`.
#' @import methods
#' @export
galaxy <- function(history_name = "R API request", galaxy_url = "https://usegalaxy.eu") {
  resolved_url <- .resolve_galaxy_url(galaxy_url)

  obj <- new(
    "Galaxy",
    history_name = history_name,
    galaxy_url = resolved_url,
    state = "new"
  )

  validObject(obj)
  obj
}

#############################
## initialize history
#############################


#' @keywords internal
#' @noRd
.galaxy_initialize <- function(name = "R API request", galaxy_url = "https://usegalaxy.eu") {
  api_key <- Sys.getenv("GALAXY_API_KEY")

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

  hist_res <- httr::POST(
    paste0(galaxy_url, "/api/histories"),
    httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"),
    body = jsonlite::toJSON(list(name = name), auto_unbox = TRUE)
  )
  httr::stop_for_status(hist_res)
  history <- httr::content(hist_res, "parsed")
  history_id <- history$id
  message("Using history:", history_id, "\n")
  return(history_id)
}

#' Create a new Galaxy history
#'
#' `galaxy_initialize()` is an S4 generic. With no `x` supplied it creates a
#' new history on the given Galaxy instance and returns its encoded ID. When
#' called with a `Galaxy` object it uses the object’s `history_name` and
#' `galaxy_url`, creates the history, and updates the object with the new
#' `history_id` and state `"pending"`.
#'
#' A valid Galaxy API key is required and must be available via the
#' `GALAXY_API_KEY` environment variable.
#'
#' @param x A `Galaxy` object, or missing to use the default method.
#' @param name Name of the history to create. Ignored when `x` is a
#'   `Galaxy`, in which case `x@history_name` is used.
#' @param galaxy_url Base URL of the Galaxy instance. Ignored when `x` is a
#'   `Galaxy`, in which case `x@galaxy_url` is used.
#' @return For the default method (`x` missing), a character scalar history ID.
#'   For the `Galaxy` method, the modified `Galaxy` object.
#' @examplesIf galaxy_has_key()
#' history_id <- galaxy_initialize("My history name")
#' g <- galaxy(history_name = "My history name")
#' g <- galaxy_initialize(g)
#' @export
setGeneric("galaxy_initialize",
           function(x, name = "R API request", galaxy_url = "https://usegalaxy.eu")
             standardGeneric("galaxy_initialize"),
           signature = "x")

#' @rdname galaxy_initialize
#' @export
setMethod("galaxy_initialize", "missing",
          function(name, galaxy_url) .galaxy_initialize(name, galaxy_url))


#' @rdname galaxy_initialize
#' @export
setMethod("galaxy_initialize", "Galaxy",
          function(x) {
            x@history_id <- .galaxy_initialize(x@history_name, x@galaxy_url)
            x@state <- "pending"
            validObject(x)
            x
          })

#############################
## File upload
#############################


#' @keywords internal
#' @noRd
.galaxy_upload_ftp <- function(input_file,
                               history_id,
                               galaxy_ftp = "ftp.usegalaxy.eu",
                               galaxy_url = "https://usegalaxy.eu") {
  api_key  <- Sys.getenv("GALAXY_API_KEY")
  username <- Sys.getenv("GALAXY_USERNAME")
  password <- Sys.getenv("GALAXY_PASSWORD")
  galaxy_url <- .resolve_galaxy_url(galaxy_url)

  username_enc <- utils::URLencode(username, reserved = TRUE)
  password_enc <- utils::URLencode(password, reserved = TRUE)
  ftp_url <- paste0("ftp://", username_enc, ":", password_enc, "@", galaxy_ftp, "/")

  system2("curl",
          c("--ssl-reqd", "-T", shQuote(input_file), ftp_url),
          stdout = TRUE, stderr = TRUE)

  ftp_filename <- basename(input_file)
  fetch_payload <- list(
    history_id = history_id,
    targets = list(list(
      destination = list(type = "hdas"),
      elements = list(list(
        src      = "ftp_import",
        ftp_path = ftp_filename,
        ext      = "auto",
        dbkey    = "?"
      ))
    ))
  )
  res <- httr::POST(
    paste0(galaxy_url, "/api/tools/fetch"),
    httr::add_headers(`x-api-key` = api_key,
                      `Content-Type` = "application/json"),
    body = jsonlite::toJSON(fetch_payload, auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  httr::content(res, "parsed")$outputs[[1]]$id
}

## generic with dispatch on x
#' Generic upload ftp
#' @rdname galaxy_upload_ftp
#' @export
setGeneric("galaxy_upload_ftp",
           function(x,
                    input_file,
                    galaxy_ftp = "ftp.usegalaxy.eu",
                    galaxy_url = "https://usegalaxy.eu",
                    ...)
             standardGeneric("galaxy_upload_ftp"),
           signature = "x")


#' FTP file upload to Galaxy
#'
#' `galaxy_upload_ftp()` is an S4 generic. With no `x` supplied it uploads a
#' local file via FTP and registers it in the specified history, returning the
#' encoded dataset ID. When called with a `Galaxy` object it uses the
#' object's `history_id` and `galaxy_url` and updates the object with the new
#' `input_dataset_id`.
#'
#' A valid API key (`GALAXY_API_KEY`) and FTP credentials (`GALAXY_USERNAME`,
#' `GALAXY_PASSWORD`) must be available in the environment.
#'
#' @param x A `Galaxy` object, or a `history_id` to use the default method.
#' @param input_file Path to the local file to upload.
#' @param galaxy_ftp FTP server address of the Galaxy instance.
#' @param galaxy_url Base URL of the Galaxy instance, used by the default
#'   method. If `GALAXY_URL` is set it takes precedence.
#' @param ... not in use
#' @return For the default method, a character scalar dataset ID. For the
#'   `Galaxy` method, the modified `Galaxy` object.
#' @examplesIf galaxy_has_key() && nzchar(Sys.getenv("GALAXY_USERNAME")) && nzchar(Sys.getenv("GALAXY_PASSWORD"))
#' galaxy_ftp <- "ftp.usegalaxy.eu"
#' input_file <- tempfile(fileext = ".txt")
#' writeLines("Example", input_file)
#' hid <- galaxy_initialize("test upload")
#' did <- galaxy_upload_ftp(input_file, hid, galaxy_ftp)
#' g <- galaxy()
#' g <- galaxy_initialize(g)
#' g <- galaxy_upload_ftp(g, input_file, galaxy_ftp = galaxy_ftp)
#' @rdname galaxy_upload_ftp
#' @export
setMethod("galaxy_upload_ftp", "character",
          function(x,
                   input_file,
                   galaxy_ftp = "ftp.usegalaxy.eu",
                   galaxy_url = "https://usegalaxy.eu",
                   ...)
            .galaxy_upload_ftp(history_id = x, input_file = input_file, galaxy_ftp, galaxy_url))

## method for Galaxy objects: update the object and return it
#' Galaxy upload via ftp S4 method
#' @rdname galaxy_upload_ftp
#' @export
setMethod("galaxy_upload_ftp", "Galaxy",
          function(x,
                   input_file,
                   galaxy_ftp = "ftp.usegalaxy.eu",
                   ...)
          {
            did <- .galaxy_upload_ftp(input_file = input_file, history_id = x@history_id, galaxy_ftp, x@galaxy_url)
            x@input_dataset_id <- did
            validObject(x)
            x
          })

# internal helper, not exported
#' @keywords internal
#' @noRd
.galaxy_upload_https <- function(
    input_file,
    history_id,
    wait        = FALSE,
    wait_timeout = 600,
    galaxy_url  = "https://usegalaxy.eu",
    file_type   = "auto",
    dbkey       = "?"
) {
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  if (!file.exists(input_file))
    stop("input_file does not exist: ", input_file)
  if (missing(history_id) || !nzchar(history_id))
    stop("history_id is required.")
  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key))
    stop("GALAXY_API_KEY environment variable is not set.")

  galaxy_wait_for_dataset <- function(
    dataset_id,
    galaxy_url    = "https://usegalaxy.eu",
    poll_interval = 3,
    timeout       = 600
  ) {
    api_key <- Sys.getenv("GALAXY_API_KEY")
    start_time <- Sys.time()
    repeat {
      res <- httr::GET(
        url = paste0(galaxy_url, "/api/datasets/", dataset_id),
        httr::add_headers(`x-api-key` = api_key)
      )
      httr::stop_for_status(res)
      ds <- httr::content(res, as = "parsed")
      if (ds$state == "ok") {
        return(ds)
      }
      if (ds$state == "error") {
        stop("Galaxy dataset failed: ", ds$misc_info)
      }
      if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {
        stop("Timed out waiting for dataset to finish")
      }
      Sys.sleep(poll_interval)
    }
  }

  targets <- list(list(
    destination = list(type = "hdas"),
    elements = list(list(
      dbkey = dbkey,
      ext = file_type,
      name = basename(input_file),
      space_to_tab = FALSE,
      src = "files",
      to_posix_lines = TRUE
    ))
  ))

  res <- httr::POST(
    url = paste0(galaxy_url, "/api/tools/fetch"),
    httr::add_headers(`x-api-key` = api_key),
    body = list(
      auto_decompress = TRUE,
      history_id = history_id,
      targets = jsonlite::toJSON(targets, auto_unbox = TRUE),
      files_0 = httr::upload_file(input_file)
    ),
    encode = "multipart"
  )
  httr::stop_for_status(res)
  response <- httr::content(res, as = "parsed")
  dataset_id <- response$outputs[[1]]$id

  if (wait) {
    galaxy_wait_for_dataset(
      dataset_id = dataset_id,
      galaxy_url = galaxy_url,
      timeout = wait_timeout
    )
  }

  dataset_id
}

#' Generic upload file with https
#' @rdname galaxy_upload_https
#' @export
setGeneric("galaxy_upload_https",
           function(x,
                    input_file,
                    wait         = FALSE,
                    wait_timeout = 600,
                    galaxy_url   = "https://usegalaxy.eu",
                    file_type    = "auto",
                    dbkey        = "?",
                    ...)
             standardGeneric("galaxy_upload_https"),
           signature = "x")

#' Upload a dataset via HTTPS (direct POST) into Galaxy
#'
#' `galaxy_upload_https()` is an S4 generic. With no `x` supplied it uploads a
#' local file via HTTPS to the specified history and returns the encoded dataset
#' ID. When called with a `Galaxy` object it uses the object's `history_id` and
#' `galaxy_url`, uploads the file, and updates the object with the new
#' `input_dataset_id`.
#'
#' This uses Galaxy's built‑in `upload1` tool and performs a multipart form
#' POST. Large files may still require FTP depending on server configuration.
#' A valid API key (`GALAXY_API_KEY`) must be available in the environment.
#'
#' @param x A `Galaxy` object, or a `history_id` to use the default method.
#' @param input_file Path to the local file to upload.
#' @param wait Logical. Whether to wait for Galaxy to finish processing.
#' @param wait_timeout Time in seconds until `wait` times out with an error.
#' @param galaxy_url Base URL of the Galaxy instance, used by the default method.
#'   If `GALAXY_URL` is set it takes precedence.
#' @param file_type Galaxy datatype identifier (e.g. `"auto"`, `"fastq"`, `"bam"`).
#' @param dbkey Reference genome identifier (e.g. `"?"` or `"hg38"`).
#' @param ... not in use
#' @return For the default method, a character scalar dataset ID. For the
#'   `Galaxy` method, the modified `Galaxy` object.
#' @examplesIf galaxy_has_key()
#' hid <- galaxy_initialize("test upload")
#' test_file <- tempfile(fileext = ".txt")
#' writeLines("This is an example test file.", test_file)
#' file_id <- galaxy_upload_https(hid, test_file)
#' g <- galaxy()
#' g <- galaxy_initialize(g)
#' g <- galaxy_upload_https(g, test_file)
#' @rdname galaxy_upload_https
#' @export
setMethod("galaxy_upload_https", "character",
          function(x,
                   input_file,
                   wait         = FALSE,
                   wait_timeout = 600,
                   galaxy_url   = "https://usegalaxy.eu",
                   file_type    = "auto",
                   dbkey        = "?",
                   ...) {
            .galaxy_upload_https(input_file = input_file,
                                 history_id = x,
                                 wait       = wait,
                                 wait_timeout = wait_timeout,
                                 galaxy_url = galaxy_url,
                                 file_type  = file_type,
                                 dbkey      = dbkey)
          })

#' S4 Method for galaxy https upload
#' @rdname galaxy_upload_https
#' @export
setMethod("galaxy_upload_https", "Galaxy",
          function(x,
                   input_file,
                   wait         = FALSE,
                   wait_timeout = 600,
                   file_type    = "auto",
                   dbkey        = "?",
                   ...)
          {
            did <- .galaxy_upload_https(input_file,
                                        x@history_id,
                                        wait         = wait,
                                        wait_timeout = wait_timeout,
                                        galaxy_url   = x@galaxy_url,
                                        file_type    = file_type,
                                        dbkey        = dbkey)
            x@input_dataset_id <- did
            validObject(x)
            x
          })

#########################
## Workflow invocation and polling
#########################

#' Internal helper to build workflow inputs
#' @keywords internal
#' @noRd
.galaxy_build_wf_inputs <- function(wf_def, dataset_id = NULL, args = list()) {
  if (is.null(args)) args <- list()
  if (!is.null(dataset_id)) {
    wf_inputs <- names(wf_def$inputs)
    if (!any(wf_inputs %in% names(args))) {
      args[[wf_inputs[1L]]] <- list(src = "hda", id = dataset_id)
    }
  }
  args
}

#' Internal helper to validate workflow inputs
#' @keywords internal
#' @noRd
.galaxy_validate_wf_inputs <- function(wf_def, inputs) {
  # top–level workflow inputs
  expected <- names(wf_def$inputs)

  # allow overrides of tool parameters: step_id|param_name
  step_allowed <- character()
  if (!is.null(wf_def$steps)) {
    for (st in wf_def$steps) {
      if (!is.null(st$tool_id) && length(st$tool_inputs)) {
        params <- names(st$tool_inputs)
        step_allowed <- c(step_allowed,
                          paste(st$id, params, sep = "|"))
      }
    }
  }

  allowed <- c(expected, step_allowed)
  unknown <- setdiff(names(inputs), allowed)
  if (length(unknown)) {
    stop("Unknown workflow inputs: ", paste(unknown, collapse = ", "))
  }

  ## only the true workflow inputs without defaults are required
  has_default <- function(inp) !is.null(inp$value)
  req_idx <- !vapply(wf_def$inputs,
                     function(inp) isTRUE(inp$optional) || has_default(inp),
                     logical(1L))
  required <- expected[req_idx]
  missing  <- setdiff(required, names(inputs))
  if (length(missing)) {
    stop("Missing required workflow inputs: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

# internal helper, not exported
#' @keywords internal
#' @noRd
.galaxy_start_workflow <- function(history_id,
                                   workflow_id,
                                   inputs     = NULL,
                                   dataset_id = NULL,
                                   galaxy_url = "https://usegalaxy.eu") {
  if (missing(workflow_id) || !nzchar(workflow_id)) {
    stop("workflow_id is required.")
  }
  wf_def   <- galaxy_get_workflow(workflow_id, galaxy_url = galaxy_url)
  built_in <- .galaxy_build_wf_inputs(wf_def, dataset_id = dataset_id, args = inputs)
  .galaxy_validate_wf_inputs(wf_def, built_in)

  run_body <- list(inputs = built_in, history_id = history_id)
  run_url  <- paste0(.resolve_galaxy_url(galaxy_url),
                     "/api/workflows/", workflow_id, "/invocations")
  res <- httr::POST(run_url,
                    httr::add_headers(`x-api-key` = Sys.getenv("GALAXY_API_KEY"),
                                      `Content-Type` = "application/json"),
                    body = jsonlite::toJSON(run_body, auto_unbox = TRUE))
  httr::stop_for_status(res)
  httr::content(res, "parsed")$id
}

## generic dispatches on the first argument
#' Generic start workflow
#' @rdname galaxy_start_workflow
#' @export
setGeneric("galaxy_start_workflow",
           function(x,
                    workflow_id,
                    inputs     = NULL,
                    dataset_id = NULL,
                    galaxy_url = "https://usegalaxy.eu")
             standardGeneric("galaxy_start_workflow"),
           signature = "x")

#' Start a Galaxy workflow with inputs and parameters
#'
#' `galaxy_start_workflow()` is an S4 generic. With `x` as a character vector
#' it is treated as a history ID: the given workflow is invoked in that history
#' and the invocation ID is returned. With `x` as a `Galaxy` object, the
#' history ID and URL are taken from the object; the workflow is started and
#' the object is updated with the resulting `invocation_id`.
#'
#' @param x A `Galaxy` object, or a history ID (`character`) to use the default
#'   method.
#' @param workflow_id Character. Galaxy workflow ID.
#' @param dataset_id Character. ID of the input dataset (HDA). Ignored if
#'   `inputs` is supplied. When `x` is a `Galaxy` and `dataset_id` is missing,
#'   `x@input_dataset_id` is used.
#' @param inputs Named list. Optional workflow input mapping; keys are workflow
#'   input step IDs, values are lists describing datasets/parameters.
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method. If `GALAXY_URL` is set it takes precedence.
#' @return For the character method, a character scalar invocation ID. For the
#'   `Galaxy` method, the modified `Galaxy` object.
#' @rdname galaxy_start_workflow
#' @export
setMethod("galaxy_start_workflow", "character",
          function(x, workflow_id, inputs = NULL, dataset_id = NULL,
                   galaxy_url = "https://usegalaxy.eu") {
            .galaxy_start_workflow(history_id = x,
                                   workflow_id = workflow_id,
                                   inputs      = inputs,
                                   dataset_id  = dataset_id,
                                   galaxy_url  = galaxy_url)
          })

#' S4 function to start a galaxy workflow
#' @rdname galaxy_start_workflow
#' @export
setMethod("galaxy_start_workflow", "Galaxy",
          function(x, workflow_id, inputs = NULL, dataset_id = NULL) {
            inv <- .galaxy_start_workflow(history_id = x@history_id,
                                          workflow_id = workflow_id,
                                          inputs      = if (is.null(inputs)) x@inputs else inputs,
                                          dataset_id  = if (is.null(dataset_id)) x@input_dataset_id else dataset_id,
                                          galaxy_url  = x@galaxy_url)
            x@invocation_id <- inv
            x@state <- "pending"
            validObject(x)
            x
          })

#' Helper function for workflow polling
#' @keywords internal
#' @noRd
.galaxy_poll_workflow <- function(invocation_id,
                                  galaxy_url    = "https://usegalaxy.eu",
                                  poll_interval = 30) {
  api_key   <- Sys.getenv("GALAXY_API_KEY")
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  any_error <- FALSE

  repeat {
    ## Get workflow invocation
    status_res <- httr::GET(
      paste0(galaxy_url, "/api/invocations/", invocation_id),
      httr::add_headers(`x-api-key` = api_key)
    )
    httr::stop_for_status(status_res)
    status <- httr::content(status_res, "parsed")
    steps  <- status$steps

    ## Get all job IDs from the steps
    job_ids <- lapply(steps, function(step) step$job_id)
    job_ids <- job_ids[!sapply(job_ids, is.null)]
    job_ids <- job_ids[nzchar(job_ids)]
    if (!length(job_ids)) {
      message(Sys.time(), " ,No jobs yet, waiting...")
      Sys.sleep(poll_interval)
      next
    }

    ## Check each job state
    job_states <- vapply(job_ids, function(jid) {
      job_res <- httr::GET(
        paste0(galaxy_url, "/api/jobs/", jid),
        httr::add_headers(`x-api-key` = api_key)
      )
      httr::content(job_res, "parsed")$state
    }, character(1L))

    message(Sys.time(), " ,Job states: ", paste(job_states, collapse = ", "))

    if (all(job_states == "ok")) {
      message("All jobs finished successfully!")
      break
    }
    if (any(job_states %in% c("error", "failed", "deleted"))) {
      any_error <- TRUE
      message("Some workflow jobs failed or were cancelled.")
      break
    }
    Sys.sleep(poll_interval)
  }

  ## Once all jobs are ok, return the HDA IDs in the workflow history
  history_id <- status$history_id
  datasets_res <- httr::GET(
    paste0(galaxy_url, "/api/histories/", history_id, "/contents"),
    httr::add_headers(`x-api-key` = api_key)
  )
  datasets <- httr::content(datasets_res, "parsed")
  output_ids <- vapply(datasets, function(d) {
    if (isTRUE(d$state == "ok") && !isTRUE(d$deleted)) d$id else NA_character_
  }, character(1L))
  output_ids <- output_ids[!is.na(output_ids)]

  list(success = !any_error, output_ids = output_ids)
}

#' Generic for polling workflows
#' @rdname galaxy_poll_workflow
#' @export
setGeneric("galaxy_poll_workflow",
           function(x,
                    galaxy_url    = "https://usegalaxy.eu",
                    poll_interval = 30,
                    ...)
             standardGeneric("galaxy_poll_workflow"),
           signature = "x")

#' Poll a Galaxy workflow invocation until completion
#'
#' `galaxy_poll_workflow()` is an S4 generic. With `x` as a character vector it
#' is treated as a workflow invocation ID; the invocation is polled until it
#' completes and a list of output dataset IDs is returned. With `x` as a
#' `Galaxy` object, the `invocation_id` and `galaxy_url` are taken from the
#' object, and the object is updated with the resulting `output_dataset_ids` and
#' state.
#'
#' @param x A workflow invocation ID (`character`) or a `Galaxy` object.
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method. If `GALAXY_URL` is set it takes precedence.
#' @param poll_interval Time in seconds between polling attempts.
#' @param ... not in use
#' @return For the character method, a list with elements `success` and
#'   `output_ids`. For the `Galaxy` method, the modified `Galaxy` object.
#' @examplesIf galaxy_has_key()
#' invocation_id <- "abc123"
#' galaxy_poll_workflow(invocation_id)
#' @rdname galaxy_poll_workflow
#' @export
setMethod("galaxy_poll_workflow", "character",
          function(x,
                   galaxy_url    = "https://usegalaxy.eu",
                   poll_interval = 30,
                   ...) {
            .galaxy_poll_workflow(invocation_id = x,
                                  galaxy_url    = galaxy_url,
                                  poll_interval = poll_interval)
          })

#' S4 object galaxy workflow polling function
#' @rdname galaxy_poll_workflow
#' @export
setMethod("galaxy_poll_workflow", "Galaxy",
          function(x,
                   poll_interval = 30,
                   ...) {
            res <- .galaxy_poll_workflow(invocation_id = x@invocation_id,
                                         galaxy_url    = x@galaxy_url,
                                         poll_interval = poll_interval)
            x@output_dataset_ids <- res$output_ids
            x@state <- if (isTRUE(res$success)) "success" else "error"
            validObject(x)
            x
          })

#############################
## File download
#############################

#' Helper function for unique naming
#' @keywords internal
#' @noRd
.make_unique_names <- function(names, out_dir, overwrite = FALSE) {
  out <- character(length(names))
  for (i in seq_along(names)) {
    nm   <- names[i]
    if (!nzchar(nm)) nm <- sprintf("dataset_%02d", i)
    ext  <- tools::file_ext(nm)
    base <- if (nzchar(ext)) tools::file_path_sans_ext(nm) else nm
    cand <- nm
    idx  <- 1L
    while (cand %in% out ||
           (!overwrite && file.exists(file.path(out_dir, cand)))) {
      cand <- if (nzchar(ext)) sprintf("%s_%d.%s", base, idx, ext)
      else sprintf("%s_%d", base, idx)
      idx <- idx + 1L
    }
    if (cand != nm) {
      warning("File '", nm, "' exists; using '", cand, "' instead.")
    }
    out[i] <- cand
  }
  out
}

#' Helper function for downloading the results of a history
#' @keywords internal
#' @noRd
.galaxy_download_result <- function(output_ids,
                                    out_dir   = ".",
                                    galaxy_url = "https://usegalaxy.eu",
                                    overwrite = FALSE) {
  if (is.list(output_ids) && "output_ids" %in% names(output_ids)) {
    output_ids <- output_ids$output_ids
  }
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  api_key    <- Sys.getenv("GALAXY_API_KEY")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  info <- galaxy_get_file_info(output_ids, galaxy_url = galaxy_url)
  targets <- .make_unique_names(info$name, out_dir, overwrite = overwrite)

  mapply(function(fid, fname) {
    dest <- file.path(out_dir, fname)
    httr::GET(
      paste0(galaxy_url, "/api/datasets/", fid, "/display"),
      httr::add_headers(`x-api-key` = api_key),
      httr::write_disk(dest, overwrite = TRUE)  # we've ensured uniqueness
    )
  }, info$id, targets, SIMPLIFY = FALSE)
}

#' Generic for downloading files from a history
#' @rdname galaxy_download_result
#' @export
setGeneric("galaxy_download_result",
           function(x,
                    out_dir    = ".",
                    galaxy_url = "https://usegalaxy.eu",
                    overwrite  = FALSE)
             standardGeneric("galaxy_download_result"),
           signature = "x")

#' Download result datasets from a Galaxy history
#'
#' `galaxy_download_result()` is an S4 generic. With `x` as a character vector
#' of HDA output IDs, all corresponding datasets are downloaded into `out_dir`
#' using their Galaxy names; duplicate names are disambiguated by appending
#' `_<i>` before the extension. Existing files are not overwritten if
#' `overwrite = FALSE`, and a warning is issued when a name is adjusted.
#' With `x` as a `Galaxy` object its `output_dataset_ids` and `galaxy_url`
#' are used; the object is returned invisibly after performing the downloads.
#'
#' @param x A vector of HDA output IDs (`character`), or a `Galaxy` object.
#' @param out_dir Directory in which to save the downloaded files.
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method.
#' @param overwrite Logical; if `FALSE` (default), do not overwrite existing
#'   files but choose unique names instead.
#' @return For the character method, a list of `httr` responses; for the
#'   `Galaxy` method, the (unchanged) `Galaxy` object invisibly.
#' @rdname galaxy_download_result
#' @export
setMethod("galaxy_download_result", "character",
          function(x,
                   out_dir    = ".",
                   galaxy_url = "https://usegalaxy.eu",
                   overwrite  = FALSE) {
            .galaxy_download_result(output_ids = x,
                                    out_dir    = out_dir,
                                    galaxy_url = galaxy_url,
                                    overwrite  = overwrite)
          })

#' S4 method to download files from a history
#' @rdname galaxy_download_result
#' @export
setMethod("galaxy_download_result", "Galaxy",
          function(x,
                   out_dir   = ".",
                   overwrite = FALSE) {
            .galaxy_download_result(output_ids = x@output_dataset_ids,
                                    out_dir    = out_dir,
                                    galaxy_url = x@galaxy_url,
                                    overwrite  = overwrite)
            invisible(x)
          })


#' @keywords internal
#' @noRd
.galaxy_download_rocrate <- function(history_id,
                                     dest_file     = tempfile(fileext = ".zip"),
                                     galaxy_url    = "https://usegalaxy.eu",
                                     format        = "rocrate.zip",
                                     poll_interval = 30,
                                     timeout       = 600) {
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  api_key    <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) stop("GALAXY_API_KEY is not set.")
  start_time <- Sys.time()

  # request a short-term storage export in RO-Crate format
  prep <- httr::POST(
    paste0(galaxy_url, "/api/histories/", history_id, "/prepare_store_download"),
    httr::add_headers(`x-api-key` = api_key,
                      `Content-Type` = "application/json"),
    body = jsonlite::toJSON(list(model_store_format = format), auto_unbox = TRUE)
  )
  httr::stop_for_status(prep)
  prep_info   <- httr::content(prep, "parsed")
  storage_id  <- prep_info$storage_request_id %||% prep_info$id

  # poll until the export is ready
  repeat {
    ready <- httr::GET(
      paste0(galaxy_url, "/api/short_term_storage/", storage_id, "/ready"),
      httr::add_headers(`x-api-key` = api_key)
    )
    httr::stop_for_status(ready)
    if (isTRUE(httr::content(ready, "parsed"))) break
    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout) {
      stop("Timed out waiting for RO-Crate export.")
    }
    Sys.sleep(poll_interval)
  }

  # download the crate
  httr::GET(
    paste0(galaxy_url, "/api/short_term_storage/", storage_id),
    httr::add_headers(`x-api-key` = api_key),
    httr::write_disk(dest_file, overwrite = TRUE)
  )
  message("RO-Crate downloaded to: ", dest_file)
  return(dest_file)
}

#' Generic for downloading a history as an RO-Crate
#' @rdname galaxy_download_rocrate
#' @export
setGeneric("galaxy_download_rocrate",
           function(x,
                    dest_file     = tempfile(fileext = ".zip"),
                    galaxy_url    = "https://usegalaxy.eu",
                    format        = "rocrate.zip",
                    poll_interval = 5,
                    timeout       = 600)
             standardGeneric("galaxy_download_rocrate"),
           signature = "x")

#' Download a Galaxy history as an RO-Crate
#'
#' `galaxy_download_rocrate()` is an S4 generic. With `x` as a history ID
#' (`character`) it requests an export in RO-Crate format, polls until ready,
#' and downloads the archive to `dest_file`. With `x` as a `Galaxy` object,
#' its `history_id` and `galaxy_url` are used and the object is returned
#' invisibly after performing the download.
#'
#' @param x A history ID (`character`), or a `Galaxy` object.
#' @param dest_file Path to save the downloaded RO-Crate (defaults to a
#'   temporary `.zip` file).
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method. If `GALAXY_URL` is set it takes precedence.
#' @param format Format for the history export. Possible formats depend on the Galaxy
#' server. Typical inputs are 'tgz', 'tar', 'tar.gz', 'bag.zip', 'bag.tar', 'bag.tgz',
#' 'rocrate.zip' or 'bco.json'. Defaults to 'rocrate.zip'.
#' @param poll_interval Seconds between status checks.
#' @param timeout Maximum time to wait in seconds before giving up.
#' @return For the character method, the path to the downloaded file. For the
#'   `Galaxy` method, the (unchanged) `Galaxy` object invisibly.
#' @examplesIf galaxy_has_key()
#' hid <- "0123456789abcdef"
#' crate <- galaxy_download_rocrate(hid, dest_file = "history_rocrate.zip")
#' g <- galaxy()
#' g <- galaxy_initialize(g)
#' g <- galaxy_download_rocrate(g, dest_file = "history_rocrate.zip")
#' @rdname galaxy_download_rocrate
#' @export
setMethod("galaxy_download_rocrate", "character",
          function(x,
                   dest_file     = tempfile(fileext = ".zip"),
                   galaxy_url    = "https://usegalaxy.eu",
                   format        = "rocrate.zip",
                   poll_interval = 30,
                   timeout       = 600) {
            .galaxy_download_rocrate(history_id = x,
                                     dest_file     = dest_file,
                                     galaxy_url    = galaxy_url,
                                     format        =  format,
                                     poll_interval = poll_interval,
                                     timeout       = timeout)
          })

#' @rdname galaxy_download_rocrate
#' @export
setMethod("galaxy_download_rocrate", "Galaxy",
          function(x,
                   dest_file     = tempfile(fileext = ".zip"),
                   format        =  format,
                   poll_interval = 5,
                   timeout       = 600) {
            .galaxy_download_rocrate(history_id = x@history_id,
                                     dest_file     = dest_file,
                                     galaxy_url    = x@galaxy_url,
                                     poll_interval = poll_interval,
                                     timeout       = timeout)
            invisible(x)
          })


#############################
## Tool invocation and polling
#############################

# build an inputs list from a tool definition, a dataset id and a user list
#' @keywords internal
#' @noRd
.galaxy_build_tool_inputs <- function(tool_def,
                                      dataset_id = NULL,
                                      args = list()) {
  if (is.null(args)) args <- list()

  ## find the first data parameter in the tool definition
  param_defs <- tool_def$inputs
  data_param <- NULL
  if (!is.null(dataset_id)) {
    for (p in param_defs) {
      if (!is.null(p$type) && p$type == "data") {
        data_param <- p$name
        break
      }
    }
  }

  ## if no data input supplied and we have a dataset id, insert it
  if (!is.null(data_param) && is.null(args[[data_param]])) {
    args[[data_param]] <- list(src = "hda", id = dataset_id)
  }

  args
}

# very basic name‑based validation
#' @keywords internal
#' @noRd
.galaxy_validate_tool_inputs <- function(tool_def, inputs) {
  expected <- vapply(tool_def$inputs, function(p) p$name, character(1L))
  unknown  <- setdiff(names(inputs), expected)
  if (length(unknown)) {
    stop("Unknown tool inputs: ", paste(unknown, collapse = ", "))
  }

  has_default <- function(p) {
    !is.null(p$value) && !(is.list(p$value) && length(p$value) == 0L)
  }

  req_idx <- !vapply(tool_def$inputs,
                     function(p) isTRUE(p$optional) || has_default(p),
                     logical(1L))
  required <- expected[req_idx]
  missing  <- setdiff(required, names(inputs))
  if (length(missing)) {
    stop("Missing required inputs: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

#' Helper function for single tool invocations
#' @keywords internal
#' @noRd
.galaxy_run_tool <- function(tool_id,
                             history_id,
                             inputs = NULL,
                             dataset_id = NULL,
                             galaxy_url = "https://usegalaxy.eu") {
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  if (missing(tool_id) || !nzchar(tool_id)) stop("tool_id is required.")
  if (missing(history_id) || !nzchar(history_id)) stop("history_id is required.")

  tool_def <- galaxy_get_tool(tool_id, galaxy_url = galaxy_url, tool_version = NULL)
  built    <- .galaxy_build_tool_inputs(tool_def, dataset_id = dataset_id, args = inputs)
  .galaxy_validate_tool_inputs(tool_def, built)

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) stop("GALAXY_API_KEY environment variable is not set.")
  payload <- list(history_id = history_id, tool_id = tool_id, inputs = built)

  res <- httr::POST(
    url = paste0(galaxy_url, "/api/tools"),
    httr::add_headers(
      `x-api-key`   = api_key,
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  job <- httr::content(res, as = "parsed", simplifyVector = FALSE)
  job$jobs[[1]]$id
}

#' Generic run tool
#' @rdname galaxy_run_tool
#' @export
setGeneric("galaxy_run_tool",
           function(x,
                    tool_id,
                    inputs     = NULL,
                    dataset_id = NULL,
                    galaxy_url = "https://usegalaxy.eu"
                    )
             standardGeneric("galaxy_run_tool"),
           signature = "x")

#' Run a Galaxy tool programmatically
#'
#' `galaxy_run_tool()` is an S4 generic. With `x` as a character vector it is
#' treated as a history ID; the specified tool is invoked in that history and
#' the job ID is returned. With `x` as a `Galaxy` object, the history ID and
#' URL are taken from the object and the object is updated with the job ID.
#'
#' @param x A history ID (`character`) or a `Galaxy` object.
#' @param tool_id Tool identifier to execute.
#' @param dataset_id ID of the input dataset (HDA).
#' @param inputs Named list of tool inputs.
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method.
#' @return For the character method, a job ID; for the `Galaxy` method, the
#'   modified `Galaxy` object.
#' @rdname galaxy_run_tool
#' @export
setMethod("galaxy_run_tool", "character",
          function(x, tool_id,
                   inputs = NULL,
                   dataset_id = NULL,
                   galaxy_url = "https://usegalaxy.eu") {
            .galaxy_run_tool(tool_id = tool_id,
                             history_id = x,
                             inputs = inputs,
                             dataset_id = dataset_id,
                             galaxy_url = galaxy_url)
          })

#' S4 Method for single tool invocation
#' @rdname galaxy_run_tool
#' @export
setMethod("galaxy_run_tool", "Galaxy",
          function(x,
                   tool_id,
                   inputs     = NULL,
                   dataset_id = NULL
                   ) {
            job_id <- .galaxy_run_tool(tool_id   = tool_id,
                                       history_id = x@history_id,
                                       inputs     = if (is.null(inputs)) x@inputs else inputs,
                                       dataset_id = if (is.null(dataset_id)) x@input_dataset_id else dataset_id,
                                       galaxy_url = x@galaxy_url)
            x@invocation_id <- job_id
            validObject(x)
            x
          })

#' Helper function for tool polling
#' @keywords internal
#' @noRd
.galaxy_poll_tool <- function(invocation_id,
                              galaxy_url   = "https://usegalaxy.eu",
                              poll_interval = 3,
                              timeout       = 600) {
  galaxy_url <- .resolve_galaxy_url(galaxy_url)

  if (missing(invocation_id) || !nzchar(invocation_id)) {
    stop("invocation_id is required.")
  }

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  start_time <- Sys.time()

  repeat {

    res <- httr::GET(
      url = paste0(galaxy_url, "/api/jobs/", invocation_id),
      httr::add_headers(`x-api-key` = api_key)
    )
    httr::stop_for_status(res)

    job <- httr::content(res, as = "parsed")

    state <- job$state

    if (state == "ok") {
      return(job)
    }

    if (state %in% c("error", "deleted")) {
      stop(
        "Galaxy job failed (state = '", state, "').\n",
        if (!is.null(job$stderr)) job$stderr else ""
      )
    }

    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > timeout) {
      stop("Timed out waiting for Galaxy job to finish.")
    }

    Sys.sleep(poll_interval)
  }
}

#' Generic for galaxy_poll_tool
#' @rdname galaxy_poll_tool
#' @export
setGeneric("galaxy_poll_tool",
           function(x,
                    galaxy_url    = "https://usegalaxy.eu",
                    poll_interval = 3,
                    timeout       = 600)
             standardGeneric("galaxy_poll_tool"),
           signature = "x")

#' Wait for a Galaxy job to complete
#'
#' @param x A job ID (`character`) or a `Galaxy` object.
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method.
#' @param poll_interval Seconds between status checks.
#' @param timeout Maximum time to wait in seconds.
#' @return For the character method, the final job object; for the `Galaxy`
#'   method, the modified `Galaxy` object.
#' @rdname galaxy_poll_tool
#' @export
setMethod("galaxy_poll_tool", "character",
          function(x,
                   galaxy_url    = "https://usegalaxy.eu",
                   poll_interval = 3,
                   timeout       = 600) {
            .galaxy_poll_tool(invocation_id = x,
                              galaxy_url    = galaxy_url,
                              poll_interval = poll_interval,
                              timeout       = timeout)
          })

#' S4 method to poll the status of a tool invocation
#' @rdname galaxy_poll_tool
#' @export
setMethod("galaxy_poll_tool", "Galaxy",
          function(x,
                   poll_interval = 3,
                   timeout       = 600) {
            job <- .galaxy_poll_tool(invocation_id = x@invocation_id,
                                     galaxy_url    = x@galaxy_url,
                                     poll_interval = poll_interval,
                                     timeout       = timeout)
            if (!is.null(job$outputs)) {
              out_ids <- vapply(job$outputs, function(o) o$id, character(1L))
              x@output_dataset_ids <- out_ids
            }
            x@state <- if (identical(job$state, "ok")) "success" else "error"
            validObject(x)
            x
          })


