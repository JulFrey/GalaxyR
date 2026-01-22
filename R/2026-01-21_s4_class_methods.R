##########################
## S4 class definition and create function
##########################


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
    return("galaxy_url must be set once the Galaxy object is initialised.")
  }

  TRUE
})

#' Create a Galaxy session object
#'
#' Constructor for a `Galaxy` S4 object used for pipe‑based workflows.
#'
#' @param history_name Character. Default name to give to a new history,
#'   stored in the object and used by `galaxy_initialize()` if you don’t
#'   override it.
#' @param galaxy_url Character. Base URL of the Galaxy instance. If the
#'   environment variable `GALAXY_URL` is set, it takes precedence.
#'
#' @return A `Galaxy` object in state `"new"`.
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
#' called with a [`Galaxy`] object it uses the object’s `history_name` and
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
           function(x, name = "R API request", galaxy_url = "https://usegalaxy.eu", ...)
             standardGeneric("galaxy_initialize"),
           signature = "x")

#' @rdname galaxy_initialize
#' @export
setMethod("galaxy_initialize", "missing",
          function(name, galaxy_url, ...) .galaxy_initialize(name, galaxy_url))


#' @rdname galaxy_initialize
#' @export
setMethod("galaxy_initialize", "Galaxy",
          function(x, ...) {
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
#' encoded dataset ID. When called with a [`Galaxy`] object it uses the
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
#' @export
setMethod("galaxy_upload_ftp", "missing",
          function(x,
                   input_file,
                   galaxy_ftp = "ftp.usegalaxy.eu",
                   galaxy_url = "https://usegalaxy.eu",
                   ...)
            .galaxy_upload_ftp(input_file = input_file, history_id = x, galaxy_ftp, galaxy_url))

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

## generic with dispatch on the first argument
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
#' ID. When called with a [`Galaxy`] object it uses the object's `history_id` and
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

#' S4 Method for galaxy ftp upload
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

# internal helper, not exported
#' @keywords internal
#' @noRd
.galaxy_start_workflow <- function(
    history_id,
    dataset_id,
    workflow_id,
    inputs    = NULL,
    galaxy_url = "https://usegalaxy.eu"
) {
  if (missing(workflow_id) || !nzchar(workflow_id)) {
    stop("workflow_id is required.")
  }
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }
  if (is.null(inputs)) {
    if (missing(dataset_id) || !nzchar(dataset_id)) {
      stop("Either dataset_id or inputs must be provided.")
    }
    inputs <- setNames(list(list(src = "hda", id = dataset_id)), "0")
  }
  run_body <- list(inputs = inputs)
  if (!is.na(history_id)) {
    run_body$history_id <- history_id
  }
  run_url <- paste0(galaxy_url, "/api/workflows/", workflow_id, "/invocations")
  res <- httr::POST(
    run_url,
    httr::add_headers(
      `x-api-key`    = api_key,
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(run_body, auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  inv <- httr::content(res, as = "parsed")
  message("Workflow invocation ID: ", inv$id)
  inv$id
}

## generic dispatches on the first argument
setGeneric("galaxy_start_workflow",
           function(x,
                    workflow_id,
                    dataset_id,
                    inputs     = NULL,
                    galaxy_url = "https://usegalaxy.eu",
                    ...)
             standardGeneric("galaxy_start_workflow"),
           signature = "x")

#' Start a Galaxy workflow with inputs and parameters
#'
#' `galaxy_start_workflow()` is an S4 generic. With `x` as a character vector
#' it is treated as a history ID: the given workflow is invoked in that history
#' and the invocation ID is returned. With `x` as a [`Galaxy`] object, the
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
#' @export
setMethod("galaxy_start_workflow", "character",
          function(x,
                   workflow_id,
                   dataset_id,
                   inputs     = NULL,
                   galaxy_url = "https://usegalaxy.eu",
                   ...) {
            .galaxy_start_workflow(history_id = x,
                                   dataset_id = dataset_id,
                                   workflow_id = workflow_id,
                                   inputs     = inputs,
                                   galaxy_url = galaxy_url)
          })

#' S4 function to start a galaxy workflow
#' @rdname galaxy_start_workflow
#' @export
setMethod("galaxy_start_workflow", "Galaxy",
          function(x,
                   workflow_id,
                   dataset_id,
                   inputs = NULL,
                   ...) {
            hid <- x@history_id
            ds  <- if (!missing(dataset_id)) dataset_id else x@input_dataset_id
            inp <- if (!is.null(inputs)) inputs else if (length(x@inputs) > 0) x@inputs else NULL
            inv <- .galaxy_start_workflow(history_id = hid,
                                          dataset_id = ds,
                                          workflow_id = workflow_id,
                                          inputs     = inp,
                                          galaxy_url = x@galaxy_url)
            x@invocation_id <- inv
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
#' [`Galaxy`] object, the `invocation_id` and `galaxy_url` are taken from the
#' object, and the object is updated with the resulting `output_dataset_ids` and
#' state.
#'
#' @param x A workflow invocation ID (`character`) or a `Galaxy` object.
#' @param galaxy_url Base URL of the Galaxy instance, used by the character
#'   method. If `GALAXY_URL` is set it takes precedence.
#' @param poll_interval Time in seconds between polling attempts.
#' @return For the character method, a list with elements `success` and
#'   `output_ids`. For the `Galaxy` method, the modified `Galaxy` object.
#' @examplesIf galaxy_has_key()
#' invocation_id <- "abc123"
#' galaxy_poll_workflow(invocation_id)
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
