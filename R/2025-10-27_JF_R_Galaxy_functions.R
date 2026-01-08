## functions to communicate with the galaxy api
## written by Julian Frey
## 2025-10-27

#' Start a session with the GALAXY API
#' @param galaxy_url Your Galaxy API key
#' @param name Name of the history to create
#' @return history_id The ID of the created history
#' @examples
#' \dontrun{
#' # set up your API Key in your .Renviron file first
#' galaxy_url <- "https://usegalaxy.eu"
#' api_key <- Sys.getenv("GALAXY_API_KEY")
#' history_id <- galaxy_initialize(api_key)
#' }
#' @export galaxy_initialize
galaxy_initialize <- function(name = "R API request", galaxy_url = "https://usegalaxy.eu") {
  api_key <- Sys.getenv("GALAXY_API_KEY")
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


#' Upload a file to Galaxy via FTP and register it in a history
#'
#' @param input_file Path to the local file to upload
#' @param galaxy_url Base URL of the Galaxy instance
#' @param galaxy_ftp FTP server address of the Galaxy instance
#' @param history_id The ID of the Galaxy history where the dataset will be uploaded
#'
#' @returns dataset_id The ID of the uploaded dataset in Galaxy
#' @export galaxy_upload
#'
#' @examples
#' \dontrun{
#' # set up your API Key, username and password in your .Renviron file first
#' galaxy_url <- "https://usegalaxy.eu"
#' galaxy_ftp <- "ftp.usegalaxy.eu"
#' input_file <- "path/to/your/file.txt"
#' dataset_id <- galaxy_upload(input_file, galaxy_url, galaxy_ftp)
#' print(dataset_id)
#' }
galaxy_upload <- function(input_file, history_id, galaxy_url = "https://usegalaxy.eu", galaxy_ftp = "ftp.usegalaxy.eu"){
  api_key <- Sys.getenv("GALAXY_API_KEY")
  username <- Sys.getenv("GALAXY_USERNAME")
  password <- Sys.getenv("GALAXY_PASSWORD")

  username_enc <- utils::URLencode(username, reserved = TRUE)
  password_enc <- utils::URLencode(password, reserved = TRUE)
  ftp_url <- paste0("ftp://", username_enc, ":", password_enc, "@", galaxy_ftp, "/")

  # UPLOAD using ftp
  system2(
    "curl",
    c(
      "--ssl-reqd", "-T", shQuote(input_file),
      ftp_url
    ),
    stdout = TRUE, stderr = TRUE
  )

  # fetch the dataset using API
  ftp_filename <- basename(input_file)

  fetch_payload <- list(
    history_id = history_id,
    targets = list(list(
      destination = list(type = "hdas"),
      elements = list(list(
        src = "ftp_import",
        ftp_path = ftp_filename,
        ext = "auto",
        dbkey = "?"
      ))
    ))
  )

  res <- httr::POST(
    paste0(galaxy_url, "/api/tools/fetch"),
    httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"),
    body = jsonlite::toJSON(fetch_payload, auto_unbox = TRUE)
  )

  httr::stop_for_status(res)
  upload_result <- httr::content(res, "parsed")
  #print(upload_result)

  dataset_id <- upload_result$outputs[[1]]$id
  return(dataset_id)
}

#' Start a Galaxy workflow with a given dataset as input
#'
#' @param dataset_id The ID of the input dataset in Galaxy
#' @param workflow_id The ID of the workflow to run
#' @param galaxy_url Base URL of the Galaxy instance
#' @param history_id The ID of the history where the workflow will run
#'
#' @returns invocation_id The ID of the started workflow invocation
#' @export galaxy_start_workflow
#' @importFrom stats setNames
galaxy_start_workflow <- function(dataset_id, workflow_id, history_id = NA, galaxy_url = "https://usegalaxy.eu"){
  api_key <- Sys.getenv("GALAXY_API_KEY")
  run_url <- paste0(galaxy_url, "/api/workflows/", workflow_id, "/invocations")
  run_body <- list(
    inputs = setNames(list(list(src = "hda", id = dataset_id)), "0")  # map input 0
  )

  # include history id in payload so the workflow runs into the specified history
  if(!is.na(history_id)){
    run_body$history_id <- history_id
  }

  run_res <- httr::POST(
    run_url,
    httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"),
    body = jsonlite::toJSON(run_body, auto_unbox = TRUE)
  )

  httr::stop_for_status(run_res)
  invocation <- httr::content(run_res, "parsed")
  invocation_id <- invocation$id
  message("Workflow invocation ID:", invocation_id, "\n")
  return(invocation_id)
}

#' Poll a Galaxy workflow invocation until completion
#'
#' @param invocation_id The ID of the workflow invocation to poll
#' @param galaxy_url Base URL of the Galaxy instance
#' @param poll_interval Time in seconds between polling attempts in seconds
#'
#' @returns A vector of HDA IDs corresponding to the output datasets of the workflow
#' @export galaxy_poll_workflow
galaxy_poll_workflow <- function(invocation_id, galaxy_url = "https://usegalaxy.eu", poll_interval = 30) {
  api_key <- Sys.getenv("GALAXY_API_KEY")
  any_error <- FALSE
  repeat {
    Sys.sleep(poll_interval)

    # Get workflow invocation
    status_res <- httr::GET(
      paste0(galaxy_url, "/api/invocations/", invocation_id),
      httr::add_headers(`x-api-key` = api_key)
    )
    httr::stop_for_status(status_res)
    status <- httr::content(status_res, "parsed")

    steps <- status$steps

    # Get all job IDs from the steps
    job_ids <- sapply(steps, function(step) step$job_id)
    job_ids <- job_ids[!sapply(job_ids, is.null)]

    if (length(job_ids) == 0) {
      message(Sys.time(), " ,No jobs yet, waiting...")
      next
    }

    # Check each job state
    job_states <- sapply(job_ids, function(jid) {
      job_res <- httr::GET(
        paste0(galaxy_url, "/api/jobs/", jid),
        httr::add_headers(`x-api-key` = api_key)
      )
      job <- httr::content(job_res, "parsed")
      job$state
    })

    message(Sys.time(), " ,Job states: ", paste(job_states, collapse = ", "))

    if (all(job_states == "ok")) {
      message("All jobs finished successfully!")
      break
    }
    if (any(job_states == "error" | job_states == "failed" | job_states == "deleted")) {
      any_error <- TRUE
      message("Some workflow jobs failed or were cancelled.")
      break
    }
  }

  # Once all jobs are ok, return the HDA IDs in the workflow history
  history_id <- status$history_id
  datasets_res <- httr::GET(
    paste0(galaxy_url, "/api/histories/", history_id, "/contents"),
    httr::add_headers(`x-api-key` = api_key)
  )
  datasets <- httr::content(datasets_res, "parsed")

  output_ids <- sapply(datasets, function(d) if(d$state == "ok" && !isTRUE(d$deleted)) d$id else NULL)
  output_ids <- output_ids[!sapply(output_ids, is.null)]
  output <- list(success = !any_error, output_ids = output_ids)

  return(output)
}

#' Download final result dataset from Galaxy
#'
#' @param output_ids Vector of HDA IDs from the workflow outputs the last one will be downloaded
#' @param out_file Path to save the downloaded file
#' @param galaxy_url Base URL of the Galaxy instance
#'
#' @returns The response object from the download request for debugging
#' @export galaxy_download_result
galaxy_download_result <- function(output_ids, out_file = "result.laz", galaxy_url = "https://usegalaxy.eu" ){
  if(!is.null(output_ids$output_ids)){
    output_ids <- output_ids$output_ids
  }

  api_key <- Sys.getenv("GALAXY_API_KEY")
  download_res <- httr::GET(
    paste0(galaxy_url, "/api/datasets/", output_ids[length(output_ids)], "/display"),
    httr::add_headers(`x-api-key` = api_key),
    httr::write_disk(out_file, overwrite = TRUE)
  )
  return(download_res)
}

# Helper to trim trailing slash
.rtrim <- function(x, char = "/") {
  sub(paste0(char, "+$"), "", x)
}

#' Delete a Galaxy dataset by ID
#'
#' Delete a dataset (HDA) from a Galaxy instance using the Galaxy API.
#'
#' This function performs an HTTP DELETE against the Galaxy
#' /api/datasets/<id> endpoint. By default it requests a purge
#' (permanent removal) by adding ?purge=true. The Galaxy API key is
#' read from the environment variable \code{GALAXY_API_KEY}.
#'
#' @param dataset_id Character. The Galaxy dataset ID to delete.
#' @param purge Logical. If \code{TRUE} the API call will include
#'   \code{purge=true} to permanently remove the dataset and free
#'   space. If \code{FALSE} the dataset may be only soft-deleted
#'   depending on Galaxy configuration. Default: \code{TRUE}.
#' @param verbose Logical. If \code{TRUE} a message with the HTTP
#'   status code will be printed. Default: \code{TRUE}.
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   including scheme (for example \code{"https://usegalaxy.eu"}).
#'   Default: \code{"https://usegalaxy.eu"}.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{success}{Logical. \code{TRUE} for 2xx responses, otherwise \code{FALSE}.}
#'     \item{status}{Integer. HTTP status code returned by the API.}
#'     \item{content}{Character. The raw response body (text).}
#'   }
#'
#' @details
#' - Make sure \code{Sys.getenv("GALAXY_API_KEY")} is set to a valid API key..
#' - Use caution when running with \code{purge = TRUE} as this permanently
#'   removes data.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(GALAXY_API_KEY = "MY_KEY")
#' galaxy_delete_dataset("abcdef12-3456-7890-abcd-ef1234567890")
#' }
#'
#' @export galaxy_delete_dataset
#' @importFrom httr VERB add_headers content status_code
galaxy_delete_dataset <- function(dataset_id, purge = TRUE, verbose = FALSE, galaxy_url = "https://usegalaxy.eu") {
  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (identical(api_key, "")) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  url <- sprintf("%s/api/datasets/%s", .rtrim(galaxy_url, "/"), dataset_id)
  if (purge) url <- paste0(url, "?purge=true")
  resp <- httr::VERB("DELETE", url, httr::add_headers(`x-api-key` = api_key))
  if (verbose) {
    message(sprintf("DELETE %s -> %s", url, httr::status_code(resp)))
  }
  status <- httr::status_code(resp)
  content_text <- httr::content(resp, "text", encoding = "UTF-8")
  if (status >= 200 && status < 300) {
    return(list(success = TRUE, status = status, content = content_text))
  } else {
    return(list(success = FALSE, status = status, content = content_text))
  }
}

#' Delete multiple Galaxy datasets by ID
#'
#' Convenience wrapper that deletes a vector of dataset IDs using
#' \code{galaxy_delete_dataset}. Requests are paced with a small
#' sleep between calls to avoid overwhelming the server.
#'
#' @param output_ids Character vector of dataset IDs to delete.
#' @param purge Logical. Passed to \code{galaxy_delete_dataset}. Default: \code{TRUE}.
#' @param sleep Numeric. Seconds to wait between API calls. Default: \code{0.2}.
#' @param galaxy_url Character. Base URL of the Galaxy instance.
#'
#' @return A named list where each element is the return value from
#'   \code{galaxy_delete_dataset} for the corresponding dataset ID.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(GALAXY_API_KEY = "MY_KEY")
#' ids <- c("id1", "id2", "id3")
#' res <- galaxy_delete_datasets(ids, purge = TRUE)
#' }
#'
#' @export galaxy_delete_datasets
galaxy_delete_datasets <- function(output_ids, purge = TRUE, sleep = 0.2, galaxy_url = "https://usegalaxy.eu") {
  if(!is.null(output_ids$output_ids)){
    output_ids <- output_ids$output_ids
  }
  if(is.list(output_ids)) output_ids <- unlist(output_ids)
  if (!is.character(output_ids)) {
    stop("output_ids must be a character vector of dataset IDs.")
  }
  results <- list()
  for (id in output_ids) {
    Sys.sleep(sleep)  # gentle pacing
    results[[id]] <- galaxy_delete_dataset(id, purge = purge, verbose = TRUE, galaxy_url = galaxy_url)
  }
  return(results)
}

#' Trim trailing characters
#'
#' Internal helper to remove trailing characters (defaults to "/")
#' from a string. Not exported.
#'
#' @param x Character vector of length 1.
#' @param char Character. The character to trim from the end. Default "/".
#' @return Character string with trailing characters removed.
#' @keywords internal
.rtrim <- function(x, char = "/") {
  sub(paste0(char, "+$"), "", x)
}

#' List Galaxy histories (name and history id)
#'
#' @param galaxy_url Base URL of the Galaxy instance, e.g. "https://usegalaxy.eu"
#' @return data.frame with columns: name, history_id
#' @export
galaxy_list_histories <- function(galaxy_url = "https://usegalaxy.eu") {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package required but not installed")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required but not installed")
  }

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (identical(api_key, "") || is.na(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set")
  }

  base_url <- file.path(galaxy_url, "api", "histories")
  limit <- 500L
  offset <- 0L
  all_items <- list()

  repeat {
    res <- httr::GET(
      url = base_url,
      httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"),
      query = list(limit = limit, offset = offset)
    )
    httr::stop_for_status(res)
    items <- httr::content(res, as = "parsed", simplifyVector = TRUE)

    if (length(items) == 0) break
    all_items <- c(all_items, items)

    if (length(items) < limit) break
    offset <- offset + limit
  }

  if (length(all_items) == 0) {
    return(data.frame(name = character(0), history_id = character(0), stringsAsFactors = FALSE))
  }

  # Extract name and id (history id)
  df <- data.frame(history_name = all_items$name, history_id = all_items$id)

  # Remove possible duplicate rows and return
  df <- unique(df)
  return(df)
}

#' List workflow invocations for a given workflow
#'
#' @param workflow_id The Galaxy workflow id to list invocations for
#' @param galaxy_url Base URL of the Galaxy instance, e.g. "https://usegalaxy.eu"
#' @return data.frame with columns: invocation_id, workflow_id, history_id, state, create_time, update_time
#' @export
galaxy_list_invocations <- function(workflow_id, galaxy_url = "https://usegalaxy.eu") {
  if (missing(workflow_id) || identical(workflow_id, "") ) {
    stop("workflow_id is required")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package required but not installed")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required but not installed")
  }

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (identical(api_key, "") || is.na(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set")
  }

  # Use the workflow-specific endpoint
  base_url <- file.path(galaxy_url, "api", "workflows", workflow_id, "invocations")
  limit <- 50L
  offset <- 0L
  all_items <- list()

  repeat {
    res <- httr::GET(
      url = base_url,
      httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"),
      query = list(limit = limit, offset = offset)
    )
    httr::stop_for_status(res)
    items <- httr::content(res, as = "parsed", simplifyVector = TRUE)

    if (nrow(items) == 0) break
    if(length(all_items) == 0) {
      all_items <- data.frame(items)
    } else {
      all_items <- rbind(all_items, data.frame(items))
    }

    if (nrow(items) < limit) break
    offset <- offset + limit
  }

  # If nothing found return empty df with consistent columns
  empty_df <- data.frame(
    invocation_id = character(0),
    workflow_id = character(0),
    history_id = character(0),
    state = character(0),
    create_time = character(0),
    update_time = character(0),
    stringsAsFactors = FALSE
  )
  if (length(all_items) == 0) return(empty_df)

  # Normalize fields (different Galaxy versions may use slightly different field names)
  df <- as.data.frame(all_items)
  #df <- unique(df)
  return(df)
}

#' Get the disk usage / size of a Galaxy history
#'
#' The function first tries to read a size/disk_usage field from the history
#' summary endpoint. If that is not present it fetches the history contents
#' and sums dataset sizes (robust to a few different field names used by
#' different Galaxy versions). Results are returned as a data.frame with
#' bytes and a human-readable size.
#'
#' @param history_id Galaxy history id (required)
#' @param galaxy_url Base URL of the Galaxy instance (default: "https://usegalaxy.eu")
#' @param include_deleted Logical; whether to include deleted datasets when summing (default FALSE)
#' @return data.frame with columns history_id, bytes, human_size
#' @export
galaxy_history_size <- function(history_id,
                                galaxy_url = "https://usegalaxy.eu",
                                include_deleted = FALSE) {
  if (missing(history_id) || identical(history_id, "")) stop("history_id is required")
  if (!requireNamespace("httr", quietly = TRUE)) stop("httr package required but not installed")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite package required but not installed")

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (identical(api_key, "") || is.na(api_key)) stop("GALAXY_API_KEY environment variable is not set")

  # helper: coalesce values
  coalesce <- function(...) {
    for (v in list(...)) {
      if (!is.null(v)) return(v)
    }
    NULL
  }

  # human readable bytes
  human_bytes <- function(bytes) {
    if (is.na(bytes) || length(bytes) == 0) return(NA_character_)
    b <- as.numeric(bytes)
    if (is.na(b)) return(NA_character_)
    units <- c("B", "KB", "MB", "GB", "TB")
    if (b == 0) return("0 B")
    idx <- floor(log(b, 1024))
    idx <- pmin(idx, length(units) - 1)
    sprintf("%.2f %s", b / (1024 ^ idx), units[idx + 1])
  }


  # 1) Try summary/history endpoint for any disk/size fields
  history_url <- paste0(galaxy_url, "/api/histories/", history_id)
  res <- httr::GET(history_url, httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"))
  httr::stop_for_status(res)
  hist <- httr::content(res, as = "parsed", simplifyVector = TRUE)

  # possible fields used by different Galaxy versions
  possible_history_fields <- c("disk_usage", "size", "total_size", "total_disk_usage", "usage")
  found <- NULL
  for (f in possible_history_fields) {
    if (!is.null(hist[[f]])) { found <- hist[[f]]; break }
  }

  if (!is.null(found)) {
    bytes <- as.numeric(found)
    return(data.frame(history_id = as.character(history_id),
                      bytes = bytes,
                      human_size = human_bytes(bytes),
                      stringsAsFactors = FALSE))
  }

  # 2) Fallback: list history contents and sum per-dataset size fields (paginated)
  contents_url <- paste0(galaxy_url, "/api/histories/", history_id, "/contents")
  limit <- 500L
  offset <- 0L
  total_bytes <- 0
  repeat {
    q <- list(limit = limit, offset = offset, deleted = if (isTRUE(include_deleted)) "True" else "False")
    res2 <- httr::GET(contents_url, httr::add_headers(`x-api-key` = api_key, `Content-Type` = "application/json"), query = q)
    httr::stop_for_status(res2)
    items <- httr::content(res2, as = "parsed", simplifyVector = TRUE)
    if (length(items) == 0) break

    # each item typically has file_size or size or disk_usage; be robust
    sizes <- vapply(items, FUN.VALUE = numeric(1), USE.NAMES = FALSE, FUN = function(it) {
      # some items may be lists; get numeric size candidate
      vals <- c(it[["file_size"]], it[["file_size_bytes"]], it[["size"]], it[["disk_usage"]], it[["file_size_uncompressed"]])
      # also try nested extras if present
      if (is.null(vals) || all(sapply(vals, is.null))) {
        if (!is.null(it$extra) && is.list(it$extra)) {
          vals <- c(vals, it$extra[["file_size"]], it$extra[["size"]], it$extra[["disk_usage"]])
        }
      }
      # coalesce first non-null, numeric
      for (v in vals) {
        if (!is.null(v) && !is.na(v) && v != "") {
          nv <- suppressWarnings(as.numeric(v))
          if (!is.na(nv)) return(nv)
        }
      }
      0
    })

    total_bytes <- total_bytes + sum(as.numeric(sizes), na.rm = TRUE)

    if (length(items) < limit) break
    offset <- offset + limit
  }

  data.frame(history_id = as.character(history_id),
             bytes = as.numeric(total_bytes),
             human_size = human_bytes(total_bytes),
             stringsAsFactors = FALSE)
}


#' Set the Galaxy API key for the current R session
#'
#' @param api_key Character. The API key to be used for authenticating
#'   subsequent Galaxy API calls.
#' @param overwrite Logical. Whether to overwrite an existing value in
#'   \code{Sys.getenv("GALAXY_API_KEY")}. Default: \code{TRUE}.
#'
#' @return Invisibly returns the API key that was set.
#'
#' @details
#' This helper is intended for interactive sessions. It simply calls
#' \code{Sys.setenv(GALAXY_API_KEY = api_key)}.
#'
#' @examples
#' \dontrun{
#' galaxy_set_api_key("your-secret-key")
#' }
#'
#' @export
galaxy_set_api_key <- function(api_key, overwrite = TRUE) {
  if (missing(api_key) || !nzchar(api_key)) {
    stop("api_key must be a non-empty string.")
  }
  existing <- Sys.getenv("GALAXY_API_KEY", unset = NA_character_)
  if (!isTRUE(overwrite) && !is.na(existing) && nzchar(existing)) {
    stop("GALAXY_API_KEY already set; use overwrite = TRUE to replace.")
  }
  Sys.setenv(GALAXY_API_KEY = api_key)
  invisible(api_key)
}

#' List tools installed on a Galaxy instance
#'
#' @param galaxy_url Character. Base URL of the Galaxy instance.
#'   Default: \code{"https://usegalaxy.eu"}.
#' @param in_panel Logical. If \code{TRUE}, return the tool panel
#'   structure (sections/categories). If \code{FALSE}, return the flat
#'   list of all tools as supplied by Galaxy. Default: \code{FALSE}.
#' @param panel_id Optional character. When supplied, only tools from the
#'   matching panel (section/category) are returned. The value is matched
#'   against both the panel \code{id} and \code{name}. Supplying
#'   \code{panel_id} automatically requests the panelized structure,
#'   regardless of the value of \code{in_panel}.
#'
#' @return A list corresponding to the parsed JSON returned by Galaxy.
#'   If \code{panel_id} is provided, a list of tool entries belonging to
#'   the requested panel is returned (each entry is the raw tool metadata
#'   as provided by Galaxy).
#'
#' @examples
#' \dontrun{
#' # All tools (flat list)
#' galaxy_list_tools()
#'
#' # Panel structure
#' galaxy_list_tools(in_panel = TRUE)
#'
#' # Tools from a specific panel (match by id or name)
#' galaxy_list_tools(panel_id = "get-data")
#' galaxy_list_tools(panel_id = "Get Data")
#' }
#'
#' @export
galaxy_list_tools <- function(galaxy_url = "https://usegalaxy.eu",
                              in_panel = FALSE,
                              panel_id = NULL) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required.")
  }

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  request_panel <- isTRUE(in_panel) || !is.null(panel_id)

  res <- httr::GET(
    url = paste0(galaxy_url, "/api/tools"),
    httr::add_headers(`x-api-key` = api_key),
    query = list(in_panel = if (request_panel) "true" else "false")
  )
  httr::stop_for_status(res)
  content <- httr::content(res, as = "parsed", simplifyVector = FALSE)

  if (is.null(panel_id)) {
    return(content)
  }

  find_panel <- function(items) {
    for (item in items) {
      if (!is.list(item)) next
      # match by id or name
      if (!is.null(item$id) && identical(item$id, panel_id)) return(item)
      if (!is.null(item$name) && identical(item$name, panel_id)) return(item)
      # look into nested containers
      for (child_field in c("items", "elems", "sections", "children")) {
        child <- item[[child_field]]
        if (is.list(child)) {
          found <- find_panel(child)
          if (!is.null(found)) return(found)
        }
      }
    }
    NULL
  }

  panel <- find_panel(content)
  if (is.null(panel)) {
    stop("Panel '", panel_id, "' not found in the tool panel structure.")
  }

  collect_tools <- function(node) {
    collected <- list()
    recurse <- function(x) {
      if (!is.list(x)) return()
      is_tool <- (!is.null(x$type) && identical(x$type, "tool")) ||
        (!is.null(x$model_class) && grepl("tool", x$model_class, ignore.case = TRUE))
      if (is_tool) {
        collected <<- c(collected, list(x))
      }
      for (child_field in c("items", "elems", "sections", "children")) {
        child <- x[[child_field]]
        if (is.list(child)) lapply(child, recurse)
      }
    }
    recurse(node)
    collected
  }

  tools <- collect_tools(panel)
  if (!length(tools)) {
    warning("Panel found but no tool entries were detected.")
  }
  tools
}

# Helper for coalescing values
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Retrieve detailed metadata for a Galaxy tool
#'
#' @param tool_id Character. The Galaxy tool identifier (for example
#'   \code{"toolshed.g2.bx.psu.edu/repos/devteam/fastqc/fastqc/0.73"}).
#' @param galaxy_url Character. Base URL of the Galaxy instance.
#'   Default: \code{"https://usegalaxy.eu"}.
#' @param tool_version Optional character string to request a specific
#'   version. If \code{NULL}, Galaxy will return the default/latest
#'   version metadata.
#'
#' @return A list containing the tool metadata as returned by the Galaxy
#'   API (inputs, outputs, help text, etc.).
#'
#' @examples
#' \dontrun{
#' fastqc_tool <- galaxy_get_tool("toolshed.g2.bx.psu.edu/repos/devteam/fastqc/fastqc/0.73")
#' names(fastqc_tool$inputs)
#' }
#'
#' @export
galaxy_get_tool <- function(tool_id,
                            galaxy_url = "https://usegalaxy.eu",
                            tool_version = NULL) {
  if (missing(tool_id) || !nzchar(tool_id)) {
    stop("tool_id is required.")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required.")
  }
  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  url <- paste(galaxy_url, "api", "tools", tool_id, sep = "/")
  res <- httr::GET(
    url = url,
    httr::add_headers(`x-api-key` = api_key),
    query = list(tool_version = tool_version, io_details = 'true')
  )
  httr::stop_for_status(res)
  httr::content(res, as = "parsed", simplifyVector = FALSE)
}

#' Retrieve Galaxy tool IDs by name
#'
#' @param name Character string to search for in tool names.
#' @param tools Optional list as returned by \code{galaxy_list_tools}.
#'   If \code{NULL}, the function will fetch tools on the fly by calling
#'   \code{galaxy_list_tools}.
#' @param ignore_case Logical. Whether matching should ignore case.
#'   Default: \code{TRUE}.
#' @param galaxy_url Character. Base URL of the Galaxy instance.
#'   Ignored when \code{tools} is supplied. Default: \code{"https://usegalaxy.eu"}.
#' @param panel_id Optional character. Passed through to
#'   \code{galaxy_list_tools} when \code{tools} is \code{NULL} so you can
#'   restrict the search to a panel/section.
#'
#' @return Character vector of matching tool IDs. Returns \code{character(0)}
#'   if no tools match.
#'
#' @examples
#' \dontrun{
#' galaxy_set_api_key("YOUR_KEY")
#'
#' # Fetch the full tool list once, then lookup
#' tools <- galaxy_list_tools()
#' galaxy_get_tool_id("FastQC", tools = tools)
#'
#' # Or let the helper fetch on demand
#' galaxy_get_tool_id("FastQC")
#'
#' # Exact, case-sensitive match inside a specific panel
#' galaxy_get_tool_id("Concatenate datasets", exact = TRUE,
#'                    ignore_case = FALSE, panel_id = "textutil")
#' }
#'
#' @export
galaxy_get_tool_id <- function(name,
                               tools = NULL,
                               ignore_case = TRUE,
                               galaxy_url = "https://usegalaxy.eu",
                               panel_id = NULL) {
  if (missing(name) || !nzchar(name)) {
    stop("Argument 'name' must be a non-empty string.")
  }

  if (is.null(tools)) {
    tools <- galaxy_list_tools(galaxy_url = galaxy_url, panel_id = panel_id)
  }

  if (!is.list(tools)) {
    stop("'tools' must be a list as returned by galaxy_list_tools().")
  }

  tool_entries <- data.frame(t(sapply(tools, function(x) c(x$model_class, x$id, x$name))))
  tool_entries <- tool_entries[tool_entries[,1] == "Tool",]

  if (!length(tool_entries)) {
    warning("No tool entries detected in the provided 'tools' object.")
    return(character(0))
  }

  matches <- grep(name, tool_entries[,3], ignore.case = ignore_case)
  return(tool_entries[matches,2])
}

#' Run a Galaxy tool programmatically
#'
#' @param tool_id Character. Tool identifier to execute.
#' @param history_id Character. History ID where outputs will be stored.
#' @param inputs Named list describing tool inputs exactly as required
#'   by the Galaxy tool. The list will be JSON-encoded automatically.
#' @param galaxy_url Character. Base Galaxy URL. Default:
#'   \code{"https://usegalaxy.eu"}.
#'
#' @return The job_id for the invocation.
#'
#' @details
#' This sends a POST request to \code{/api/tools} with the required
#' payload. You can check the structure expected for \code{inputs} by
#' inspecting \code{galaxy_get_tool()} or by looking at Galaxy's
#' "Paste Request" feature in the web UI.
#'
#' @examples
#' \dontrun{
#' res <- galaxy_run_tool(
#'   tool_id = "upload1",
#'   history_id = "your-history-id",
#'   inputs = list(dbkey = "?", file_type = "auto")
#' )
#' }
#'
#' @export
galaxy_run_tool <- function(tool_id,
                            history_id,
                            inputs,
                            galaxy_url = "https://usegalaxy.eu") {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required.")
  }
  if (missing(tool_id) || !nzchar(tool_id)) stop("tool_id is required.")
  if (missing(history_id) || !nzchar(history_id)) stop("history_id is required.")
  if (missing(inputs) || !is.list(inputs)) stop("inputs must be a named list.")

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) stop("GALAXY_API_KEY environment variable is not set.")

  payload <- list(
    history_id = history_id,
    tool_id = tool_id,
    inputs = inputs
  )

  res <- httr::POST(
    url = paste0(galaxy_url, "/api/tools"),
    httr::add_headers(
      `x-api-key` = api_key,
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  job <- httr::content(res, as = "parsed", simplifyVector = FALSE)
  return(job$jobs[[1]]$id)
}

#' Upload a dataset via HTTPS (direct POST) into Galaxy
#'
#' @param input_file Character. Path to the local file to upload.
#' @param history_id Character. ID of the Galaxy history to receive the dataset.
#' @param galaxy_url Character. Base URL of the Galaxy instance.
#'   Default: \code{"https://usegalaxy.eu"}.
#' @param file_type Character. Galaxy datatype identifier
#'   (for example \code{"auto"}, \code{"fastq"}, \code{"bam"}). Default: \code{"auto"}.
#' @param dbkey Character. Reference genome identifier (for example \code{"?"} or \code{"hg38"}). Default: \code{"?"}.
#'
#' @return A list describing the newly created dataset(s) as returned by Galaxy.
#'
#' @details
#' This function uses the built-in \code{upload1} tool and performs a
#' multipart form POST. Large files may still require FTP depending on
#' the Galaxy server's configuration limits.
#'
#' @examples
#' \dontrun{
#' galaxy_upload_https("reads.fastq.gz", history_id = "abc123")
#' }
#'
#' @export
galaxy_upload_https <- function(
    input_file,
    history_id,
    galaxy_url = "https://usegalaxy.eu",
    file_type = "auto",
    dbkey = "?"
) {

  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required.")
  }

  if (!file.exists(input_file))
    stop("input_file does not exist: ", input_file)

  if (missing(history_id) || !nzchar(history_id))
    stop("history_id is required.")

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key))
    stop("GALAXY_API_KEY environment variable is not set.")

  galaxy_wait_for_dataset <- function(
    dataset_id,
    galaxy_url = "https://usegalaxy.eu",
    poll_interval = 3,
    timeout = 600
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

  ## Extract encoded dataset ID
  dataset_id <- response$outputs[[1]]$id

  ## Wait until Galaxy finishes processing it
  dataset <- galaxy_wait_for_dataset(
    dataset_id = dataset_id,
    galaxy_url = galaxy_url
  )

  ## Return completed dataset (or dataset$id)
  return(dataset)
}


#' Wait for a Galaxy job to complete
#'
#' Polls the Galaxy API until a job reaches a terminal state
#' (`ok`, `error`, or `deleted`).
#'
#' @param job_id Character scalar. Encoded Galaxy job ID.
#' @param galaxy_url Character scalar. Base Galaxy URL.
#'   Defaults to `"https://usegalaxy.eu"`.
#' @param poll_interval Numeric. Seconds to wait between status checks.
#'   Defaults to 3.
#' @param timeout Numeric. Maximum time to wait in seconds.
#'   Defaults to 600 (10 minutes).
#'
#' @return A named list containing the final Galaxy job object.
#'
#' @details
#' Galaxy jobs are executed asynchronously. This function polls
#' `/api/jobs/{job_id}` until the job state becomes `"ok"`.
#'
#' If the job enters state `"error"` or `"deleted"`, an error is raised.
#' If the timeout is exceeded, the function stops with an error.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{galaxy_run_tool}} for submitting tools
#' }
#'
#' @examples
#' \dontrun{
#' job <- galaxy_run_tool(
#'   tool_id = tool,
#'   history_id = history,
#'   inputs = list(
#'     text_input = "added text",
#'     infile = list(src = "hda", id = file_id$id)
#'   )
#' )
#'
#' final_job <- galaxy_wait_for_job(job$jobs[[1]]$id)
#' }
#'
#' @export
galaxy_wait_for_job <- function(
    job_id,
    galaxy_url = "https://usegalaxy.eu",
    poll_interval = 3,
    timeout = 600
) {

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required.")
  }

  if (missing(job_id) || !nzchar(job_id)) {
    stop("job_id is required.")
  }

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  start_time <- Sys.time()

  repeat {

    res <- httr::GET(
      url = paste0(galaxy_url, "/api/jobs/", job_id),
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
