## functions to communicate with the galaxy api
## written by Julian Frey
## 2025-10-27

#' Check whether a Galaxy API key is available
#'
#' Check whether the environment variable
#' \code{GALAXY_API_KEY} is set and non-empty.
#'
#' @return Logical. \code{TRUE} if an API key is available, otherwise \code{FALSE}.
#'
#' @examples
#' galaxy_has_key() # returns true if api key is set
#'
#' @export galaxy_has_key
galaxy_has_key <- function() {
  api_key <- Sys.getenv("GALAXY_API_KEY", unset = "")
  return(api_key != "")
}

#' Resolve the Galaxy base URL
#'
#' Internal helper that resolves the Galaxy base URL. If the environment
#' variable \code{GALAXY_URL} is set, it takes precedence over the value
#' supplied via the \code{galaxy_url} argument.
#'
#' @param galaxy_url Character. Default Galaxy base URL to use if
#'   \code{GALAXY_URL} is not set.
#'
#' @return Character. The resolved Galaxy base URL.
#'
#' @keywords internal
.resolve_galaxy_url <- function(galaxy_url) {
  env_url <- Sys.getenv("GALAXY_URL", unset = "")
  if (nzchar(env_url)) {
    return(env_url)
  }
  galaxy_url
}

#' List workflows available to the user
#'
#' Retrieves workflows accessible to the authenticated user from a Galaxy
#' instance. Optionally includes public (published) workflows if supported
#' by the Galaxy server.
#'
#' @param include_public Logical. If \code{TRUE}, attempt to also include
#'   published public workflows. Default: \code{FALSE}.
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#'
#' @return
#' A data.frame with one row per workflow and columns including:
#' \code{id}, \code{name}, \code{published}, \code{owner}.
#'
#' @details
#' By default, only workflows owned by or shared with the current user
#' are returned. When \code{include_public = TRUE}, the function will
#' attempt to request published workflows as well. Availability of
#' public workflows depends on the Galaxy instance and version.
#'
#' @examplesIf galaxy_has_key()
#' workflows <- galaxy_list_workflows(TRUE)
#' head(workflows)
#'
#' @export
galaxy_list_workflows <- function(
      include_public = FALSE,
      galaxy_url = "https://usegalaxy.eu"
) {

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required.")
  }

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  query <- list()
  if (isTRUE(include_public)) {
    # best-effort: supported by many Galaxy instances
    query$show_published <- TRUE
  }

  res <- httr::GET(
    url = paste0(galaxy_url, "/api/workflows"),
    httr::add_headers(`x-api-key` = api_key),
    query = query
  )

  httr::stop_for_status(res)
  workflows <- httr::content(res, as = "parsed", simplifyVector = TRUE)

  if (length(workflows) == 0) {
    return(data.frame(
      id = character(0),
      name = character(0),
      published = logical(0),
      owner = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    id = workflows$id,
    name = workflows$name,
    published = workflows$published %||% FALSE,
    owner = workflows$owner %||% NA_character_,
    stringsAsFactors = FALSE
  )
}


#' Receive workflow metadata from the API
#'
#' @param workflow_id Character. Galaxy workflow ID.
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#'
#' @returns a structured list with all metadata
#' @export
#'
#' @examplesIf nzchar(Sys.getenv("GALAXY_API_KEY"))
#' \dontrun{
#' galaxy_get_workflow("f2db41e1fa331b3e")
#' }
galaxy_get_workflow <- function(workflow_id,
                                galaxy_url = "https://usegalaxy.eu") {
  galaxy_url <- .resolve_galaxy_url(galaxy_url)
  api_key <- Sys.getenv("GALAXY_API_KEY")
  url <- paste0(galaxy_url, "/api/workflows/", workflow_id)
  res <- httr::GET(url,
                   httr::add_headers(`x-api-key` = api_key),
                   query = list(io_details = "true"))
  httr::stop_for_status(res)
  httr::content(res, "parsed")
}

#' Retrieve input definitions for a Galaxy workflow
#'
#' Retrieves and summarizes the input steps required by a Galaxy workflow.
#'
#' @param workflow_id Character. Galaxy workflow ID.
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#'
#' @return
#' A data.frame with one row per workflow input and the columns:
#' \code{step_id}, \code{name}, \code{type}, \code{optional}, \code{default}.
#'
#' @details
#' This function queries \code{/api/workflows/{workflow_id}} and extracts
#' workflow input steps (data and parameter inputs). The returned
#' \code{step_id} values must be used as names in the \code{inputs} argument
#' of \code{galaxy_start_workflow}.
#'
#' @examplesIf nzchar(Sys.getenv("GALAXY_API_KEY"))
#' \dontrun{
#' galaxy_get_workflow_inputs("f2db41e1fa331b3e")
#' }
#'
#' @export
galaxy_get_workflow_inputs <- function(
    workflow_id,
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

  res <- httr::GET(
    url = paste0(galaxy_url, "/api/workflows/", workflow_id),
    httr::add_headers(`x-api-key` = api_key)
  )
  httr::stop_for_status(res)

  wf <- httr::content(res, as = "parsed")
  steps <- wf$steps

  if (length(steps) == 0) {
    return(data.frame(
      step_id = character(0),
      name = character(0),
      type = character(0),
      optional = logical(0),
      default = character(0),
      stringsAsFactors = FALSE
    ))
  }

  inputs <- lapply(names(steps), function(step_id) {
    step <- steps[[step_id]]

    if (!step$type %in% c("data_input", "parameter_input", "data_collection_input")) {
      return(NULL)
    }

    data.frame(
      step_id = step_id,
      name = step$label %||% step$name %||% NA_character_,
      type = step$type,
      optional = isTRUE(step$optional),
      default = step$default_value %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  inputs <- inputs[!sapply(inputs, is.null)]

  if (length(inputs) == 0) {
    return(data.frame(
      step_id = character(0),
      name = character(0),
      type = character(0),
      optional = logical(0),
      default = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, inputs)
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
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
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
#' @examplesIf galaxy_has_key()
#' input_file <- tempfile(fileext = ".txt")
#' test_text <- "This is an example \nfile."
#' writeLines(test_text,input_file)
#' history_id <- galaxy_initialize("test upload")
#' dataset_id <- galaxy_upload_https(input_file, history_id)
#'
#' galaxy_delete_dataset(dataset_id)
#'
#'
#' @export galaxy_delete_dataset
#' @importFrom httr VERB add_headers content status_code
galaxy_delete_dataset <- function(dataset_id, purge = TRUE, verbose = FALSE, galaxy_url = "https://usegalaxy.eu") {
  api_key <- Sys.getenv("GALAXY_API_KEY")

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#'
#' @return A named list where each element is the return value from
#'   \code{galaxy_delete_dataset} for the corresponding dataset ID.
#'
#' @examplesIf galaxy_has_key()
#' input_file <- tempfile(fileext = ".txt")
#' input_file2 <- tempfile(fileext = ".txt")
#' test_text <- "This is an example \nfile."
#' writeLines(test_text,input_file)
#' writeLines(test_text,input_file2)
#' history_id <- galaxy_initialize("test upload")
#' dataset_id <- galaxy_upload_https(input_file, history_id)
#' dataset_id2 <- galaxy_upload_https(input_file2, history_id)
#'
#' galaxy_delete_datasets(list(output_ids = c(dataset_id, dataset_id2)))
#'
#' @export galaxy_delete_datasets
galaxy_delete_datasets <- function(output_ids, purge = TRUE, sleep = 0.2, galaxy_url = "https://usegalaxy.eu") {

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#' @return data.frame with columns: name, history_id
#'
#' @examplesIf galaxy_has_key()
#' histories <- galaxy_list_histories()
#'
#' @export
galaxy_list_histories <- function(galaxy_url = "https://usegalaxy.eu") {

  galaxy_url <- .resolve_galaxy_url(galaxy_url)
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
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#' @return data.frame with columns: invocation_id, workflow_id, history_id, state, create_time, update_time
#' @export
galaxy_list_invocations <- function(workflow_id, galaxy_url = "https://usegalaxy.eu") {
  if (missing(workflow_id) || identical(workflow_id, "") ) {
    stop("workflow_id is required")
  }

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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

#' Galaxy history size
#' Get the disk usage / size of a Galaxy history
#'
#' The function first tries to read a size/disk_usage field from the history
#' summary endpoint. If that is not present it fetches the history contents
#' and sums dataset sizes (robust to a few different field names used by
#' different Galaxy versions). Results are returned as a data.frame with
#' bytes and a human-readable size.
#'
#' @param history_id Galaxy history id (required)
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#' @param include_deleted Logical; whether to include deleted datasets when summing (default FALSE)
#' @return data.frame with columns history_id, bytes, human_size
#'
#' @examplesIf galaxy_has_key()
#' histories <- galaxy_list_histories()
#' if(nrow(histories > 0)){
#'   galaxy_history_size(histories$history_id[1])
#' } else {
#'   message("No histories found for current user.")
#' }
#'
#'
#' @export galaxy_history_size
galaxy_history_size <- function(history_id,
                                galaxy_url = "https://usegalaxy.eu",
                                include_deleted = FALSE) {
  if (missing(history_id) || identical(history_id, "")) stop("history_id is required")
  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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


#' Set Galaxy connection parameters for the current R session
#'
#' @param api_key Character. Galaxy API key.
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (e.g. \code{"https://usegalaxy.eu"}). If set all galaxy_url arguments of functions will be ignored.
#' @param username Character. Galaxy username (only required for FTP uploads).
#' @param password Character. Galaxy password (only required for FTP uploads).
#' @param overwrite Logical. Whether to overwrite existing environment
#'   variables. Default: \code{TRUE}.
#'
#' @return Invisibly returns a named list of values that were set.
#'
#' @details
#' This helper is intended for interactive sessions. It sets the following
#' environment variables using \code{Sys.setenv()}:
#'
#' \itemize{
#'   \item \code{GALAXY_API_KEY}
#'   \item \code{GALAXY_URL}
#'   \item \code{GALAXY_USERNAME}
#'   \item \code{GALAXY_PASSWORD}
#' }
#'
#' Only arguments that are provided (non-NULL) are set.
#'
#' @examples
#' # This requires valid credentials to your galaxy instance
#' \dontrun{
#' galaxy_set_credentials(
#'   api_key = "your-secret-key",
#'   galaxy_url = "https://usegalaxy.eu",
#'   username = "your-username",
#'   password = "your-password"
#' )
#' }
#'
#' @export
galaxy_set_credentials <- function(api_key = NULL,
                                   galaxy_url = NULL,
                                   username = NULL,
                                   password = NULL,
                                   overwrite = TRUE) {

  values <- list(
    GALAXY_API_KEY = api_key,
    GALAXY_URL = galaxy_url,
    GALAXY_USERNAME = username,
    GALAXY_PASSWORD = password
  )

  if(galaxy_has_key() & overwrite) {
    warning("There was already an API key set which will be overwritten")
  }

  set_values <- list()

  for (name in names(values)) {
    value <- values[[name]]

    if (is.null(value)) {
      next
    }

    if (!nzchar(value)) {
      stop(name, " must be a non-empty string.")
    }

    existing <- Sys.getenv(name, unset = NA_character_)
    if (!isTRUE(overwrite) && !is.na(existing) && nzchar(existing)) {
      stop(name, " already set; use overwrite = TRUE to replace.")
    }

    Sys.setenv(structure(value, names = name))
    set_values[[name]] <- value
  }

  invisible(set_values)
}

#' List tools installed on a Galaxy instance
#'
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
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
#' @examplesIf galaxy_has_key()
#' # All tools (flat list)
#' tools_list <- galaxy_list_tools()
#' length(tools_list)
#'
#' # Panel structure
#' panel_list <- galaxy_list_tools(in_panel = TRUE)
#' length(panel_list)
#'
#' # Tools from a specific panel (match by id or name)
#' tools_list <- galaxy_list_tools(panel_id = "Get Data")
#' length(tools_list)
#'
#' @export
galaxy_list_tools <- function(galaxy_url = "https://usegalaxy.eu",
                              in_panel = FALSE,
                              panel_id = NULL) {
  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#' @param tool_version Optional character string to request a specific
#'   version. If \code{NULL}, Galaxy will return the default/latest
#'   version metadata.
#'
#' @return A list containing the tool metadata as returned by the Galaxy
#'   API (inputs, outputs, help text, etc.).
#'
#' @examplesIf galaxy_has_key()
#' tool_id <- galaxy_get_tool_id("FastQC")[1]
#' fastqc_tool <- galaxy_get_tool(tool_id)
#' fastqc_tool$description
#'
#' @export
galaxy_get_tool <- function(tool_id,
                            galaxy_url = "https://usegalaxy.eu",
                            tool_version = NULL) {
  if (missing(tool_id) || !nzchar(tool_id)) {
    stop("tool_id is required.")
  }

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#' @param panel_id Optional character. Passed through to
#'   \code{galaxy_list_tools} when \code{tools} is \code{NULL} so you can
#'   restrict the search to a panel/section.
#'
#' @return Character vector of matching tool IDs in decreasing order (usually highest version first). Returns \code{character(0)}
#'   if no tools match.
#'
#' @examplesIf galaxy_has_key()
#'
#' # Fetch the full tool list once, then lookup
#' tools <- galaxy_list_tools()
#' galaxy_get_tool_id("FastQC", tools = tools)
#'
#' # Or let the helper fetch on demand
#' galaxy_get_tool_id("FastQC")
#'
#' # Exact, case-sensitive match inside a specific panel
#' galaxy_get_tool_id("Concatenate datasets",
#' ignore_case = FALSE, panel_id = "Text Manipulation")
#'
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

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

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
  return(sort(tool_entries[matches,2], decreasing = TRUE))
}

#' Get information for one or more Galaxy datasets
#'
#' Retrieves metadata for one or more Galaxy history datasets (HDAs),
#' including name, size, type, state, and deletion status.
#'
#' @param file_ids Character vector of Galaxy dataset IDs.
#' @param galaxy_url Character. Base URL of the Galaxy instance
#'   (for example \code{"https://usegalaxy.eu"}).
#'   If the environment variable \code{GALAXY_URL} is set, it takes precedence.
#'
#' @return
#' A data.frame with one row per dataset and the columns:
#' \code{id}, \code{name}, \code{size_bytes}, \code{human_size},
#' \code{file_type}, \code{state}, \code{deleted}.
#'
#' @details
#' This function queries the \code{/api/datasets/{id}} endpoint for each
#' provided dataset ID. If a dataset cannot be retrieved, its fields
#' are returned as \code{NA}.
#'
#' @examplesIf nzchar(Sys.getenv("GALAXY_API_KEY"))
#' tmp_dir <- tempdir()
#' f_name <- "iris.csv"
#' f_path <- paste(tmp_dir, f_name, sep = "\\")
#' write.csv(datasets::iris, f_path, row.names = FALSE)
#'
#' history_id <- galaxy_initialize("IRIS")
#' file_id <- galaxy_upload_https(f_path, history_id)
#' galaxy_get_file_info(file_id)
#'
#' @export
galaxy_get_file_info <- function(file_ids,
                                 galaxy_url = "https://usegalaxy.eu") {

  if (missing(file_ids) || length(file_ids) == 0) {
    stop("file_ids must be a non-empty character vector")
  }

  galaxy_url <- .resolve_galaxy_url(galaxy_url)

  api_key <- Sys.getenv("GALAXY_API_KEY")
  if (!nzchar(api_key)) {
    stop("GALAXY_API_KEY environment variable is not set.")
  }

  # helper: human-readable bytes
  human_bytes <- function(bytes) {
    if (is.na(bytes) || length(bytes) == 0) return(NA_character_)
    b <- suppressWarnings(as.numeric(bytes))
    if (is.na(b)) return(NA_character_)
    units <- c("B", "KB", "MB", "GB", "TB")
    if (b == 0) return("0 B")
    idx <- floor(log(b, 1024))
    idx <- pmin(idx, length(units) - 1)
    sprintf("%.2f %s", b / (1024 ^ idx), units[idx + 1])
  }

  file_ids <- as.character(file_ids)

  results <- lapply(file_ids, function(fid) {

    res <- try(
      httr::GET(
        url = paste0(galaxy_url, "/api/datasets/", fid),
        httr::add_headers(`x-api-key` = api_key)
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error") || httr::status_code(res) >= 400) {
      return(data.frame(
        id = fid,
        name = NA_character_,
        size_bytes = NA_real_,
        human_size = NA_character_,
        file_type = NA_character_,
        state = NA_character_,
        deleted = NA,
        stringsAsFactors = FALSE
      ))
    }

    ds <- httr::content(res, as = "parsed")

    # robust size handling across Galaxy versions
    size <- ds$file_size
    if (is.null(size)) size <- ds$file_size_bytes
    if (is.null(size)) size <- ds$disk_usage
    size <- suppressWarnings(as.numeric(size))

    data.frame(
      id = ds$id %||% fid,
      name = ds$name %||% NA_character_,
      size_bytes = size,
      human_size = human_bytes(size),
      file_type = ds$extension %||% NA_character_,
      state = ds$state %||% NA_character_,
      deleted = isTRUE(ds$deleted),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
