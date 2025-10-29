## functions to communigate with the galaxy api
## written by Julian Frey
## 2025-10-27

#' Start a session with the GALAXY API
#' @param galaxy_url Your Galaxy API key
#' @param name Name of the history to create
#' @return history_id The ID of the created history
#' @examples
#' \donotrun{
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
#'
#' @returns dataset_id The ID of the uploaded dataset in Galaxy
#' @export galaxy_upload
#'
#' @examples
#' \donotrun{
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
#'
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
#' /api/datasets/{id} endpoint. By default it requests a purge
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
