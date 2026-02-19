# GalaxyR

## Description <img src="https://github.com/JulFrey/GalaxyR/blob/main/inst/figures/logo_galaxyr.png" align="right" width="290/"/>

**GalaxyR** is an R package for programmatic interaction with the **Galaxy API** (tested primarily against [Galaxy Europe](https://usegalaxy.eu)).\
It allows you to manage histories, upload data, run tools and workflows, wait for jobs to complete, and download results — all directly from R.

This package is designed for **automation, reproducibility, and scripting**, not UI replacement.

[![R-CMD-check](https://github.com/JulFrey/GalaxyR/actions/workflows/r.yml/badge.svg)](https://github.com/JulFrey/GalaxyR/actions/workflows/r.yml)

------------------------------------------------------------------------

## Features

-   🔑 Simple API key management
-   📁 Create and manage Galaxy histories
-   ⬆️ Upload datasets via HTTPS or FTP
-   🔧 Discover and inspect Galaxy tools and workflows
-   ▶️ Run tools and workflow programmatically
-   ⏳ Wait for jobs to finish (robust polling)
-   📥 Download resulting datasets
-   📊 Inspect history size and disk usage
-   🔧 S4-based interface for pipe friendly usage

------------------------------------------------------------------------

## Installation

Install this package from CRAN:
``` r
install.packages("GalaxyR")
```

Or install the latest version directly from GitHub:
``` r
# install.packages("remotes")
remotes::install_github("JulFrey/GalaxyR")
```

------------------------------------------------------------------------

## Authentication

Before using the package, you must set your **Galaxy API key**.

You can either:

### Option 1: Set it once per session

``` r
galaxy_set_credentials("your-secret-key")
```

### Option 2: Add it to `~/.Renviron` (recommended)

``` r       
#usethis::edit_r_environ()
GALAXY_API_KEY = your-secret-key
```

Restart R after editing`.Renviron`.

------------------------------------------------------------------------

## Supported Galaxy Instances

The default Galaxy instance is:

``` r
"https://usegalaxy.eu"
```

Most functions accept a `galaxy_url` argument if you want to target a different Galaxy server.

------------------------------------------------------------------------

## Basic Workflow Example

Below is a complete example that:

1.  Creates a new history
2.  Uploads a text file
3.  Runs the **“Add line to file”** tool
4.  Waits for the job to complete
5.  Downloads and inspects the result

``` r
# Load GalaxyR
library(GalaxyR)

# Get the tool ID and inspect inputs 
tool <- galaxy_get_tool_id("Add line to file")
inputs <- galaxy_get_tool(tool)

# Create a tiny test file
test_file <- tempfile(fileext = ".txt")
test_text <- "This is an example \ntest file."
writeLines(test_text,test_file)

# directory for outputs
outdir <- tempdir()

# Run on Galaxy
gxy <- galaxy(history_name = "add line example") |> # S4 class with history name
  galaxy_initialize() |> # initialise Galaxy history
  galaxy_upload_https(test_file) |> # upload test file
  galaxy_run_tool(tool, inputs = list(text_input = "added example text")) |> # run 
  galaxy_poll_tool() |> # wait for completion
  galaxy_download_result(outdir)

# Inspect the result
readLines(list.files(outdir, full.names = TRUE)[1])
```

------------------------------------------------------------------------

## Important Notes on Tool Inputs

-   **Always use input `name`, not label**\
    Example: `text_input`, not `"text to add"`

-   **Dataset inputs must be passed as objects**, not plain strings:

    ``` r
    infile = list(
      src = "hda",
      id  = DATASET_ID
    )
    ```

-   You can inspect expected inputs using:

    ``` r
    galaxy_get_tool(tool_id)
    ```

------------------------------------------------------------------------

## Job and Dataset States

Galaxy jobs and datasets are **asynchronous**.

This package provides helpers to wait safely until execution finishes:
`galaxy_poll_tool()` waits for tool execution.

Terminal states: - ✅ `ok` - ❌ `error` - 🗑️ `deleted`

------------------------------------------------------------------------

## Common Helper Functions

| Function                   | Description                |
|----------------------------|----------------------------|
| `galaxy_initialize()`      | Create a new history       |
| `galaxy_upload_https()`    | Upload a file via HTTPS    |
| `galaxy_run_tool()`        | Run a Galaxy tool          |
| `galaxy_poll_tool()`       | Wait for tool completion   |
| `galaxy_download_result()` | Download dataset           |
| `galaxy_get_tool()`        | Inspect tool metadata      |
| `galaxy_list_tools()`      | List installed tools       |
| `galaxy_history_size()`    | Compute history disk usage |

------------------------------------------------------------------------

Author:<br> Julian Frey<br> Chair of Forest Growth and Dendroecology<br> University of Freiburg
