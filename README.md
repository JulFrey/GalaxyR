# GalaxyRTools

**GalaxyRTools** is an R package for programmatic interaction with the **Galaxy API** (tested primarily against [Galaxy Europe](https://usegalaxy.eu)).  
It allows you to manage histories, upload data, run tools and workflows, wait for jobs to complete, and download results — all directly from R.

This package is designed for **automation, reproducibility, and scripting**, not UI replacement.

[![R-CMD-check](https://github.com/JulFrey/GalaxyR/actions/workflows/r.yml/badge.svg)](https://github.com/JulFrey/GalaxyR/actions/workflows/r.yml)

---

## Features

- 🔑 Simple API key management
- 📁 Create and manage Galaxy histories
- ⬆️ Upload datasets via HTTPS or FTP
- 🔧 Discover and inspect Galaxy tools
- ▶️ Run tools programmatically
- ⏳ Wait for jobs to finish (robust polling)
- 📥 Download resulting datasets
- 📊 Inspect history size and disk usage
- 🧬 Workflow invocation support (experimental)

---

## Installation

This package is not yet on CRAN. Install directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("JulFrey/GalaxyR")
```

---

## Authentication

Before using the package, you must set your **Galaxy API key**.

You can either:

### Option 1: Set it once per session
```r
galaxy_set_credentials("your-secret-key")
```

### Option 2: Add it to `~/.Renviron` (recommended)
```
#usethis::edit_r_environ()
GALAXY_API_KEY = your-secret-key
```

Restart R after editing `.Renviron`.

---

## Supported Galaxy Instances

The default Galaxy instance is:

```
https://usegalaxy.eu
```

Most functions accept a `galaxy_url` argument if you want to target a different Galaxy server.

---

## Basic Workflow Example

Below is a complete example that:

1. Creates a new history
2. Uploads a text file
3. Runs the **“Add line to file”** tool
4. Waits for the job to complete
5. Downloads and inspects the result

```r
# Initialize a new history
history_id <- galaxy_initialize("add line")

# Find the tool ID by name
tool <- galaxy_get_tool_id("Add line to file")

# Create a temporary input file
test_file <- tempfile(fileext = ".txt")
test_text <- "This is an example \ntest file."
writeLines(test_text, test_file)

# Upload the file to Galaxy
file_id <- galaxy_upload_https(test_file, history_id)

# (Optional) Inspect tool inputs
inputs <- galaxy_get_tool(tool)

# Run the tool
job_id <- galaxy_run_tool(
  tool_id = tool,
  history_id = history_id,
  inputs = list(
    text_input = "added text",
    infile = list(
      src = "hda",
      id  = file_id$id
    ),
    options = "header"   # add text at the top of the file
  )
)

# Wait for the job to finish
result <- galaxy_wait_for_job(job_id)

# Get the output dataset ID
result$outputs$outfile$id

# Download the result
test_file_result <- tempfile(fileext = ".txt")
galaxy_download_result(
  list(output_ids = result$outputs$outfile$id),
  test_file_result
)

# Inspect the result
readLines(test_file_result)
```

---

## Important Notes on Tool Inputs

- **Always use input `name`, not label**  
  Example: `text_input`, not `"text to add"`

- **Dataset inputs must be passed as objects**, not plain strings:
  ```r
  infile = list(
    src = "hda",
    id  = DATASET_ID
  )
  ```

- You can inspect expected inputs using:
  ```r
  galaxy_get_tool(tool_id)
  ```

---

## Job and Dataset States

Galaxy jobs and datasets are **asynchronous**.

This package provides helpers to wait safely until execution finishes:

- `galaxy_wait_for_job()` — waits for tool execution
- `galaxy_wait_for_dataset()` — waits for dataset processing

Terminal states:
- ✅ `ok`
- ❌ `error`
- 🗑️ `deleted`

---

## Common Helper Functions

| Function | Description |
|--------|-------------|
| `galaxy_initialize()` | Create a new history |
| `galaxy_upload_https()` | Upload a file via HTTPS |
| `galaxy_run_tool()` | Run a Galaxy tool |
| `galaxy_wait_for_job()` | Wait for job completion |
| `galaxy_download_result()` | Download dataset |
| `galaxy_get_tool()` | Inspect tool metadata |
| `galaxy_list_tools()` | List installed tools |
| `galaxy_history_size()` | Compute history disk usage |

---

Author: 
Julian Frey
Chair of Forest Growth and Dendroecology
University of Freiburg

