library(lidR)
library(GalaxyR)
library(future)

# ---- CONFIGURATION ----
workflow_id <- "d3bf3b2d7d56619b"
input_dir <- "Z:/Sattelmuehle_precision_inventory/ALS/5_Pointclouds/laz_files"
output_dir <- "Z:/Sattelmuehle_precision_inventory/ALS/7_Output/SAT_output/"

# LAS Catalog example
catalog <- lidR::readTLScatalog(input_dir, select = "XYZ")

# retile catalog
lidR::opt_chunk_size(catalog) <- 250
lidR::opt_chunk_buffer(catalog) <- 10
lidR::opt_output_files(catalog) <- paste0(output_dir,"{ID}")

future::plan(future::multisession, workers = 20L)

catalog_function <- function(cluster) {

  las <- suppressWarnings(lidR::readLAS(cluster)) # read files
  if (lidR::is.empty(las) ) return(NULL) # stop if empty

  # get the bbox bbbox
  bbox <- cluster@bbox
  bbbox <- cluster@bbbox

  # get the TileID
  tile_id <- as.numeric(basename(cluster@save))

  # write the cluster to a tmp file
  tmpfile <- tempfile(fileext = ".laz")
  lidR::writeLAS(las, tmpfile)

  tmpdir <- tmpdir()
  # initialize a history
  gxy <- galaxy(paste0("sattelm_", tile_id)) |>
    galaxy_initialize() |>
    galaxy_upload(tmpfile, history_id) |>
    galaxy_start_workflow(workflow_id) |>
    galaxy_poll_workflow(invocation_id, poll_interval = 30) |>
    galaxy_download_result(tmpdir)

  if(!gxy@state == "success") return(NULL)

  galaxy_delete_datasets(gxy$output_dataset_ids)

  # read the result
  las <- lidR::readTLS(list.files(tmpdir, pattern = "*.laz")[1])
  if (lidR::is.empty(las)) return(NULL)

  # get the row index of the highest Z per PredInstance
  idx <- las@data[, .I[which.max(Z)], by = PredInstance]$V1

  # subset to get X,Y,Z (and PredInstance if you want)
  highest_points <- las@data[idx, .(PredInstance, X, Y, Z)]

  # Get PredINstance IDs of heighest points in bbox
  highest_points <- highest_points[highest_points$X >= bbox[1,1] & highest_points$X <= bbox[1,2] &
                                       highest_points$Y >= bbox[2,1] & highest_points$Y <= bbox[2,2], ]

  # filter las by InstanceIds with highest points in bbox
  las <- las |> lidR::filter_poi(PredInstance %in% highest_points$PredInstance)
  if(lidR::is.empty(las) ) return(NULL)

  # renumber trees
  las@data$PredInstance <- las@data$PredInstance + (100000 * tile_id)

  return(las)
}

ctg <- lidR::catalog_apply(catalog, catalog_function)


