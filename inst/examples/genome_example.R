# This is an example showing how to use GalaxyR to run a simple genomics workflow in Galaxy.
# It follows the first part of the instructions of the "Galaxy Basics for genomics" tutorial from the Galaxy Training Network (GTN).
# https://training.galaxyproject.org/training-material/topics/introduction/tutorials/galaxy-intro-101/tutorial.html

library(GalaxyR)

# -------------------------------------------------------------------
# 0) Setup
# -------------------------------------------------------------------
# Assumes GALAXY_API_KEY is already set (or use galaxy_set_credentials()).
galaxy_set_credentials(
  api_key    = Sys.getenv("GALAXY_API_KEY"),
  galaxy_url = "https://usegalaxy.eu"
)

# Create Galaxy session object and initialize a history
gxy <- galaxy(history_name = "Galaxy Basics for genomics count SNPs per exon")
gxy <- galaxy_initialize(gxy)

# -------------------------------------------------------------------
# 1) Download tutorial input files from Zenodo
# -------------------------------------------------------------------
exons_url <- "https://zenodo.org/record/4104428/files/UCSC-hg38-chr22-Coding-Exons.bed"
snps_url  <- "https://zenodo.org/record/4104428/files/UCSC-hg38-chr22-dbSNP153-Whole-Gene-SNPs.bed"

exons_file <- file.path(tempdir(), "UCSC-hg38-chr22-Coding-Exons.bed")
snps_file  <- file.path(tempdir(), "UCSC-hg38-chr22-dbSNP153-Whole-Gene-SNPs.bed")

download.file(exons_url, exons_file, mode = "wb")
download.file(snps_url,  snps_file,  mode = "wb")

# -------------------------------------------------------------------
# 2) Upload Exons and SNPs to Galaxy
# -------------------------------------------------------------------
gxy <- galaxy_upload_https(gxy, exons_file, wait = TRUE, file_type = "bed", dbkey = "hg38")
exons_id <- gxy@input_dataset_id

gxy <- galaxy_upload_https(gxy, snps_file, wait = TRUE, file_type = "bed", dbkey = "hg38")
snps_id <- gxy@input_dataset_id

# -------------------------------------------------------------------
# 3) bedtools intersect intervals
# -------------------------------------------------------------------
tools <- galaxy_list_tools()
intersect_tool_id <- galaxy_get_tool_id("bedtools Intersect intervals", tools = tools)[1]

# optional: you can inspect the tool inputs and parameters with:
inputs <- galaxy_get_tool(intersect_tool_id)
galaxy_print_tool_inputs(inputs)

# run the tool
gxy <- galaxy_run_tool(
  gxy,
  tool_id = intersect_tool_id,
  inputs = list(
    inputA = list(src = "hda", id = exons_id),
    reduce_or_iterate = list(
      value = "iterate",
      inputB = list(src = "hda", id = snps_id)
    ),
    strand = "",
    overlap_mode = "-wb",
    fraction_cond = list(value = "no"),
    genome_file_opts = list(value = "no")
  )
)

# poll the result until the tool finishes and the output dataset is ready
gxy <- galaxy_poll_tool(gxy)
gxy@input_dataset_id <- gxy@output_dataset_ids[1]

# -------------------------------------------------------------------
# 4) Datamash: Count unique SNP IDs per exon
# -------------------------------------------------------------------
datamash_tool_id <- galaxy_get_tool_id("Datamash", tools = tools)[1]

# Optional: inspect exact input schema:
inputs <- galaxy_get_tool(datamash_tool_id)
galaxy_print_tool_inputs(inputs)

# run the Datamash tool
gxy <- galaxy_run_tool(
  gxy,
  tool_id = datamash_tool_id,
  inputs = list(
    in_file = list(src = "hda", id = intersect_out_id),
    grouping = "4",
    operations = list(
      list(
        op_name = "countunique",
        op_column = "10"
      )
    )
  )
)

# wait for the tool to finish and poll the result
gxy <- galaxy_poll_tool(gxy)

count_per_exon_id <- gxy@output_dataset_ids[1]

# Optional: inspect and download result
galaxy_get_file_info(count_per_exon_id)
galaxy_download_result(count_per_exon_id, out_dir = ".")

