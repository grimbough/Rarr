#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# simple --key=val parser
options <- args |>
  grepv(pattern = "^--", x = _) |>
  strsplit("=") |>
  do.call(rbind, args = _) |>
  as.data.frame() |>
  setNames(c("arg", "val"))

opt_list <- setNames(options$val, gsub("^--", "", options$arg))

dims <- Rarr::zarr_overview(
  zarr_array_path = opt_list[["array_path"]],
  as_data_frame = TRUE
)$dim[[1]]

res <- Rarr::read_zarr_array(
  zarr_array_path = opt_list[["array_path"]]
)
