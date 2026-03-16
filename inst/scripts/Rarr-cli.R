#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# simple --key=val parser
options <- args |>
  grepv(pattern = "^--", x = _) |>
  strsplit("=", fixed = TRUE) |>
  do.call(rbind, args = _)

opt_list <- setNames(options[, 2], gsub("^--", "", options[, 1]))

res <- Rarr::read_zarr_array(
  zarr_array_path = opt_list[["array_path"]]
)
