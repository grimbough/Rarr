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

res <- Rarr::read_zarr_array(
  zarr_array_path = opt_list[["array_path"]]
)
