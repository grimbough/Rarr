#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# simple --key=val parser
options <- args |>
  Filter(\(x) startsWith(x, "--"), x = _) |>
  strsplit("=", fixed = TRUE)

opt_list <- setNames(
  vapply(options, `[`, 2, FUN.VALUE = character(1)),
  gsub("^--", "", vapply(options, `[`, 1, FUN.VALUE = character(1)))
)

res <- Rarr::read_zarr_array(
  zarr_array_path = opt_list[["array_path"]]
)
