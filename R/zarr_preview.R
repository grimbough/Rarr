zarr_preview <- function(zarr_path) {
  metadata <- .read_array_metadata(zarr_path)
  if (length(metadata$shape) > 2) {
    stop("`zarr_preview()` is only implemented for 1D and 2D arrays")
  }
  chunks <- list.files(zarr_path, recursive = TRUE, full.names = TRUE)
  chunk_sizes <- file.size(chunks)
  browser()
  chunk_coords <- gsub(paste0(zarr_path, "/"), "", chunks) |>
    strsplit(
      metadata$chunk_key_encoding$configuration$separator,
      fixed = TRUE
    ) |>
    lapply(as.integer) |>
    do.call(rbind, args = _) |>
    as.data.frame() |>
    cbind(chunk_sizes) |>
    dplyr::arrange(V1, V2)
  ggplot(chunk_coords, aes(x = V1, y = V2, fill = chunk_sizes)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(
      x = "Chunk coordinate 1",
      y = "Chunk coordinate 2",
      fill = "Information density\n(= chunk size in bytes)"
    ) +
    theme_minimal()
}
