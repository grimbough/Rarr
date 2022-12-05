#' @import jsonlite
read_metadata <- function(zarr_file) {
  metadata <- read_json(file.path(zarr_file, ".zarray"))
  return(metadata)
}

