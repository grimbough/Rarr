## We'll come back to this later


# setClass(Class = "Zarrchive",
#          slots = c(
#            path = "character",
#            metadata = "ANY"
#          ))
#
#
# openZarr <- function(zarr_file) {
#
#   metadata <- read_zarr_metadata(zarr_file)
#
#   new("Zarrchive",
#       path = zarr_file,
#       metadata = metadata)
#
#
# }
#
#
# setMethod("show",
#   signature = "Zarrchive",
#   function(object) {
#     cat("Location: ", object@path, "\n", sep = "")
# 
#     arrays <- grep(names(metadata$metadata), pattern = ".zarray$", 
#                    value = TRUE)
#     for (i in seq_along(arrays)) {
#       array_md <- read_array_metadata(file.path(object@path, 
#                                                 dirname(arrays[i])))
#       cat(dirname(arrays[i]), ":", paste(unlist(rev(array_md$shape)), 
#                                          collapse = " x "), "\n")
#     }
#   }
# )
