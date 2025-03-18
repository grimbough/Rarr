#' create_zarr_group
#'
#' create zarr groups
#' 
#' @param store the location of (zarr) store
#' @param name name of the group
#' @param version zarr version
#' @export
create_zarr_group <- function(store, name, version = "v2"){
  split.name <- strsplit(name, split = "\\/")[[1]]
  if(length(split.name) > 1){
    split.name <- vapply(seq_len(length(split.name)), 
                         function(x) paste(split.name[seq_len(x)], collapse = "/"), 
                         FUN.VALUE = character(1)) 
    split.name <- rev(tail(split.name,2))
    if(!dir.exists(file.path(store,split.name[2])))
      create_zarr_group(store = store, name = split.name[2])
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  switch(version, 
         v2 = {
           write("{\"zarr_format\":2}", file = file.path(store, split.name[1], ".zgroup"))},
         v3 = {
           stop("Currently only zarr v2 is supported!") 
         },
         stop("only zarr v2 is supported. Use version = 'v2'")
         )
}

#' create_zarr
#'
#' create zarr store
#'
#' @param dir the location of zarr store
#' @param prefix prefix of the zarr store
#' @param version zarr version
#' 
#' @examples
#' dir.create(td <- tempfile())
#' zarr_name <- "test"
#' create_zarr(dir = td, prefix = "test")
#' dir.exists(file.path(td, "test.zarr"))
#' 
#' @export
create_zarr <- function(dir, prefix, version = "v2"){
  create_zarr_group(store = dir, name = paste0(prefix, ".zarr"), version = version)
}