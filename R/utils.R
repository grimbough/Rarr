#' @keywords Internal
check_index <- function(index, metadata) {
  
  ## check we have the correct number of dimensions
  if(isFALSE(length(index) == length(metadata$shape))) {
    stop("The number of dimensions provided to 'index' does not match the shape of the array")
  }
  
  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep(FALSE, n = length(index))
  for(i in seq_along(index)) {
    if (is.null(index[[i]])) {
      index[[i]] <- seq_len(metadata$shape[[i]])
    } else if(any(index[[i]] < 1) || any(index[[i]] > metadata$shape[[i]])) {
      failed[i] <- TRUE
    }
  }
  
  if(any(failed)) {
    stop(sprintf("Selected indices for dimension(s) %s are out of range.", 
                 paste(which(failed), collapse = " & ")))
  }
  
  invisible(TRUE)
  
}
