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
  
  return(index)
  
}

s3_provider <- function(path) {
  
  if(!grepl(pattern = "(^https?://)|(^s3://)", x = path)) {
    provider <- NULL
  } else {
    matches <- regmatches(x = path, 
                          m = regexpr(pattern = "(amazonaws\\.com)|(embl\\.de)", 
                                      text = path))
    if(!length(matches)) { matches <- "other" }
    provider <- switch(matches,
                       "amazonaws.com" = "aws",
                       "embl.de"       = "other",
                       "other")
  }
  return(provider)
}

url_parse_aws <- function(url) {
  
  res <- list()
  
  if(grepl(pattern = "^https?://s3\\.", x = url, ignore.case = TRUE)) {
    tmp <- httr2::url_parse(url)
    bucket <- gsub(x = tmp$path, pattern = "^/([a-z0-9\\.-]*)/.*", replacement = "\\1", ignore.case = TRUE)
    object <- gsub(x = tmp$path, pattern = "^/([a-z0-9\\.-]*)/(.*)", replacement = "\\2", ignore.case = TRUE)
    region <- gsub(x = url, pattern = "^https?://s3\\.([a-z0-9-]*)\\.amazonaws\\.com/.*$", replacement = "\\1")
  } else if (grepl(pattern = "^https?://[a-z0-9\\.-]*.s3\\.", x = url, ignore.case = TRUE)) {
    tmp <- httr2::url_parse(url)
    bucket <- gsub(x = tmp$hostname, pattern = "^([a-z0-9\\.-]*)\\.s3.*", replacement = "\\1", ignore.case = TRUE)
    object <- tmp$path
    region <- gsub(x = tmp$hostname, pattern = "^.*\\.s3\\.([a-z0-9-]*)\\.amazonaws\\.com$", replacement = "\\1", ignore.case = TRUE)
  } else {
    stop("Unknown AWS path style.  Please report this the package maintainer.")
  }
  
  res$bucket <- bucket
  res$object <- object
  res$region <- region
  res$hostname <- "s3.amazonaws.com"
  
  return(res)
  
}
