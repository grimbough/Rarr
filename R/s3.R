
parse_s3_path <- function(path) {
  
  s3_provider <- .determine_s3_provider(path)
  
  if(is.null(s3_provider)) {
    parsed_url <- NULL
  } else if (s3_provider == "aws") {
    parsed_url <- .url_parse_aws(path)
  } else {
    parsed_url <- .url_parse_other(path)
  }
  
  return(parsed_url)
  
}

.determine_s3_provider <- function(path) {
  if (!grepl(pattern = "(^https?://)|(^s3://)", x = path)) {
    provider <- NULL
  } else {
    matches <- regmatches(
      x = path,
      m = regexpr(
        pattern = "(amazonaws\\.com)|(embl\\.de)",
        text = path
      )
    )
    if (!length(matches)) {
      matches <- "other"
    }
    provider <- switch(matches,
                       "amazonaws.com" = "aws",
                       "embl.de"       = "other",
                       "other"
    )
  }
  return(provider)
}

#' @importFrom httr parse_url
#' @keywords Internal
.url_parse_aws <- function(url) {
  tmp <- parse_url(url)
  
  if (grepl(pattern = "^https?://s3\\.", x = url, ignore.case = TRUE)) {
    ## path style address
    bucket <- gsub(x = tmp$path, pattern = "^/?([a-z0-9\\.-]*)/.*", 
                   replacement = "\\1", ignore.case = TRUE)
    object <- gsub(x = tmp$path, pattern = "^/?([a-z0-9\\.-]*)/(.*)", 
                   replacement = "\\2", ignore.case = TRUE)
    region <- gsub(x = url, 
                   pattern = "^https?://s3\\.([a-z0-9-]*)\\.amazonaws\\.com/.*$", 
                   replacement = "\\1")
  } else if (grepl(pattern = "^https?://[a-z0-9\\.-]*.s3\\.", x = url, ignore.case = TRUE)) {
    ## virtual-host style address
    bucket <- gsub(x = tmp$hostname, pattern = "^([a-z0-9\\.-]*)\\.s3.*", 
                   replacement = "\\1", ignore.case = TRUE)
    object <- tmp$path
    region <- gsub(x = tmp$hostname, 
                   pattern = "^.*\\.s3\\.([a-z0-9-]*)\\.amazonaws\\.com$", 
                   replacement = "\\1", ignore.case = TRUE)
  } else {
    stop("Unknown AWS path style.  Please report this to the package maintainer.")
  }
  
  res <- list(bucket = bucket, object = object, region = region, 
              hostname = "https://s3.amazonaws.com")
  return(res)
}

#' @importFrom httr parse_url
#' 
#' @keywords Internal
.url_parse_other <- function(url) {
  parsed_url <- httr::parse_url(url)
  bucket <- gsub(x = parsed_url$path, pattern = "^/?([[a-z0-9\\.-]*)/.*", 
                 replacement = "\\1", ignore.case = TRUE)
  object <- gsub(x = parsed_url$path, pattern = "^/?([a-z0-9\\.-]*)/(.*)", 
                 replacement = "\\2", ignore.case = TRUE)
  
  res <- list(bucket = bucket, 
              object = object, 
              region = "auto", 
              hostname = paste0(parsed_url$scheme, "://", parsed_url$hostname))
  return(res)
}

#' This is a modified version of paws.storge:::get_credentials().  It is
#' included to prevent using the `:::` operator.  Look at that function if
#' things stop working.
#' 
#' @param credentials Content stored at `.internal$config$credentials` in 
#' an object created by `paws.storage::s3()`.
#' 
#' @returns A credentials list to be reinserted into a `paw.storage` s3 object.
#' If no valid credentials are found this function will error, which is expected
#' and is caught by `.check_credentials`.
#' 
#' @importFrom methods formalArgs
#' @keywords Internal
.get_credentials <- function(credentials) {
  
  for (provider in credentials$provider) {
    args <- formalArgs(provider)
    if(is.null(args)) { 
      creds <- provider()
    } else {
      creds <- do.call(provider, as.list(credentials)[args])
    }
    if (!is.null(creds)) {
      credentials$creds <- creds
      break
    }
  }
  return(credentials)
}
  
.check_credentials <- function(s3_client, parsed_url) {
  
  test <- try(
    .get_credentials(s3_client$.internal$config$credentials), 
    silent = TRUE
  )
  
  if(inherits(test, "try-error")) 
    s3_client <- s3(
      config = list(
        credentials = list(
          anonymous = TRUE
        ),
        region = parsed_url$region,
        endpoint = parsed_url$hostname
      )
    )
  
  return(s3_client)
}


#' @importFrom paws.storage s3
.create_s3_client <- function(path) {
  
  parsed_url <- parse_s3_path(path)
  
  if(is.null(parsed_url)) {
    s3_client <- NULL 
  } else {
    
    s3_client <- s3(
      config = list(
        region = parsed_url$region,
        endpoint = parsed_url$hostname
      )
    )
    
    s3_client <- .check_credentials(s3_client, parsed_url)
  }
  return(s3_client)
}

.s3_object_exists <- function(s3_client, Bucket, Key) {
  
  exists <- tryCatch(
    expr = { 
      s3_client$head_object(Bucket = Bucket, Key = Key)$ContentLength > 0 
    }, 
    error = function(e) { FALSE } 
  )
  
  return(exists)
}