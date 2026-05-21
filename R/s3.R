parse_s3_path <- function(path) {
  s3_provider <- .determine_s3_provider(path)

  if (is.null(s3_provider)) {
    parsed_url <- NULL
  } else if (s3_provider == "aws") {
    parsed_url <- .url_parse_aws(path)
  } else {
    parsed_url <- .url_parse_other(path)
  }

  return(parsed_url)
}

.determine_s3_provider <- function(path) {
  if (!any(startsWith(path, c("http://", "https://", "s3://")))) {
    return(NULL)
  }
  if (grepl("amazonaws.com", path, fixed = TRUE)) {
    return("aws")
  }
  return("other")
}

#' @keywords internal
.url_parse_aws <- function(url) {
  if (grepl(pattern = "^https?://s3\\.", x = url, ignore.case = TRUE)) {
    ## path style address
    url_parts <- regmatches(
      url,
      regexec(
        "^https?://s3\\.([a-z0-9-]*)\\.amazonaws\\.com/?([a-z0-9\\.-]*)/(.*)",
        url,
        ignore.case = TRUE
      )
    )[[1L]]
    region <- url_parts[2L]
    bucket <- url_parts[3L]
    object <- url_parts[4L]
  } else if (
    grepl(
      pattern = "^https?://[a-z0-9\\.-]*\\.s3\\.",
      x = url,
      ignore.case = TRUE
    )
  ) {
    ## virtual-host style address
    tmp <- curl::curl_parse_url(url)
    bucket <- sub(
      x = tmp$host,
      pattern = "^([a-z0-9\\.-]*)\\.s3.*",
      replacement = "\\1",
      ignore.case = TRUE
    )
    object <- sub("^/?(.*)", "\\1", tmp$path)
    region <- sub(
      x = tmp$host,
      pattern = "^.*\\.s3\\.([a-z0-9-]*)\\.amazonaws\\.com$",
      replacement = "\\1",
      ignore.case = TRUE
    )
  } else {
    stop(
      "Unknown AWS path style.  Please report this to the package maintainer."
    )
  }

  res <- list(
    bucket = bucket,
    object = object,
    region = region,
    hostname = "https://s3.amazonaws.com"
  )
  return(res)
}

#' @keywords internal
.url_parse_other <- function(url) {
  parsed_url <- curl::curl_parse_url(url)
  path_parts <- regmatches(
    parsed_url$path,
    regexec("^/?([a-z0-9:\\.-]*)/(.*)", parsed_url$path, ignore.case = TRUE)
  )[[1L]]
  bucket <- path_parts[2L]
  object <- path_parts[3L]
  hostname <- paste0(parsed_url$scheme, "://", parsed_url$host)

  if (!is.null(parsed_url$port)) {
    hostname <- paste0(hostname, ":", parsed_url$port)
  }

  res <- list(
    bucket = bucket,
    object = object,
    region = "auto",
    hostname = hostname
  )
  return(res)
}

#' This is a modified version of paws.storage:::get_credentials().  It is
#' included to prevent using the `:::` operator.  Look at that function if
#' things stop working.
#'
#' @param credentials Content stored at `.internal$config$credentials` in
#' an object created by `paws.storage::s3()`.
#'
#' @returns A credentials list to be reinserted into a `paws.storage` s3 object.
#' If no valid credentials are found this function will error, which is expected
#' and is caught by `.check_credentials`.
#'
#' @keywords internal
.get_credentials <- function(credentials) {
  for (provider in credentials$provider) {
    args <- names(formals(provider))
    if (is.null(args)) {
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

  if (inherits(test, "try-error")) {
    s3_client <- s3(
      config = list(
        credentials = list(
          anonymous = TRUE
        ),
        region = parsed_url$region,
        endpoint = parsed_url$hostname
      )
    )
  }

  return(s3_client)
}


#' @importFrom paws.storage s3
.create_s3_client <- function(path) {
  parsed_url <- parse_s3_path(path)

  if (is.null(parsed_url)) {
    return(NULL)
  }
  s3_client <- s3(
    config = list(
      region = parsed_url$region,
      endpoint = parsed_url$hostname
    )
  )

  .check_credentials(s3_client, parsed_url)
}

.s3_object_exists <- function(s3_client, Bucket, Key) {
  exists <- s3_client$list_objects_v2(
    Bucket = Bucket,
    Prefix = Key
  )$KeyCount >
    0L

  return(exists)
}
