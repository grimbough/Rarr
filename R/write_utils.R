#' Truncate overflow values and warn about bool NA values before writing
#'
#' Applies the two pre-write data checks that are needed identically in both
#' [write_zarr_array()] and [update_zarr_array()]:
#'
#' 1. If the target type is narrower than R's native representation (e.g.
#'    `int8`, `int16`, `float32`), clamp out-of-range values via
#'    `.truncate_overflow()`.
#' 2. If the target type is `bool`, warn when `x` contains `NA` values (which
#'    the Zarr bool type cannot represent).
#'
#' @param x The R array to be written.
#' @param metadata Array metadata list as returned by [.read_array_metadata()].
#'
#' @returns `x`, possibly with overflow values clamped.
#'
#' @keywords internal
.prepare_write_data <- function(x, metadata) {
  same_type_lower_bytesize <- metadata$data_type %in%
    c("int8", "int16", "float32")
  lower_bytesize_type <- storage.mode(x) == "double" &&
    metadata$data_type == "float32"

  if (same_type_lower_bytesize || lower_bytesize_type) {
    x <- .truncate_overflow(x, metadata$datatype$nbytes)
  }

  if (metadata$data_type == "bool" && anyNA(x)) {
    warning(
      "Zarr native 'bool' data type does not support NA values. ",
      "NA values will be converted to FALSE. ",
      "To preserve NA values, use 'uint8' datatype in `write_zarr_array()` and `as.logical()` after reading.",
      call. = FALSE
    )
  }

  return(x)
}

.truncate_overflow <- function(x, nbytes) {
  max_value <- 2L^(nbytes * 8L - 1L) - 1L
  min_value <- -max_value - 1L

  positive_overflow <- x > max_value
  negative_overflow <- x < min_value
  if (any(positive_overflow | negative_overflow)) {
    warning(
      "Some values in 'x' are too large to be represented by the ",
      "specified data type. They will be truncated when written.",
      call. = FALSE
    )
    storage.mode(max_value) <- storage.mode(x)
    storage.mode(min_value) <- storage.mode(x)
    x[positive_overflow] <- max_value
    x[negative_overflow] <- min_value
  }
  return(x)
}

.guess_int_size <- function(x, data_type) {
  if (anyNA(x)) {
    # Lower bitsize types cannot represent NA values.
    return("<i4")
  }
  # There is no situation where we may return i8 or u8 because these large integers cannot exist in R.
  # FIXME: potential optimization, avoid checking for overflow later if we used this function.
  max_value <- max(x, na.rm = TRUE)
  min_value <- min(x, na.rm = TRUE)

  if (min_value < 0L) {
    if (min_value >= -128L && max_value <= 127L) {
      return("|i1")
    } else if (min_value >= -32768L && max_value <= 32767L) {
      return("<i2")
    } else {
      return("<i4")
    }
  }
  if (max_value <= 255L) {
    return("|u1")
  } else if (max_value <= 65535L) {
    return("<u2")
  } else {
    return("<u4")
  }
}
