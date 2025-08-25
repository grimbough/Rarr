#' @useDynLib Rarr
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Backport from R 4.4.0
"%||%" <- function(x, y) {
  if (is.null(x)) y else x # nolint: coalesce_linter.
}
