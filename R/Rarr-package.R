#' @useDynLib Rarr
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecate_warn
## usethis namespace: end
NULL

# Backport from R 4.4.0
"%||%" <- function(x, y) {
  if (is.null(x)) y else x # nolint: coalesce_linter.
}

# Backport from R 4.6.0
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0
}
