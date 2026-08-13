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

# Backport from R 4.6.0
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

# Backport from R 4.7.0
Partial <- function(f, ...) {
  f <- match.fun(f)
  args <- list(...)
  function(...) {
    do.call(f, c(args, list(...)))
  }
}

Partial_with_splicing <- function(f, ...) {
  f <- match.fun(f)
  args <- rlang::list2(...)
  function(...) {
    do.call(f, c(args, list(...)))
  }
}
