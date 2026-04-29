# Custom linters

#' Linter to enforce simplifyVector = FALSE in fromJSON calls
#'
#' @return A linter function to be used with \code{lintr::lint()}.
#' @noRd
from_json_simplify_linter <- lintr::make_linter_from_function_xpath(
  function_names = "fromJSON",
  xpath = "parent::expr[not(SYMBOL_SUB[
    text() = 'simplifyVector' and
    following-sibling::expr[1]/NUM_CONST[text() = 'FALSE']
  ])]",
  lint_message = "fromJSON() should always be called with simplifyVector = FALSE.",
  type = "warning"
)
