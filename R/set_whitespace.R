#' @name set_whitespace
#'
#' @title
#' Set whitespace in character
#'
#' @description
#' When printing whitespace, some applications will
#' ignore or remove the whitespace, for example it will
#' simplify `"  "` to `" "`. In this instance, it will be
#' useful to replace the double whitespace with a space
#' preceding by `"symbol"` (or the other way round conditionally).
#'
#' @param x
#' vector of character strings to set the whitespace for
#'
#' @param symbol
#' character string to be used as the whitespace.
#' In general, for HTML output (default), use `"&emsp;"` and
#' for LaTeX, use `"\\\\quad"` (the double escape is required)
#'
#' @export
#'
#' @examples
#' x <- c("  a", "b  ", "  c  ")
#' set_whitespace(x)
set_whitespace <- function(x, symbol = "&emsp;") {
  if (!is.character(x)) {
    rlang::abort("x needs to be a character in set_whitespace()")
  }
  if (!is.character(symbol) || length(symbol) != 1) {
    rlang::abort("symbol needs to be a single character")
  }

  symbol2 <- paste0(symbol, " ")
  symbol2_rev <- stringi::stri_reverse(symbol2)

  x_rev <- stringi::stri_reverse(x)
  x_rev_ws <- gsub("  ", symbol2_rev, x_rev)

  x_ws <- stringi::stri_reverse(x_rev_ws)

  return(x_ws)
}
