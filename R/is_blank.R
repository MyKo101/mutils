#' @name is_blank
#'
#' @title
#' Checks if a string is blank
#'
#' @description
#' Checks if a character string is made up of only spaces
#'
#' @param x
#' character vector to be checked for blank-ness
#'
#' @export
#'
#' @examples
#' x <- c("red","blue green","  "," ","","yellow")
#' is_blank(x)
#'

is_blank <- function(x)
{
  if(!is.character(x))
    rlang::abort("x needs to be of type character")
  y <- gsub(" ","",x)
  nchar(y) == 0
}
