#' @name has_names
#'
#' @title
#' Does an object have names?
#'
#' @description
#' Checks to see if object x has \code{names}
#' assciated with it
#'
#' @param x
#' object to be checked
#'
#' @export
#'
#' @examples
#' has_names(3)
#' has_names(c(y=3))
#'
#' iris2 <- rlang::set_names(iris,NULL)
#' has_names(iris)
#' has_names(iris2)
#'
#'

has_names <- function(x)
{
  !is.null(attr(x,"names"))
}
