#' @name rep_list
#'
#' @title
#' Replicate objects into a list
#'
#' @description
#' Creates a list of length `n` containing `x`
#' as every element.
#'
#' @param x
#' value to be replicated
#'
#' @param n
#' number of replications
#'
#' @export

rep_list <- function(x,n)
{
  if(n > 0)
  {
    res <- vector("list",n)
    res[1:n] <- x
  } else
  {
    res <- NULL
  }

  res
}
