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
    lapply(1:n,function(...) x)
  else
    NULL
}

#' @rdname rep_list
#' @export
#'
NULL_list <- function(n)
{
  rep_list(NULL,n)
}
