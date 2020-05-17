#' @name table
#'
#' @title
#' Cross Tabulation and Table Creation in pipes
#'
#' @description
#' Wraps the base package's \code{\link[base]{table}} function to allow users
#' to use this within a pipeline.
#'
#' @param x
#' data within which the \code{\link[base]{table}} is
#' to be evaluated
#'
#' @param ...
#' arguments to be passed to \code{\link[base]{table}}
#'
#' @examples
#' tbl <- data.frame(x=c("a","a","b","a","a","b","b","b","a"),
#'                   y=c("c","d","c","d","c","d","c","d","c"))
#' table(tbl$x,tbl$y)
#' table(tbl,x,y)
#' tbl %>% table(x,y)
#'

table <- function(x,...)
{
  .call <- sys.call()
  .call[[1]] <- quote(base::table)
  if(is.list(x))
  {
    .call[[2]] <- NULL
    res <- eval(.call,x)
  } else {
    res <- eval(.call,parent.frame())
  }

  return(res)
}
