#' @name eval_expr
#'
#' @title
#' Evaluate an expression within context
#'
#' @description
#' Uses \code{eval_tidy()} to evaluate an expression
#' within the context of \code{data}.
#'
#' @param expr
#' expression to evaluate. Can be a quosure, as with
#' \code{eval_tidy()} or a function to be applied to \code{data}.
#'
#' @param data
#' A data frame or named list or vector to be passed to
#' \code{eval_tidy}, or an argment to be passed to \code{expr}
#'
#' @export
#'
#' @examples
#'
#' tbl <- tibble::tibble(x=1:5)
#' eval_expr(mean(x),tbl)
#' eval_expr(. %>% dplyr::pull(x) %>% mean,tbl)
#'
#'


eval_expr <- function(expr,data=NULL)
{
  .expr <- rlang::enquo(expr)
  expr_res <- if(has_names(data))
  {
    rlang::eval_tidy(.expr,data)
  } else
  {
    rlang::eval_tidy(.expr)
  }

  if(is.function(expr_res))
  {
    expr_res <- expr_res(data)
  }

  expr_res
}
