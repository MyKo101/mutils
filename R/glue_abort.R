#' @name glue_abort
#'
#' @title
#' Signal an error or warning using glue
#'
#' @description
#' These functions act as wrappers around `rlang::abort()` and
#' `rlang::warn()` to allow the `glue`-style insertion of variables
#' names. By default, object names are parsed via `substitute()`,
#' rather than evaluated, meaning that the objects do not need to be
#' coerced to strings. This is useful inside functions to grab the
#' variable name passed by the user
#'
#' @param msg
#' Message to be passed as either an error or a warning
#'
#' @param sub
#' should the `substitute()` function be wrapped around the elements
#' of `msg`
#'
#' @param ...
#' additional arguments to be passed to `rlang::abort()` or `rlang::warn()`
#'
#' @export
#'
#' @examples
#' f <- function(input_argument)
#' {
#'   glue_abort("Error is with {input_argument}")
#' }
#'
#' x <- "Show My Name, Not My String"
#' \dontrun{
#' f(x)
#' f("Show Me")
#' }
#'
#'
#'

glue_abort <- function(msg,...,sub=T){
  if(sub)
    msg <- gsub("\\{(.*?)\\}","{rlang::as_label(substitute(\\1))}",msg)
  rlang::abort(glue::glue(msg,.envir=rlang::caller_env()),...)
}

#' @rdname glue_abort
#' @export

glue_warn <- function(msg,...,sub=T){
  if(sub)
    msg <- gsub("\\{(.*?)\\}","{rlang::as_label(substitute(\\1))}",msg)
  rlang::warn(glue::glue(msg,.envir=rlang::caller_env()),...)
}
