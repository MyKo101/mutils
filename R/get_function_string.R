#' @name get_function_string
#'
#' @title
#' Simplify a function to a string
#'
#' @description
#' Extracts the details of a function as either it's name, or
#' (if anonymous), some minimal details about it.
#'
#' @param f
#' function to be coerced to string
#'
#' @export
#'
#' @examples
#' f <- function(x) x+1
#'
#' get_function_string(f)
#' get_function_string(
#'   function(x) x+1
#' )
#' get_function_string(
#'   function(x) mean(1:10)
#' )
#' get_function_string(
#'   function(x) stats::mean(1:10)
#' )
#' get_function_string(
#'   function(x) (x+1)
#' )
#' get_function_string(
#'   function(x) (3*x+1)/2
#' )
#' get_function_string(
#'   function(x){
#'   x+2
#' })
#' get_function_string(
#'   function(x){
#'   y <- x+2
#'   x+y
#' })
#' get_function_string(
#'   function(x){
#'   y <- x+2
#'   x+y
#'   y^2
#' })
#'
#'
get_function_string <- function(f){
  if(!rlang::is_function(f))
    glue_abort("{f} must be a function")
  sub_f <- substitute(f)
  if(is.symbol(sub_f)){
    rlang::as_label(sub_f)
  } else {
    body_f <- body(f)
    if(body_f[[1]] == "{") {
      if(length(body_f) == 1) {
        "{ }"
      } else if(length(body_f) == 2) {
        paste0("{ ",
               rlang::as_label(body_f[[2]]),
               " }")
      } else if(length(body_f) == 3) {
        paste0("{ ",
               rlang::as_label(body_f[[2]]),"; ",
               rlang::as_label(body_f[[3]]),
               " }")
      } else {
        paste0("{ ",
               rlang::as_label(body_f[[2]]),
               "; ... }")
      }
    } else {
      rlang::as_label(body(f))
    }
  }
}
