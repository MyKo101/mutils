#' @name dot_arg
#'
#' @title
#' Check whether a call has dots for arguments
#'
#' @description
#' Looks at arguments within a call and checks whether
#' they consist of a `.` argument, or an attempt to subset
#' a `.` argument (e.g. `.$x` or `.[[1]]`).
#'
#' @param call
#' a call to check for `.` arguments
#'
#' @param arg
#' an argment within a call to check for either `.` arguments or
#' for subsettings applied directly to a `.` argument
#' (e.g. `.$x`, `.[1]` or `.[[1]]` will trigger a `TRUE` result).
#'
#' @export
#'
#' @examples
#'
#' has_dot_arg(quote(length(.)))
#'
#' has_dot_arg(quote(length(x)))
#'
#' has_dot_arg(quote(length(.$x)))
#' has_dot_arg(quote(length(.[[1]])))

has_dot_arg <- function(call)
{
  length(call)>1 &&
    any(vapply(call[-1],is_dot_arg,logical(1)))
}

#' @rdname dot_arg
#' @export
is_dot_arg <- function(arg)
{
  identical(arg,quote(.)) ||
    (length(arg)>1 &&
       (identical(arg[[1]],quote(`$`)) ||
          identical(arg[[1]],quote(`[`)) ||
          identical(arg[[1]],quote(`[[`))) &&
       identical(arg[[2]],quote(.)))
}
