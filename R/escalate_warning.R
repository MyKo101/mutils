#' @name escalate_warning
#'
#' @title
#' Escalate a warning message to an Error message
#'
#' @description
#' Uses the `rlang::abort()` function to escalate
#' a warning message into an Error to prevent code from
#' running any further.
#'
#' @param w
#' a warning object (which has a message inside it)
#'
#' @export
#'

escalate_warning <- function(w) {
  rlang::abort(paste0("\rError Escalated from Warning:\n", w$message))
}
