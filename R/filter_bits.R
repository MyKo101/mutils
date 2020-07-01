#' @name filter_bits
#'
#' @title
#' Filtering functions
#'
#' @description
#' A few additional functions to facilitate filtering of vectors
#'
#' @param x
#' vector of numerics to be filtered
#'
#' @examples
#' x <- seq(-3, 3, 0.5)
NULL

#' @rdname filter_bits
#' @export
#' @examples
#' drop_zeros(x)
drop_zeros <- function(x) x[x != 0]

#' @rdname filter_bits
#' @export
#' @examples
#' positives(x)
positives <- function(x) x[x > 0]

#' @rdname filter_bits
#' @export
#' @examples
#' negatives(x)
negatives <- function(x) x[x < 0]

#' @rdname filter_bits
#' @export
#' @examples
#' integers(x)
integers <- function(x) x[x %% 1 == 0]
