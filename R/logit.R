#' @name logit
#'
#' @title
#' The logit function
#'
#' @description
#' Calculates the logit (or log-odds) value of a probability
#'
#' @param p
#' vector of probabilities between 0 and 1 (exclusive)
#'
#' @param a
#' a numeric vector
#'
#' @export
#'
#' @examples
#'
#' probs <- runif(100)
#' a <- logit(probs)
#'
#' plot(probs, a, xlim = c(0, 1))
#'
#' probs0 <- unlogit(a)
#'
#' plot(probs, probs0, xlim = c(0, 1), ylim = c(0, 1))
logit <- function(p) {
  if (min(p) <= 0 || max(p) >= 1) {
    stop("p out of range")
  }
  log(p / (1 - p))
}

#' @rdname logit
#' @export
unlogit <- function(a) exp(a) / (exp(a) + 1)
