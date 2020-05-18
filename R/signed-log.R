#' @name signed-log
#'
#' @title
#' Calculates the signed log of a value
#'
#' @description
#' The signed logarithm allows a pseudo-logarithmic re-scaling to
#' be performed on data that is both positive and negativeis. It defined as:
#'
#' \deqn{ \textrm{signed_log}_b(x) = \textrm{sign}(x)\log_b(|x| + 1)}
#'
#'
#' @param x
#' a numeric vector.
#'
#' @param base
#' a positive number
#'
#' @export
#'
#' @examples
#' x <- seq(-10,10,0.01)
#' y <- signed_log(x)
#'
#' plot(x,y,type="l")
#'
#' x0 <- unsigned_log(y)
#'
#' plot(y,x0,type="l")
#'
#' plot(x,x0,type="l")

signed_log <- function(x,base=exp(1))
{
  if(is.complex(x) || is.complex(base))
    stop("unimplemented for complex numbers")
  log(abs(x) + 1,base=base)*sign(x)
}

#' @rdname signed-log
#' @export

unsigned_log <- function(x,base=exp(1))
{
  if(is.complex(x) || is.complex(base))
    stop("unimplemented for complex numbers")
  sign(x)*(exp(log(base)*abs(x)) - 1)
}
