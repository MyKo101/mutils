#' @name signed-log
#'
#' @title
#' Calculates the signed log of a value
#'
#' @description
#' The standard logarithm (\code{\link[base]{log}}) can takes positive values
#' as inputs and rescales values that are heavily clustered around low values
#' (with outliers) and thus helps to Normalise some skewed data.
#' However, if your data is clustered around zero and also contains both positive and
#' negative values, the negative values cannot be passed into a logarithm
#' function. The `signed_log` function will adjust the values slightly to
#' maintain both the logarithmic-spread of the distribution and the sign.
#'
#' \deqn{ \textrm{signed_log}_b(x) = \textrm{sign}(x)\log_b(|x| + 1)}
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
