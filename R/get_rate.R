#' @name get_rate
#'
#' @title
#' Calculate Rate of Change
#'
#' @description
#' Calculates the rate of change of a value, provided a time variable.
#' \code{time} and \code{units} should be compatible with the \code{\link{lubridate}} package
#'
#' @param x
#' value to calculate the rate of
#'
#' @param time
#' date or time variable to be used in the calculations of rate
#'
#' @param unit
#' units of time to be used in output
#'
#' @param historic
#' accepts \code{"backward"}, \code{"forward"} or \code{"average"} as possible values.
#' \code{"backward"} means look backwards for the rate, in this case the first
#' observation will have a rate of \code{NA}. Similarly, \code{"forward"} means look
#' forward and so the last observation will have a rate of \code{NA}. \code{"average"}
#' will take the average of these two measurements and return no \code{NA}s
#'
#' @export
#'
#' @examples
#' x <- c(4, 2, 1, 4, 6, 7, 4, 2, 2, 9)
#' time <- lubridate::ymd(c(
#'   "2000-08-23", "2000-04-19", "2000-10-12", "2000-07-22", "2000-12-13",
#'   "2000-06-20", "2000-01-02", "2000-11-12", "2000-03-11", "2000-08-04"
#' ))
#' get_rate(x, time)
#' \dontrun{
#' df <- tibble::tibble(x, time) %>%
#'   dplyr::mutate(x.rate = get_rate(x, time, historic = "average"))
#' }
#'
get_rate <- function(x, time, unit = "days", historic = "backward") {
  requireNamespace("lubridate", quietly = T)

  len.x <- length(x)

  if (len.x != length(time)) {
    stop("lengths of x & time must be equal")
  }
  if (length(unique(time)) < length(time)) {
    stop("duplicate times detected")
  }

  id <- 1:len.x

  id <- id[order(time)]
  x <- x[order(time)]
  time <- time[order(time)]


  dX <- x[-1] - x[-len.x]

  dT.int <- lubridate::interval(time[-len.x], time[-1])
  dT <- lubridate::as.duration(dT.int) / lubridate::duration(1, unit)

  dX_dT <- dX / dT

  historic.ref <- substring(historic, 1, 1)

  if (historic.ref == "b") {
    dX_dT2 <- c(NA, dX_dT)
  } else if (historic.ref == "f") {
    dX_dT2 <- c(dX_dT, NA)
  } else if (historic.ref == "a") {
    dX_dT2 <- c(
      dX_dT[1],
      (dX_dT[-1] + dX_dT[-(len.x - 1)]) / 2,
      dX_dT[len.x - 1]
    )
  }

  dX_dT2 <- dX_dT2[order(id)]

  return(dX_dT2)
}
