#' @name get-age
#'
#' @title
#' Calculates ages
#'
#' @description
#' Calculates the age of individuals at a given date,
#' based on their date of birth. This uses \code{\link{lubridate}} functions
#' to calculate an age, which means that leap years are also taken into account.
#'
#' @param DOB
#' vector of \code{Date} objects to be used as the date of
#' birth
#'
#' @param Date
#' vector of \code{Date} objects to be used as the date at
#' which the age is requested
#'
#' @examples
#' birthdays <- lubridate::dmy(c("01/01/1990","29/02/1992","31/12/2000","15/06/1995"))
#' get_age(birthdays,today())
#'

get_age <- function(DOB,Date,leap_March = T)
{
  requireNamespace("lubridate",quietly=T)
  dT.int <- lubridate::interval(DOB,Date)
  dT.length <- lubridate::time_length(dT.int,unit="year")
  return(floor(dT.length))
}





















