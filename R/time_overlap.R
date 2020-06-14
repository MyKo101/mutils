#' @name time_overlap
#'
#' @title
#' Create a variable based on time overlaps
#'
#' @description
#' Creates a sequence number for each set of overlapping intervals
#'
#' @param start
#' start of the interval. Can be date or numeric, but must be the same as `end`
#'
#' @param end
#' end of the interval. Can be date or numeric, but must be the same as `start`
#'
#' @export
#'
#'
#' @examples
#'
#' tbl <- tibble::tribble(
#'   ~id,          ~t1,          ~t2,
#'   "A", "01/01/2020", "04/01/2020",
#'   "A", "03/01/2020", "15/01/2020",
#'   "A", "01/02/2020", "15/02/2020",
#'   "A", "17/02/2020", "23/02/2020",
#'   "B", "13/02/2020", "18/02/2020",
#'   "B", "16/02/2020", "20/02/2020"
#' ) %>%
#'   dplyr::mutate_at(dplyr::vars(t1, t2), lubridate::dmy)
#'
#' plot(
#'   x = tbl$t1, y = 1:nrow(tbl), col = (tbl$id == "A") + 1,
#'   xlim = range(tbl$t1, tbl$t2)
#' )
#' points(x = tbl$t2, y = 1:nrow(tbl), col = (tbl$id == "A") + 1)
#' for (i in 1:nrow(tbl)) {
#'   lines(x = c(tbl$t1[i], tbl$t2[i]), y = c(i, i), col = (tbl$id[i] == "A") + 1)
#' }
#'
#' tbl$seq <- time_overlap(tbl$t1, tbl$t2)
#'
#' tbl %>%
#'   dplyr::group_by(id) %>%
#'   dplyr::mutate(seq = time_overlap(t1, t2))
time_overlap <- function(start, end) {
  if (length(start) != length(end)) {
    rlang::abort("lengths of start and end must be the same")
  }

  both_date <- lubridate::is.Date(start) && lubridate::is.Date(end)
  both_num <- is.numeric(start) && is.numeric(end)

  if (!both_date && !both_num) {
    rlang::abort("start and end must be dates or numeric")
  }

  df <- data.frame(
    ord = 1:length(start),
    start = start,
    end = end
  )
  df <- df[order(start), ]

  df$prev_end <- dplyr::lag(df$end)

  df$overlap <- df$start <= df$prev_end
  df$overlap[1] <- T
  df$ol_seq <- 1 + cumsum(!df$overlap)

  df <- df[order(df$ord), ]

  return(df$ol_seq)
}
