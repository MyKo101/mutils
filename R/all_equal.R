#' @name all_equal
#'
#' @title
#' Checks if all arguments are equal
#'
#' @description
#' Uses `identical()` (or some other comparison function) to check if
#' every argument is equal to one another.
#'
#' @param ...
#' arguments to be compared
#'
#' @param .check
#' alternative function to be used to compare the arguments
#'
#' @export
#'
#' @examples
#'
#' df <- tibble::tibble(x = runif(10), y = runif(10))
#' df2 <- df
#' df3 <- df
#' df4 <- df
#'
#' # To ensure the three variables have been copied
#' df$z <- 1
#' df2$z <- 1
#' df3$z <- 1
#' df4$z <- 2
#'
#' all_equal(df, df2, df3)
#' all_equal(df, df2, df4)
#'
#' new_df <- check_all_equal(df, df2, df3)
#' \dontrun{
#' check_all_equal(df, df4)
#' }
#'
#'
#' x <- 1
#' y <- 1
#' z <- 1
#'
#' all_equal(x, y, z, .check = magrittr::equals)
#'
#' w <- check_all_equal(x, y, z, .check = magrittr::equals)
#' \dontrun{
#' check_all_equal(x, y, z, 2, .check = magrittr::equals)
#' check_all_equal(x, 2, .check = magrittr::equals)
#' }
#'
all_equal <- function(..., .check = identical) {
  args <- list(...)
  if (length(args) < 2) {
    TRUE
  } else {
    c_comparison <- TRUE
    i <- 2
    while (c_comparison & i <= length(args)) {
      c_compare <- .check(args[[i - 1]], args[[i]])
      i <- i + 1
    }
    c_compare
  }
}

#' @rdname all_equal
#' @export

check_all_equal <- function(..., .check = identical) {
  .dots <- enquos(...)

  if (all_equal(..., .check = .check)) {
    invisible(rlang::eval_tidy(.dots[[1]]))
  } else {
    length_dots <- length(.dots)
    d1 <- .dots[[1]]
    d2 <- .dots[[2]]
    if (length_dots == 2) {
      error_msg <- "{d1} and {d2} are not equal by {.check}()"
    } else if (length_dots == 3) {
      d3 <- .dots[[3]]
      error_msg <- "Not all of {d1}, {d2} and {d3} are equal by {.check}()"
    } else {
      error_msg <- "Not all of {d1}, {d2}, etc... are equal by {.check}()"
    }
    glue_abort(error_msg)
  }
}
