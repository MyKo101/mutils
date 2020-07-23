#' @name clean_penguins
#'
#' @title
#' Get a clean penguins dataset
#'
#' @description
#' This function generates a cleaner version of the palmerpenguins
#' dataset with no missing values and only 5 variables: `species`,
#' `sex`, `island`, `bill_length` and `bill_depth` (without the
#' units as in the original dataset)
#'
#' @export
#'
clean_penguins <- function() {
  keep_vars <- c(
    "species", "sex", "island",
    "bill_length_mm",
    "bill_depth_mm"
  )
  penguins <- palmerpenguins::penguins[, keep_vars]
  names(penguins) <- gsub("_mm", "", keep_vars, fixed = T)
  penguins[rowSums(is.na(penguins)) == 0, ]
}
