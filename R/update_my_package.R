#' @name update_my_package
#'
#' @title
#' Updates current package
#'
#' @description
#' Performs a few functions to quickly and automatically
#' update a package. Only :
#'
#' * \code{devtools::document()}
#' * \code{rmarkdown::render("README.Rmd")}
#' * \code{devtools::install()}
#' * \code{devtools::load_all()}
#' * \code{devtools::check()} (if requested)
#' * \code{git(message)} (if requested)
#'
#' @param git_message
#' optional. git commit message to be passed to the \code{\link{git}()} function.
#' If no \code{message} is provided, then the \code{\link{git}()} function will
#' not be ran
#'
#' @param run_check
#' logical. Should the package be checked?
#'
#' @export
#'

update_my_package <- function(git_message = NULL,run_check = F)
{
  devtools::document()
  rmarkdown::render("README.Rmd",output_format="github_document")
  if(file.exists("README.html")) file.remove("README.html")
  devtools::install(upgrade="never")
  devtools::load_all()
  if(run_check)
  {
    check_results <- devtools::check()
    print(check_results)
    any_reports <- (length(check_results$errors) >0) ||
      (length(check_results$warnings) >0) ||
      (length(check_results$notes) >0)
  }else any_reports <- F

  if(!any_reports && !is.null(git_message))
    git(git_message)
}
