#' @name update_my_package
#'
#' @title
#' Updates current package
#'
#' @description
#' Performs a few functions to quickly and automatically
#' update a package. In general, it performs the following:
#'
#' * `devtools::document()`
#' * `rmarkdown::render("README.Rmd")`
#' * `devtools::install()`
#' * `devtools::load_all()`
#' * `devtools::check()` (if requested)
#' * `git("commit -a <git_message>","push")` (if requested)
#'
#'
#' @param git_message
#' git commit message to be passed to the `git()` function.
#' If no `git_message` is provided, then the update will not be commited. If
#' `git_message` is supplied, then `devtools::check()` will be ran first.
#' `git()` will not be ran if `check()` returns that there is something wrong
#' with the package.
#'
#' @param install
#' should package be re-installed?
#'
#' @param run_check
#' should the package be checked?
#'
#' @param update_type
#' what level of update version to use.
#' Accepts `"major"`, `"minor"`,`"patch"`, or `"dev"`. Uses `"dev"` as default.
#'
#' @param ignore_notes
#' should the Notes returned by running `devtools::check()` be ignored?
#' (Errors and Warnings are never ignored)
#'
#' @export
#'

update_my_package <- function(git_message = NULL, install=T, run_check = F, update_type="dev", ignore_notes=F)
{
  c_env <- environment()
  c_parent <- parent.env(c_env)
  In_Global <- rlang::env_label(c_parent) == "global"

  if(install)
  {
    devtools::document()
    rmarkdown::render("README.Rmd",output_format="github_document")
    if(file.exists("README.html")) file.remove("README.html")
    devtools::install(upgrade="never")
    devtools::load_all()
  }

  if(In_Global)
  {
    rlang::warn(paste0("update_my_package() was ran from Global environment.",
                  " It deleted itself."))
    rm(update_my_package,envir=.GlobalEnv)
    if(run_check || !is.null(git_message))
    {
      rlang::warn(paste0("\nArguments were passed to update_my_package, so it was",
                   " re-ran with the mutils version"))
      mutils::update_my_package(git_message=git_message,install=F,run_check=run_check)
    }

  } else {

    if(run_check || !is.null(git_message))
    {
      check_results <- devtools::check()
      print(check_results)
      if(ignore_notes)
      {
        any_reports <- (length(check_results$errors) >0) ||
          (length(check_results$warnings) >0)
      } else
      {
        any_reports <- (length(check_results$errors) >0) ||
          (length(check_results$warnings) >0) ||
          (length(check_results$notes) >0)

      }
    } else any_reports <- F

    if(!any_reports && !is.null(git_message))
    {
      versions <- Update_Version(type=update_type)
      versions_chr <- sapply(versions,paste,collapse=".")
      cat("trying to update from",versions_chr["old"],
          "to",versions_chr["new"])
      tryCatch({
        git_commit <- paste0("commit -a -m \"",git_message,"\"")
        git(git_commit,"push")
      }, error=Set_Version(Version=versions$old))
    }
  }

}






