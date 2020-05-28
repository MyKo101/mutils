#' @name update_my_package
#'
#' @title
#' Updates current package
#'
#' @description
#' Wrapper functions to quickly re-install the current package
#' and/or upload it via a `git commit` command (after appropriate checks).
#'
#'
#'
#'
NULL

#' @describeIn update_my_package performs a few functions to quickly and automatically
#' update a package. It performs the following:
#'
#' * `devtools::document()`
#' * `rmarkdown::render("README.Rmd")`
#' * `devtools::install()`
#' * `devtools::load_all()`
#'
#' @param dir
#' directory of the package (or a sub-directory of one).
#' If NULL, the current working directory will be used.
#'
#' @export
#'
reinstall_my_package <- function(dir = ".")
{
  pkg <- devtools::package_file(path=dir)

  devtools::document(pkg)
  README_file.Rmd <- file.path(pkg,"README.Rmd")
  README_file.html <- file.path(pkg,"README.html")

  rmarkdown::render(README_file.Rmd,output_format="github_document")
  if(file.exists(README_file.html)) file.remove(README_file.html)

  devtools::install(pkg,upgrade="never")
  devtools::load_all(pkg)
}

#' @describeIn update_my_package checks if a package will
#' pass `R CMD CHECK` and if it will, uploads it via a git command
#'
#' @param git_message
#' git commit message to be passed to the `git()` function.
#' If no `git_message` is provided, then the update will not be commited. If
#' `git_message` is supplied, then `devtools::check()` will be ran first.
#' `git()` will not be ran if `check()` returns that there is something wrong
#' with the package.
#'
#' @param update_type
#' what level of update version to use.
#' Accepts `"major"`, `"minor"`,`"patch"`, or `"dev"`. Uses `"dev"` as default.
#'
#' @param ignore_notes
#' should the Notes returned by running `devtools::check()` be ignored?
#' (Errors and Warnings are never ignored and should be fixed before upload)
#'
#' @export
#'

update_my_package <- function(git_message = NULL, update_type="dev", ignore_notes=F,dir=".")
{
  pkg <- devtools::package_file(path=dir)

  if(is.null(git_message))
  {
    cat("\nNo git_message supplied. Package will be checked without upload.\n")
  } else
  {
    git_dir <- file.path(pkg,".git")
    if(!file.exists(git_dir))
    {
      rlang::abort("Package is not a git repo. Use git() to set one up?")
    } else {
      cat("\nCurrent git status:\n")
      git("status")
      cat("\nThe above will be commited if check passes\n")
    }
  }

  c_env <- environment()
  c_parent <- parent.env(c_env)
  In_mutils <- rlang::env_label(c_parent) == "namespace:mutils"


  if(!In_mutils)
  {
    cat("\nupdate_my_package() was ran from Global environment.\n")
    cat("\nI will delete myself and re-run from mutils. Goodbye. Be Goood.")

    rm(update_my_package,envir=c_parent)

    mutils::update_my_package(git_message=git_message, update_type=update_type,
                              ignore_notes=ignore_notes, dir=dir)

  } else {


    cat("\nSetting up package documentation\n")
    devtools::document()
    rmarkdown::render("README.Rmd",output_format="github_document")
    if(file.exists("README.html")) file.remove("README.html")

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

    if(any_reports && !is.null(git_message))
    {
      cat("\ndevtools::check() returned reports. Fix these and try again.")
    } else if(!is.null(git_message))
    {
      cat("\nNo report found, so uploading to git")
      tryCatch(current_git <- git("config --get remote.origin.url")[2],
        warning=escalate_warning)

      Match_Version_Github(git_dir=current_git)

      versions <- Update_Version(type=update_type)
      versions_chr <- sapply(versions,paste,collapse=".")
      cat("trying to update from version",versions_chr["old"],
          "to version",versions_chr["new"],"\n")
      tryCatch({
        git_commit <- paste0("commit -a -m \"",git_message,"\"")
        git("add -A",git_commit,"push")
      }, error=function(e) Set_Version(Version=versions$old))
    }
  }

}






