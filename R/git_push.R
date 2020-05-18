#' @name git_push
#'
#' @title
#' Git Commit & Push
#'
#' @description
#' Shortcut to commit and push to a git repo. It passes
#'  \code{git commit -a -m "message"} to the terminal, followed by
#'  \code{git push}. This should only be used after repos have been set up
#'
#' @export
#'
#' @param message
#' git commit message
#'

git <- function(message)
{
  git_commit <- paste0("git commit -a -m \"",message,"\"")
  git_push <- "git push"
  system(git_commit)
  system(git_push)
}
