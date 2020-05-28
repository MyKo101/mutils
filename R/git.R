#' @name git
#'
#' @title
#' Git in terminal
#'
#' @description
#' Shortcut to using `git` in terminal.
#'
#' @param ...
#' git argument(s)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' git("add -A")
#' git("commit -m 'Commit Message'")
#' git("push")
#' }
#'
#' \dontrun{
#' git("add -A","commit -m 'Commit Message'","push")
#' }
#'

git <- function(...)
{
  args <- rlang::list2(...)
  res <- NULL
  for(c_arg in args)
  {
    git_message <- paste0("git ",c_arg)
    cres <- system(git_message,intern=T)

    cat("\n$",git_message,"\n")
    cat(paste0(cres,collapse="\n"))
    cat("\n")

    res <- c(paste0("$ ",git_message),cres)

  }

  invisible(res)
}
