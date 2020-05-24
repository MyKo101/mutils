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
  for(c_arg in args)
  {
    git_message <- paste0("git ",c_arg)
    system(git_message)
  }

}
