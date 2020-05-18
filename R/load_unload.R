#' @name load_unload
#'
#' @title
#' Load or Unload packages
#'
#' @description
#' \code{load_packages} checks if a vector of packages are installed, if not, installs them.
#' Then loads them ready for use. \code{unload_packages} unloads packages, see \code{\link{detach}}
#'
#' @param pkgs
#' character list of package names
#'
#' @param ...
#' additional arguments to be passed to \code{library} or \code{detach}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_packages(c("dplyr","tibble"))
#' unload_packages(c("dplyr","tibble"))
#' }
#'

load_packages <- function(pkgs,...)
{
  requireNamespace("purrr")
  already_installed_list <- rownames(utils::installed.packages())
  already_installed <- pkgs %in% already_installed_list

  if(any(!already_installed))
    utils::install.packages(pkgs[!already_installed])

  purrr::walk(pkgs,library,character.only=T,...)

}

#' @rdname load_unload
#' @export

unload_packages <- function(pkgs,...)
{
  requireNamespace("purrr")
  pkgs <- paste0("package:",pkgs)

  purrr::walk(pkgs,detach,character.only=T,unload=T,...)

}





