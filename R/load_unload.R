#' @name load_unload
#'
#' @title
#' Load or Unload packages
#'
#' @description
#' \code{load_packages} checks if a vector of packages are installed, if not, installs them.
#' Then loads them ready for use. \code{unload_packages} unloads packages, see \code{\link{detach}}
#'
#' @param ...
#' list of packages to be passed loaded or unloaded
#'
#' @export
#'
#' @examples
#' load_packages(magrittr, purrr, stringi, lubridate)
#' unload_packages(magrittr, purrr)
#'
#' reload_packages(stringi, lubridate)
load_packages <- function(...) {
  quo_pkgs <- as.list(enquos(...))
  pkgs <- vapply(quo_pkgs, as_name, character(1))
  already_installed_list <- rownames(utils::installed.packages())
  already_installed <- pkgs %in% already_installed_list

  if (any(!already_installed)) {
    tryCatch(utils::install.packages(pkgs[!already_installed]),
      error = utils::install.packages(pkgs[!already_installed], repos = getOption("repos"))
    )
  }

  res <- lapply(pkgs, library, character.only = T, warn.conflicts = F)
}

#' @rdname load_unload
#' @export

unload_packages <- function(...) {
  quo_pkgs <- as.list(enquos(...))
  pkgs <- vapply(quo_pkgs, as_name, character(1))
  pkgs <- paste0("package:", pkgs)
  pkgs <- pkgs[pkgs %in% search()]

  res <- invisible(lapply(pkgs, detach, character.only = T, unload = T))
}


#' @rdname load_unload
#' @export
reload_packages <- function(...) {
  unload_packages(...)
  load_packages(...)
}
