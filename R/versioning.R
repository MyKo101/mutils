#' @name versioning
#'
#' @title
#' Functions for versioning of packages
#'
#' @description
#' Performs functions surrounding versioning within packages.
#' * `Get_Version(dir)` will extract the current version from the
#' `DESCRIPTION` file in `dir`.
#'
#' @param dir
#' directory containing the `DESCRIPTION` file. Can also be
#' a web address, such as a github repo
#'
#' @param type
#' accepts `"major"`, `"minor"`, `"patch"`, `"dev"` or `"none"`
#' to describe what kind of update to perform. whichever
#' type is supplied will be the level that is incremented.
#' All lower levels will be reset to 0 (or 9000 for dev).
#'
#' @param Version
#' an appropriate vector for version numbers. Must be of
#' length 4 and the elements mst be named
#' `"major"`, `"minor"`, `"patch"` and `"dev"`, respectively
#' @export

Get_Version <- function(dir = ".") {
  Desc_file <- readLines(file.path(dir, "DESCRIPTION"))
  Version_row <- grep("^Version:", Desc_file)
  Version_raw <- Desc_file[Version_row]
  Version <- strsplit(Version_raw, split = "[\\W]", perl = T)[[1]][-(1:2)]
  Version <- as.numeric(Version)
  names(Version) <- c("major", "minor", "patch", "dev")
  Version
}

#' @rdname versioning
#'
#' @description
#' * `Set_Version(dir,Version)` will update the `DESCRIPTION` file in `dir`.
#' @export
Set_Version <- function(dir = ".", Version) {
  Check_Version(Version)
  Version <- paste0(Version, collapse = ".")

  if (file.exists(file.path(dir, "DESCRIPTION"))) {
    Date_today <- paste0(Sys.Date())

    Desc_file <- readLines("DESCRIPTION")
    Desc_file <- gsub("(^Version: ).*?($)", paste0("\\1", Version), Desc_file)
    Desc_file <- gsub("(^Date: ).*?($)", paste0("\\1", Date_today), Desc_file)

    writeLines(Desc_file, file.path(dir, "DESCRIPTION"))
  }

  if (file.exists(file.path(dir, "README.Rmd"))) {
    New_Badge <- paste0("https://img.shields.io/badge/Version-", Version, "-orange.svg")

    README_file <- readLines("README.Rmd")
    README_file <- gsub(
      "(\\s*?\\[!\\[Version Badge\\]\\().*?(\\)\\]\\(.*?\\))",
      paste0("\\1", New_Badge, "\\2"), README_file
    )

    writeLines(README_file, file.path(dir, "README.Rmd"))
  }
}

#' @rdname versioning
#'
#' @description
#' * `Add_Version(Version,type)` will increment the version based
#' on the level provided (major, minor, patch or dev).
#' @export
Add_Version <- function(Version, type) {
  Check_Version(Version)
  if (!type %in% c("major", "minor", "patch", "dev", "none")) {
    rlang::abort("type is not major, minor, dev or patch")
  } else if (type == "major") {
    Version["major"] <- Version["major"] + 1
    Version[c("minor", "patch")] <- 0
    Version["dev"] <- 9000
  } else if (type == "minor") {
    Version["minor"] <- Version["minor"] + 1
    Version["patch"] <- 0
    Version["dev"] <- 9000
  } else if (type == "patch") {
    Version["patch"] <- Version["patch"] + 1
    Version["dev"] <- 9000
  } else if (type == "dev") {
    Version["dev"] <- Version["dev"] + 1
  }

  Version
}

#' @rdname versioning
#'
#' @description
#' * `Update_Version(type)` Gets, Adds and Sets a new Version Update
#' based on the `type` of the update.
#' @export
Update_Version <- function(dir = ".", type = "dev") {
  Version_old <- Get_Version(dir = dir)
  Check_Version(Version = Version_old)
  Version_new <- Add_Version(Version = Version_old, type = type)
  Check_Version(Version = Version_new)
  Set_Version(dir = dir, Version = Version_new)
  list(
    new = Version_new,
    old = Version_old
  )
}

#' @rdname versioning
#'
#' @description
#' * `Check_Version(Version)` Checks if `Version` is appropriately named version.
#' @export
Check_Version <- function(Version) {
  if (length(Version) != 4) {
    rlang::abort("Version must be of length 4")
  }
  if (!all(names(Version) == c("major", "minor", "patch", "dev"))) {
    rlang::abort("Version be appropriately named: major, minor, patch, dev")
  }
}

#' @rdname versioning
#'
#' @param user
#' username for Github lookup
#'
#' @param repo
#' repository for Github lookup. If NULL, will extract the package name
#' from the `DESCRIPTION` file
#'
#' @param ref
#' the branch to use as a lookup. Defaults to `"master"`
#'
#' @param git_dir
#' directory for the github repo. Can be found by using
#'
#'
#' @description
#' * `Github_Version_match(user,repo,ref,dir)` extracts the current
#' version of the github repo, and sets the current version to match.
#' Looks for `DESCRIPTION` at the address:`"https://raw.githubusercontent.com/<user>/<repo>/<ref>/"`
#'
#' @export
Match_Version_Github <- function(user = NULL, repo = NULL, ref = "master", git_dir = NULL, dir = ".") {
  if (is.null(user) & is.null(git_dir)) {
    rlang::abort("Either git_dir or user must be supplied")
  } else if (is.null(git_dir)) {
    if (is.null(repo)) {
      Desc_file <- readLines(file.path(dir, "DESCRIPTION"))
      Package_row <- grep("^Package:", Desc_file)
      Package_raw <- Desc_file[Package_row]
      Package_name <- trimws(gsub("^Package: ", "", Package_raw))
      repo <- Package_name
    }
  } else {
    git_dir_split <- strsplit(git_dir, "[/.]")[[1]]
    user <- git_dir_split[5]
    repo <- git_dir_split[6]
    if (length(git_dir_split) == 9) {
      ref <- git_dir_split[8]
    } else {
      ref <- "master"
    }
  }

  basedir <- "https://raw.githubusercontent.com"
  gh_dir <- paste(basedir, user, repo, ref, sep = "/")

  GH_Version <- Get_Version(gh_dir)
  Set_Version(dir = dir, Version = GH_Version)
  invisible(GH_Version)
}
