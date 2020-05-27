#' @name justify
#'
#' @title
#' Sets alignment of object, ready for printing
#'
#' @description
#' Applies formatting to an object to align it appropriately.
#'
#' @param x
#' object to be justified
#'
#' @param ...
#' further arguments passed to or from other methods
#'
#' @export
#'

justify <- function(x,...)
  UseMethod("justify")

#' @rdname justify
#' @export
#'
#' @description
#' For numerics, strictly rounds to `d` decimal places and
#' lines up the decimal by adding appropriate whitespace. The use of `d`
#' will be taken as the exact number of decimal places. Many other number formatting
#' functions take arguments as "suggestions" only (e.g. `options("digits")`) or
#' use multiple ways of defining length of output
#' (e.g. significant figures and decimal places concurrently)
#'
#' @param d
#' explicit number of decimal places.
#'
#' @examples
#' rnd <- 10^(runif(100,-2,8))
#' rnd <- rnd*runif(100,-1,1)
#' justify(rnd,2)
#'

justify.numeric <- function(x,d=3,...)
{
  if(!is.numeric(x))
    rlang::abort("x must be numeric")

  if(length(d)>1)
  {
    d <- d[1]
    rlang::warn("d (digits) was of length greater than 1, first entry used")
  }

  x <- as.double(x)

  s <- sign(x)
  x0 <- abs(x)
  NAs <- is.na(x)|is.nan(x)
  s[NAs] <- 1

  x_chr_plain <- sprintf("%.f",floor(x0))

  x_chr_comma <- x_chr_plain %>%
    ifelse(NAs,"",.) %>%
    stringi::stri_reverse(.) %>%
    gsub("(.{3})", "\\1,", .) %>%
    gsub(",$","",.) %>%
    stringi::stri_reverse(.) %>%
    ifelse(s==-1,paste0("-",.),.)

  f <- max(nchar(x_chr_comma))

  white_space <- x_chr_comma %>%
    nchar %>%
    magrittr::subtract(f,.) %>%
    sapply(strrep,x = " ")

  x_f <- paste0(white_space,x_chr_comma)


  if(d > 0)
  {
    infs <- is.infinite(x)

    x_d <- x0 %>%
      ifelse(infs,0,.) %>%
      magrittr::mod(1) %>%
      magrittr::multiply_by(10^d) %>%
      round() %>%
      justify(d=0) %>%
      gsub(" ","0",.) %>%
      paste0(".",.) %>%
      ifelse(infs|NAs,strrep(" ",d+1),.)

    res <- paste0(x_f,x_d)

  } else
  {
    res <- x_f
  }

  return(res)
}

#' @rdname justify
#' @export
#'
#' @param align
#' `"left"`, `"right"` or `"centre"` (or just `"l"`, `"r"` or `"c"`)
#' to indicate which alignment to apply.
#'
#' @param na.rm
#' should `NA`s (or `NaN`s) be kept
#'
#' @examples
#' set.seed(1)
#' all_cols <- colours()
#' all_cols <- unique(gsub("[0-9]","",all_cols))
#' smp_cols <- sample(all_cols,20)
#' justify(smp_cols,align="centre")
#'
#'

justify.character <- function(x,align = "left",na.rm=TRUE,...)
{
  align <- tolower(substr(align,1,1))


  if(na.rm)
  {
    x[is.na(x)|is.nan(x)] <- ""
  } else
  {
    x[is.na(x)|is.nan(x)] <- "NA"
  }


  width <- max(nchar(x),na.rm=TRUE)
  extra_chars <- width - nchar(x)



  if(align == "l")
  {
    res <- paste0(x,strrep(" ",extra_chars))
  } else if(align == "r")
  {
    res <- paste0(strrep(" ",extra_chars),x)
  } else if(align=="c")
  {
    left_chars <- floor(extra_chars/2)
    right_chars <- extra_chars - left_chars
    res <- paste0(strrep(" ",left_chars),x,
                  strrep(" ",right_chars))
  }
  res

}

#' @rdname justify
#' @export
#'
#' @param form
#' should logical be printed in `"long"` format (`"TRUE"` and `"FALSE"`),
#' `"short"` format (`"T"` or `"F"`) or
#' `"numeric"` format (`"1"` or `"0"`)
#'
#' @param case
#' should output be `"upper"` (e.g. `"TRUE"`),
#' `"lower"` (e.g. `"false"`) or `"capitalised"` (e.g. `"True"`)
#'
#'
#' @examples
#'
#' x <- c(TRUE,FALSE,NA)
#'
#' justify(x)
#' justify(x,"long","lower","right")
#' justify(x,form="long",align="centre")
#' justify(x,form="numeric")
#' justify(x,na.rm=TRUE)
#'
#'

justify.logical <- function(x,form="short",case="upper",align="left",na.rm=TRUE,...)
{
  form <- tolower(substring(form,1,1))
  case <- tolower(substring(case,1,1))
  align <- tolower(substring(align,1,1))

  if(form == "s")
  {
    align <- "l"
    if(case == "c") case <- "u"
  } else if(form == "n")
  {
    case <- "u"
    align <- "l"
  }

  lookup <- tibble::tribble(
    ~form, ~case, ~align,    ~tru,    ~fal,      ~N,
      "s",   "u",    "l",     "T",     "F",     "N",
      "s",   "l",    "l",     "t",     "f",     "n",
      "l",   "u",    "l", "TRUE ", "FALSE", "NA   ",
      "l",   "u",    "r", " TRUE", "FALSE", "   NA",
      "l",   "u",    "c", "TRUE ", "FALSE", " NA  ",
      "l",   "l",    "l", "true ", "false", "na   ",
      "l",   "l",    "r", " true", "false", "   na",
      "l",   "l",    "c", "true ", "false", " na  ",
      "l",   "c",    "l", "True ", "False", "Na   ",
      "l",   "c",    "r", " True", "False", "   Na",
      "l",   "c",    "r", "True ", "False", " Na  ",
      "n",   "u",    "l",     "1",     "0",     " "
  )

  set <- lookup %>%
    dplyr::filter(.data$form == .env$form &
                    .data$case == .env$case &
                    .data$align == .env$align)

  if(na.rm==T) set$N <- strrep(" ",nchar(set$tru))

  y <- rep("",length(x))
  y[is.na(x)|is.nan(x)] <- set$N
  y[x] <- set$tru
  y[!x] <- set$fal

  y

}














