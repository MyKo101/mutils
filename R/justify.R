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
#' @description
#' For characters, inserts a `newline` between words when
#' the text overflows above the `width`. Whitespace is used to
#' fill in space around text to ensure correct alignment.
#' Note that in this context, only spaces, `" "`, can separate
#' words. This means that, in general, punctuation is considered to
#' be part of the word before it (e.g. `"test."` is a
#' five-letter word and `"five-letter"` is an eleven-letter word)
#'
#' @param width
#' number of characters before a `newline` is inserted.
#'
#' @param align
#' `"left"`, `"right"` or `"centre"` (or just `"l"`, `"r"` or `"c"`)
#' to indicate which alignment to apply.
#'
#' @param newline
#' character string to insert as a newline notation.
#'
#' @examples
#' set.seed(1)
#' all_cols <- colours()
#' all_cols <- unique(gsub("[0-9]","",all_cols))
#' smp_cols <- sample(all_cols,30)
#' smp_cols <- paste0(smp_cols,collapse=", ")
#' just_cols <- justify(smp_cols,width=40,align="centre")
#'
#' cat(just_cols)
#'
#' just_cols_html <- justify(smp_cols,width=50,align="r",newline="<br>")
#' cat(just_cols_html)
#'

justify.character <- function(x,width,align = "left",newline="\n",...)
{
  align <- substr(align,1,1)

  x_all <- gsub(newline," ",paste0(x,collapse=" "))
  x_all_split <- strsplit(x_all," ")[[1]]
  x_all_nchar <- nchar(x_all_split)

  if(max(x_all_nchar) > width)
  {
    long_word <- x_all_split[which.max(x_all_nchar)[1]]
    abort_message <- paste0("x contains the word \"",
                            long_word,"\" which is considered a ",
                            max(x_all_nchar),"-letter word.",
                            " The requested width is only ", width,".",
                            " Try a different width, or using gsub() to ",
                            "replace characters with \" \" to break this up.")
    rlang::abort(abort_message)
  }


  x_split <- lapply(x,justify_character_single,width=width,newline=newline)


  extra_chars <- lapply(x_split,
                        . %>%
                          nchar %>%
                          magrittr::subtract(width,.))

  if(align == "l")
  {
    right_chars <- extra_chars
    left_chars <- lapply(extra_chars,
                          . %>% magrittr::multiply_by(0))
  }

  if(align == "r")
  {
    left_chars <- extra_chars
    right_chars <- lapply(extra_chars,
                          . %>% magrittr::multiply_by(0))
  }

  if(align == "c")
  {
    left_chars <- lapply(extra_chars,
                         . %>%
                           magrittr::divide_by(2) %>%
                           floor)
    right_chars <- lapply(1:length(extra_chars),
                          function(i) extra_chars[[i]] - left_chars[[i]])
  }

  left_whitespace <- lapply(left_chars,strrep,x=" ")
  right_whitespace <- lapply(right_chars,strrep,x=" ")

  res_list <- lapply(1:length(x_split),
         function(i) paste0(left_whitespace[[i]],
                            x_split[[i]],
                            right_whitespace[[i]]))

  res <- sapply(res_list,paste,collapse=newline)

  res

}

justify_character_single <- function(x,width,newline)
{
  if(grepl(newline,x))
  {
    x_split <- strsplit(x,newline)[[1]]

    res_list <- lapply(x_split,justify_character_single,width=width,newline=newline)

    unlist(res_list)

  } else
  {

    df <- tibble::tibble(split = strsplit(x," ")[[1]]) %>%
      dplyr::mutate(nchar = nchar(split),
                    grp = 1) %>%
      dplyr::group_by(grp) %>%
      dplyr::mutate(cumulative = cumsum(nchar) + dplyr::row_number()-1) %>%
      dplyr::ungroup()

    while(any(df$cumulative >= width))
    {
      df <- df %>%
        dplyr::mutate(grp = grp + (cumulative>=width)) %>%
        dplyr::group_by(grp) %>%
        dplyr::mutate(cumulative = cumsum(nchar) + dplyr::row_number()-1) %>%
        dplyr::ungroup()
    }

    df %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(res = paste(split,collapse=" ")) %>%
      dplyr::mutate(nchar = nchar(res)) %>%
      dplyr::pull(res)
  }
}





