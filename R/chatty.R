#' @name chatty
#'
#' @title
#' Used to define whether a function is chatty or not
#'
#' @description
#' Can be used to make a function talk more or not depending on the
#' `verbose` argument. This will return either a `cat()` function
#' (with arguments separated by `"\n"` to make it easier to read),
#' or a silent function depending on `verbose`. Should only really
#' be used within another function to decide whether to output to
#' notes/feedback to the console or not throughout it's run. It
#' does no evaluating of arguments and so everything should be
#' easily (i.e. by `cat()`) coercible to a `character`.
#'
#' @param verbose
#' logical. Should `chatty` return a chatty function or not.
#'
#' @export
#'
#' @examples
#' I_talk <- chatty(TRUE)
#' I_talk("hello")
#' I_talk("I talk", "more...")
#'
#' Im_silent <- chatty(FALSE)
#' Im_silent("I don't say", "anything")
#'
#' f <- function(x, verbose = FALSE) {
#'   I_talk <- chatty(verbose)
#'   I_talk("hello world", "I am inside the f() function")
#'   I_talk("I'm calculating the mean...")
#'   mean(x)
#' }
#'
#' f(1:10)
#'
#' f(1:10, verbose = TRUE)
chatty <- function(verbose) {
  if (verbose) {
    function(...) cat(..., sep = "\n")
  } else {
    function(...) invisible(NULL)
  }
}
