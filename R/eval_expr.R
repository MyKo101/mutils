#' @name eval_expr
#'
#' @title
#' Evaluate an expression within context
#'
#' @description
#' Uses [eval_tidy()][rlang::eval_tidy()] to evaluate an expression
#' within the context of `data`, however it gives
#' much more flexibility to what can be supplied as an expression.
#' See examples for a non-exhaustive list of uses. Beware, that the
#' flexibility of `eval_expr()` may cause unexpected behavior in
#' some circumstances.
#'
#' @param expr
#' expression to evaluate. Can be a call or a function, the name of
#' a function, a quosure, a quote.
#'
#' @param data
#' A data frame or named list or vector to provide context within
#' which to evaluate `expr`
#'
#' @param dot_expression
#' logical indicating whether to force a `.` expression evaluation.
#' If `.` is at the top layer (e.g. `length(.)`), then this will be
#' detected automatically
#'
#' @param verbose
#' logical indicating whether to print a detailed description of what
#' `eval_expr()` is doing.
#'
#' @export
#'
#' @examples
#'
#' #Basic evaluation in a data context
#' tbl <- tibble::tibble(x=1:100)
#' eval_expr(mean(x),tbl)
#'
#' #Can also use functions defined by pipelines (fseq)
#' eval_expr(. %>% dplyr::pull(x) %>% mean,tbl)
#'
#' #Different to eval_tidy() is that the data doesn't need names
#' eval_expr(mean,1:100)
#'
#' x <- 1:50
#' eval_expr(mean,x)
#'
#'
#' #functions using dot notation work fine too
#' eval_expr(mean(.),1:100)
#' eval_expr(mean(.$x),tbl)
#' eval_expr(quantile(.,c(0.05,0.5,0.95)),1:100)
#'
#' #However, only simple cases are detected automatically
#' \dontrun{
#' eval_expr(mean(.)/length(.),1:100)
#' }
#'
#' #More complicated expressions need to be explicit:
#' eval_expr(mean(.)/length(.),1:100,dot_expression=TRUE)
#'
#' #It even pulls the function from the data
#' lst <- list(x = 1:100,fun = mean)
#' eval_expr(fun(x),lst)
#'
#' #Be wary as functions defined within the data will mask those in
#' # the environment
#' lst <- list(x=1:100,mean = sd)
#' eval_expr(mean(x),lst)
#'
#'
#' #It can even unquote arguments
#' command <- quote(mean(x))
#' eval_expr(command,tbl)
#'
#' #Although this doesn't work explicitly
#' \dontrun{
#' eval_expr(quote(mean(x)),tbl)
#' }
#'
#' #Quosures aren't a problem
#' my_quo <- rlang::quo(mean(x))
#' eval_expr(my_quo,tbl)
#'
#' #And even within functions
#' f <- function(expr,data)
#' {
#'   quasi_expr <- rlang::enquo(expr)
#'   eval_expr(!!quasi_expr,data)
#' }
#'
#' f(mean(x),tbl)
#'
#' #And allows an environment to be passed as `data`
#'
#' my_env <- new.env()
#' my_env$x <- tbl$x
#' eval_expr(mean(x),my_env)
#'
#'


eval_expr <- function(expr,data,dot_expression=F,verbose=F)
{

  if(verbose)
  {
    cat0 <- function(...) cat(...,sep="\n")
  } else
  {
    cat0 <- function(...) invisible(NULL)
  }
  cat0("Starting eval_expr()")

  cat0("enquo-ing expression")
  .expr <- rlang::enquo(expr)




  cat0("Setting environments")
  env <- rlang::caller_env()



  cat0("Assessing data")

  if(rlang::is_named(data))
  {
    if(!is.environment(data))
    {
      cat0("Coercing data to an environment for evaluation")
      .data <- rlang::new_environment(data,parent=env)
    } else
    {
      cat0("data provided was an environment, coercion not needed")
      .data <- data
    }
    cat0("Setting the environment for evaluation to data")
    .expr <- rlang::quo_set_env(.expr,.data)
  } else
  {
    cat0("data did not have names, parent environment was prepared")
    .expr <- rlang::quo_set_env(.expr,env)
  }




  cat0("Assessing expr")

  if(!rlang::quo_is_call(.expr))
  {
    if(rlang::is_quosure(rlang::eval_tidy(.expr)))
    {
      cat0("expr was found to be a quosure, so it will be extracted")
      .quo <- rlang::quo_get_expr(rlang::eval_tidy(.expr))
      .expr <- rlang::quo_set_expr(.expr,.quo)
    }

    if(rlang::is_call(rlang::eval_tidy(.expr)))
    {
      cat0("expr was found to be a variable pointing to call.",
           "\tUnwrapping the call")
      .expr <- rlang::quo_set_expr(.expr,expr)
    }
  }

  if(dot_expression || has_dot_arg(rlang::quo_get_expr(.expr)))
  {
    if(verbose)
    {
      if(dot_expression)
      {
        cat0("dot_expression set to TRUE")
      } else
      {
        cat0("Dot(s) were found in the top level of expr")
      }
    }
    cat0("expr will be wrapped in a function appropriately")
    .fun <- rlang::new_function(alist(.=),rlang::quo_get_expr(.expr))
    .expr <- rlang::quo_set_expr(.expr,.fun)
  }



  cat0("Evaluating beginning using eval_tidy()...")
  expr_res <- rlang::eval_tidy(.expr)
  cat0("Evaluating complete.")

  if(is.function(expr_res))
  {
    cat0("Contextual evaluation resulted in a function.",
         "\tThe function will be applied to data")
    expr_res <- rlang::eval_tidy(expr_res(data),env = env)
  }



  cat0("eval_expr() complete. Returning result.")
  expr_res

}


