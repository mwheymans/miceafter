#' Evaluate an Expression across a list of multiply imputed datasets
#'
#' \code{with.milist} Evaluate an expression in the form of a
#'  statistical test procedure across a list of multiply imputed datasets
#'
#' @param data data that is used to evaluate the expression in,
#'  an objects of class \code{milist} after a call to function
#'  \code{df2milist} or \code{mids2milist}. For 'df2milist'
#'  the original dataset (normally indicated as dataset 0) must be
#'  exluded and the imputed datasets must be distinguished by an
#'  imputation variable, specified under impvar and starting by 1.
#' @param expr expression to evaluate.
#' @param ... Not required.
#'
#' @return The value of the evaluated expression with class \code{mistats}
#' 'Multiply Imputed Statistical Analysis'.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
with.milist <- function(data, expr=NULL, ...)
{
  call <- match.call()

  if (!class(data)=="milist")
    stop("data must be of class 'milist'")

  statistics <- as.list(seq_len(length(data)))
  # output statistical analysis
  for (i in seq_along(statistics)) {
    df_m <- data[[i]]
    statistics[[i]] <- eval(expr = substitute(expr),
                            envir = df_m, enclos = parent.frame())
    if (is.expression(statistics[[i]])) {
      statistics[[i]] <- eval(expr = statistics[[i]],
                              envir = df_m, enclos = parent.frame())
    }
  }

  # return the repeated statistical analyses
  obj <- list(call = call, statistics = statistics)
  class(obj) <- c("mistats")
  return(obj)
}
