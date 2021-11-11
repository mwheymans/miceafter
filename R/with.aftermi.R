#' Evaluate an Expression across multiply imputed datasets
#'
#' \code{with.aftermi} Evaluate an expression in the form of a
#'  statistical test procedure across multiply imputed datasets
#'
#' @param data Data that is used to evaluate the expression in.
#'   Can be an objects of class \code{mids} after a call to function \code{mice},
#'   or to function \code{make_mids}. Or an object of class 'data.frame'
#'   consisting of a dataset containing the stacked imputed datasets (excluding)
#'   the original dataset (normally indicated as dataset 0). The imputed datasets
#'   must be distinguished by an imputation variable, specified under impvar,
#'   and starting by 1.
#' @param expr expression to evaluate.
#' @param impvar A character vector. Name of the variable that distinguishes the
#' imputed datasets. Only necessary when data object is a 'data.frame'.
#'
#' @return The value of the evaluated expression with class \code{raami} 'repeated
#'  analyses after multiple imputation'.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
with.aftermi <- function(data, expr=NULL, impvar=NULL)
{
  call <- match.call()

  if (is.mids(data))
    statistics <- as.list(seq_len(data$m))
  if (!is.mids(data) & class(data)=="list")
    statistics <- as.list(seq_len(length(data)))
  if (is.data.frame(data)){
    #if (sort(unique(data[, impvar]))[1] == 0)
    #  stop("Original dataset with missings should not be included")
    if(is.null(impvar))
      stop("You forgot to define the impvar variable")
    data <- data %>%
      group_split(get(impvar), .keep = FALSE)
    statistics <- as.list(seq_len(length(data)))
  }

  # output statistical analysis
  for (i in seq_along(statistics)) {
    if (!is.mids(data)){
      df_m <- data[[i]]
    } else {
      df_m <- complete(data, i)
    }
    statistics[[i]] <- eval(expr = substitute(expr), envir = df_m, enclos = parent.frame())
    if (is.expression(statistics[[i]])) {
      statistics[[i]] <- eval(expr = statistics[[i]], envir = df_m, enclos = parent.frame())
    }
  }

  # return the complete data analyses as a list of length nimp
  obj <- list(call = call, statistics = statistics)
  # formula=formula(analyses[[1]]$terms))
  class(obj) <- c("raami")
  return(obj)
}
