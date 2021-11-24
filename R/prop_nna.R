#' Calculates the proportion and standard error according to
#'  a nonnormal approximation
#'
#' \code{prop_nna} Calculates the proportion and
#'  related confidence interval using a beta distribution.
#'
#' @param x name of variable to calculate proportion.
#' @param data An objects of class \code{mids}, created by
#'  \code{make_mids} or after a call to function \code{mice}.
#'  If \code{data} is of type \code{data.frame}, use
#'  \code{make_mids} to convert to \code{mids} object.
#'
#' @return The mean and variance components of the beta distribution.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
prop_nna <- function(x, data){

  call <- match.call()

  eval_prop <- eval(call[[1]], parent.frame())
  if(!all(x==1 | x==0))
    stop("x variable should be a 0 - 1 variable")

  p <- sum(x)
  n <- length(x)

  # Proportion
  e <- (p + 1/2) / (n + 1)
  u <- (e*(1-e))/(n + 2)

  obj <- c(e, u)
  return(obj)
}