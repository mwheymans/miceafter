#' Calculates the posterior mean and variance components of a
#'  single proportion
#'
#' \code{prop_nna} Calculates the posterior mean and variance
#'  of a single proportion.
#'
#' @param x name of variable to calculate proportion.
#' @param data An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis').
#'
#' @return The posterior mean and variance.
#'
#' @references Raghunathan, T. (2016). Missing Data Analysis in Practice.
#'  Boca Raton, FL: Chapman and Hall/CRC. (paragr 4.6.2)
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_prop_nna}}
#'
#' @examples
#'  imp_dat <- df2milist(lbpmilr, impvar='Impnr')
#'  ra <- with(imp_dat, expr=prop_nna(Radiation))
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
