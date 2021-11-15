#' Back transformation of pooled parameters and calculating
#'  confidence intervals
#'
#' \code{back_trans} Back transformation to original scale of
#'  natural logarithmic transformed parameters and calculating
#'  confidence interval
#'
#' @param est A natural logarithmic transformed pooled parameter
#'  estimate
#' @param se A natural logarithmic transformed pooled
#'  standard error
#' @param t t-value
#'
#' @return Confidence interval.
#'
#' @details Back transformation of naturally log transformed
#'  and pooled parameter estimates.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
back_trans <- function(est, se, t){
  # Backtransform
  inv.est <- exp(est) / (1 + exp(est))
  inv.est.u <- exp(est + (t*se)) /
    (1 + exp(est + (t*se)))
  inv.est.l <- exp(est - (t*se)) /
    (1 + exp(est - (t*se)))
  obj <- matrix(c(inv.est, inv.est.l, inv.est.u),
                1, 3, byrow = T)
  return(obj)
}
