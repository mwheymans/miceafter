#' Takes the inverse of logit transformed parameters and calculates
#'  the confidence intervals
#'
#' \code{inv_logit} Takes the inverse of logit transformed
#'  parameters and calculates the confidence interval
#'  by using the critical value.
#'
#' @param est A parameter estimate on the logit scale.
#' @param se A standard error value on the logit scale.
#' @param crit.value Critical value of any distribution.
#'
#' @return Parameter, critical value and confidence
#'  intervals on original scale.
#'
#' @details Takes the inverse of logit transformed parameter
#'  estimates. The confidence interval is calculated as taking the
#'  inverse of \eqn{est +/- crit.value{1-\alpha/2} * se}.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'  inv_logit(est=1.39, se=0.25, crit.value=1.96)
#'
#' @export
inv_logit <- function(est, se, crit.value){
  inv.est <- exp(est) / (1 + exp(est))
  inv.est.u <- exp(est + (crit.value*se)) /
    (1 + exp(est + (crit.value*se)))
  inv.est.l <- exp(est - (crit.value*se)) /
    (1 + exp(est - (crit.value*se)))
  obj <- matrix(c(inv.est, crit.value, inv.est.l, inv.est.u),
                1, 4, byrow = T)
  return(obj)
}
