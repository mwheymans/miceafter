#' Calculates the pooled proportion and standard error according
#'  to Wald across multiply imputed datasets.
#'
#' \code{pool_prop_wald} Calculates the pooled proportion and
#'  standard error according to Wald across multiply imputed datasets
#'  and using Rubin's Rules.
#'
#' @param object An object of class 'mistats' (repeated statistical
#'  analysis across multiply imputed datasets).
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @details Before pooling, the proportions will be naturally log transformed and
#'  the pooled estimates back transformed to the original scale.
#'
#' @return The proportion, the Confidence intervals, the standard error
#'  and the statistic.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{prop_wald}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=prop_wald(Chronic ~ 1))
#' res <- pool_prop_wald(ra)
#' res
#'
#' @export
pool_prop_wald <- function(object, conf.level=0.95){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("est", "se", "dfcom")

  pool_est <- pool_scalar_RR(est=ra$est, se=ra$se,
                      logit_trans=TRUE,
                      conf.level = conf.level, dfcom=ra$dfcom[1])

  output <-
    inv_logit(est=pool_est$pool_est,
              se=pool_est$pool_se, crit.value=pool_est$t)
  colnames(output) <-
    c("Prop Wald", "Statistic", "95%CI L", "95%CI U")
  class(output) <- 'mipool'
  return(output)
}