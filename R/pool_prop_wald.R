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
#' @param dfcom Complete data degrees of freedom. Default
#'  number is taken from function \code{prop_wald}
#'
#' @details Before pooling, the proportions will be naturally log
#'  transformed and the pooled estimates back transformed to the original scale.
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
#' ra <- with(imp_dat, expr=prop_wald(Radiation ~ 1))
#' res <- pool_prop_wald(ra)
#' res
#'
#' @export
pool_prop_wald <- function(object,
                           conf.level=0.95,
                           dfcom=NULL){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <-
    data.frame(do.call("rbind", object$statistics))
  colnames(ra) <-
    c("est", "se", "dfcom")

  if(is_empty(dfcom)){
    dfcom <- ra$dfcom[1]
  } else {
    dfcom <- dfcom
  }

  pool_est <-
    pool_scalar_RR(est=ra$est, se=ra$se,
                      logit_trans=TRUE,
                      conf.level = conf.level, dfcom=dfcom)

  output <-
    invlogit_ci(est=pool_est$pool_est,
              se=pool_est$pool_se, crit.value=pool_est$t)
  colnames(output) <-
    c("Prop Wald", "Statistic", "95%CI L", "95%CI U")

  if(output[4] > 1) output[4] <- 1.00
  if(output[3] < 0) output[3] <- 0.00

  class(output) <- 'mipool'
  return(output)
}
