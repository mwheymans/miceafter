#' Calculates the pooled difference between proportions and standard error
#'  according to Agresti-Caffo across multiply imputed datasets.
#'
#' \code{pool_prop_wald} Calculates the pooled difference between proportions
#'  and standard error according to Agresti-Caffo across multiply imputed datasets.
#'
#' @param object An object of class 'raami' (repeated analysis after
#'  multiple imputation) after using \code{with.aftermi}.
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @return The proportion, the Confidence intervals, the standard error and statistic.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.miceafter}}, \code{\link{propdiff_ac}}
#'
#' @examples
#' imp_dat <- make_mids(lbpmilr, impvar="Impnr")
#' ra <- with.miceafter(imp_dat, expr=propdiff_ac(Chronic ~ Gender))
#' res <- pool_propdiff_ac(ra)
#' res
#'
#' @export
pool_propdiff_ac <- function(object, conf.level=0.95)
{

  if(all(class(object)!="raami"))
    stop("object must be of class 'raami'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("est", "se", "dfcom")

  pool_est <- pool_scalar_RR(est=ra$est, se=ra$se,
                             logit_trans=FALSE,
                             conf.level = conf.level, dfcom=ra$dfcom[1])
  low <- pool_est$pool_est - pool_est$t * pool_est$pool_se
  high <- pool_est$pool_est + pool_est$t * pool_est$pool_se
  output <- matrix(c(pool_est$pool_est, low, high), 1, 3)
  colnames(output) <- c("Prop diff AC",
                        c(paste(conf.level*100, "CI low"),
                          paste(conf.level*100, "CI high")))
  class(output) <- 'paami'
  return(output)
}

