#' Calculates the pooled difference between proportions and standard error
#'  according to Wald across multiply imputed datasets.
#'
#' \code{pool_propdiff_wald} Calculates the pooled difference between proportions
#'  and standard error according to Wald across multiply imputed datasets.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis').
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @return The proportion, the Confidence intervals,
#'  the standard error and statistic.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{propdiff_wald}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=propdiff_wald(Chronic ~ Gender))
#' res <- pool_propdiff_wald(ra)
#' res
#'
#' @export
pool_propdiff_wald <- function(object, conf.level=0.95)
{

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("est", "se", "dfcom")

  pool_est <- pool_scalar_RR(est=ra$est, se=ra$se,
                             logit_trans=FALSE,
                             conf.level = conf.level, dfcom=ra$dfcom[1])
  low <-
    pool_est$pool_est - pool_est$t * pool_est$pool_se
  high <-
    pool_est$pool_est + pool_est$t * pool_est$pool_se
  output <-
    round(matrix(c(pool_est$pool_est, pool_est$pool_se,
                           pool_est$t, low, high), 1, 5), 5)
  colnames(output) <-
    c("Prop diff Wald", "SE", "t",
                        c(paste(conf.level*100, "CI low"),
                          paste(conf.level*100, "CI high")))
  class(output) <-
    "mipool"
  return(output)
}
