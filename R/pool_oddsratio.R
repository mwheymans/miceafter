#' Calculates the pooled odds ratio (OR) and related confidence interval.
#'
#' \code{pool_oddsratio} Calculates the pooled odds ratio and
#'  confidence interval.
#'
#' @param object An object of class 'mistats' (repeated statistical
#'  analysis across multiply imputed datasets)
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @return The pooled OR and confidence intervals.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{oddsratio}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=oddsratio(Chronic ~ Radiation))
#' res <- pool_oddsratio(ra)
#' res
#'
#' @export
pool_oddsratio <- function(object, conf.level = 0.95){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra_or <-
    do.call("rbind", lapply(object$statistics,
                            function(x) x))

  pool_est <-
    pool_scalar_RR(est=log(ra_or[, 1]), se=ra_or[, 2],
                   logit_trans=FALSE,
                   conf.level=conf.level,
                   dfcom = ra_or[1, 3])

  low <- exp(pool_est$pool_est - pool_est$t * pool_est$pool_se)
  high <- exp(pool_est$pool_est + pool_est$t * pool_est$pool_se)
  output <- matrix(c(exp(pool_est$pool_est), low, high), 1, 3)
  colnames(output) <- c("pooled OR",
                        c(paste(conf.level*100, "CI low"),
                          paste(conf.level*100, "CI high")))
  class(output) <- 'mipool'
  return(output)
}
