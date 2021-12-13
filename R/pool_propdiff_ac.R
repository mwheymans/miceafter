#' Calculates the pooled difference between proportions and standard error
#'  according to Agresti-Caffo across multiply imputed datasets.
#'
#' \code{pool_propdiff_ac} Calculates the pooled difference between proportions
#'  and standard error according to Agresti-Caffo across multiply imputed datasets.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis').
#' @param conf.level Confidence level of the confidence intervals.
#' @param dfcom Complete data degrees of freedom. Default
#'  number is taken from function \code{propdiff_ac}
#'
#' @return The proportion, the Confidence intervals, the standard error and
#'  statistic.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{propdiff_ac}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=propdiff_ac(Chronic ~ Radiation))
#' res <- pool_propdiff_ac(ra)
#' res
#'
#' @export
pool_propdiff_ac <- function(object,
                             conf.level=0.95,
                             dfcom=NULL)
{

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("est", "se", "dfcom")

  if(is_empty(dfcom)){
    dfcom <- ra$dfcom[1]
  } else {
    dfcom <- dfcom
  }

  pool_est <- pool_scalar_RR(est=ra$est, se=ra$se,
                             logit_trans=FALSE,
                             conf.level = conf.level, dfcom=dfcom)
  low <-
    pool_est$pool_est - pool_est$t * pool_est$pool_se
  high <-
    pool_est$pool_est + pool_est$t * pool_est$pool_se
  output <-
    matrix(c(pool_est$pool_est, low, high), 1, 3)
  colnames(output) <-
    c("Prop diff AC",
                        c(paste(conf.level*100, "CI low"),
                          paste(conf.level*100, "CI high")))
  class(output) <- 'mipool'
  return(output)
}

