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
#' @details For the pooled difference between proportions the difference
#'  between proportions according to Wald are used. The Agresti-Caffo
#'  difference is used to derive the Agresti-Caffo confidence intervals.
#'
#' @return The proportion, the Confidence intervals, the standard error and
#'  statistic.
#'
#' @references Agresti, A. and Caffo, B. Simple and Effective Confidence
#'  Intervals for Proportions and Differences of Proportions Result from
#'  Adding Two Successes and Two Failures. The American Statistician.
#'  2000;54:280-288.
#' @references Fagerland MW, Lydersen S, Laake P. Recommended confidence
#'  intervals for two independent binomial proportions. Stat Methods Med Res.
#'  2015 Apr;24(2):224-54.
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
  colnames(ra) <- c("est", "se", "dfcom", "est_wald")

  if(is_empty(dfcom)){
    dfcom <- ra$dfcom[1]
  } else {
    dfcom <- dfcom
  }

  pool_est <- pool_scalar_RR(est=ra$est, se=ra$se,
                             logit_trans=FALSE,
                             conf.level = conf.level, dfcom=dfcom)
  pool_wald <- mean(ra$est_wald)

  low <-
    pool_est$pool_est - pool_est$t * pool_est$pool_se
  high <-
    pool_est$pool_est + pool_est$t * pool_est$pool_se
  output <-
    matrix(c(pool_wald, low, high), 1, 3)
  colnames(output) <-
    c("Prop diff",
      c(paste(conf.level*100, "CI low"),
        paste(conf.level*100, "CI high")))
  class(output) <- 'mipool'
  return(output)
}

