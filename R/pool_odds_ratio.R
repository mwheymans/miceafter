#' Calculates the pooled odds ratio (OR) and related confidence interval.
#'
#' \code{pool_odds_ratio} Calculates the pooled odds ratio and
#'  confidence interval.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis')
#' @param conf.level Confidence level of the confidence intervals.
#' @param dfcom Complete data degrees of freedom. Default
#'  number is taken from function \code{odds_ratio}
#'
#' @return The pooled OR and confidence intervals.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{odds_ratio}}
#'
#' @examples
#'
#' library(magrittr)
#' lbpmilr %>%
#'    df2milist(impvar="Impnr") %>%
#'      with(expr=odds_ratio(Chronic ~ Radiation)) %>%
#'        pool_odds_ratio()
#'
#' # Same as
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=odds_ratio(Chronic ~ Radiation))
#' res <- pool_odds_ratio(ra)
#'
#' @export
pool_odds_ratio <- function(object,
                           conf.level = 0.95,
                           dfcom=NULL){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra_or <-
    do.call("rbind", lapply(object$statistics,
                            function(x) x))

  if(is_empty(dfcom)){
    dfcom <- ra_or[1, 3]
  } else {
    dfcom <- dfcom
  }
  pool_est <-
    pool_scalar_RR(est=log(ra_or[, 1]), se=ra_or[, 2],
                   logit_trans=FALSE,
                   conf.level=conf.level,
                   dfcom = dfcom)

  low <-
    exp(pool_est$pool_est - pool_est$t * pool_est$pool_se)
  high <-
    exp(pool_est$pool_est + pool_est$t * pool_est$pool_se)
  output <-
    matrix(c(exp(pool_est$pool_est), low, high), 1, 3)
  colnames(output) <-
    c("pooled OR", c(paste(conf.level*100, "CI low"),
                     paste(conf.level*100, "CI high")))
  class(output) <- 'mipool'
  return(output)
}
