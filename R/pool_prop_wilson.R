#' Calculates the pooled single proportion confidence intervals according
#'  to Wilson across multiply imputed datasets.
#'
#' \code{pool_prop_wilson} Calculates the pooled single proportion and
#'  confidence intervals according to Wald across multiply imputed datasets.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis').
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @return The proportion and the 95% Confidence interval according to Wilson.
#'
#' @references Anne Lott & Jerome P. Reiter (2020) Wilson Confidence Intervals
#'  for Binomial Proportions With Multiple Imputation for Missing Data,
#'  The American Statistician, 74:2, 109-115, DOI: 10.1080/00031305.2018.1473796.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{prop_wald}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=prop_wald(Chronic ~ 1))
#' res <- pool_prop_wilson(ra)
#' res
#'
#' @export
pool_prop_wilson <- function(object, conf.level=0.95){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("est", "se", "dfcom")

  pool_est <- pool_scalar_RR(est=ra$est, se=ra$se,
                      logit_trans=FALSE,
                      conf.level = conf.level,
                      dfcom=ra$dfcom[1])

  mean_est <- pool_est$pool_est
  t <- pool_est$t
  n <- pool_est$dfcom+1
  r <- pool_est$r

  lower <- ((((2*mean_est) + ((t^2)/n) + (((t^2) * r)/n))/
               (2*(1 + ((t^2)/n) + (((t^2)*r)/n)))) -
              sqrt(((((2*mean_est) + ((t^2)/n) + (((t^2)*r)/n))^2)/
                      (4*(1 + ((t^2)/n) + (((t^2)*r)/n))^2)) -
                     ((mean_est^2)/(1 + ((t^2)/n) + (((t^2)*r)/n)))))

  upper <- ((((2*mean_est) + ((t^2)/n) + (((t^2) * r)/n))/
               (2*(1 + ((t^2)/n) + (((t^2)*r)/n)))) +
              sqrt(((((2*mean_est) + ((t^2)/n) + (((t^2)*r)/n))^2)/
                      (4*(1 + ((t^2)/n) + (((t^2)*r)/n))^2)) -
                     ((mean_est^2)/(1 + ((t^2)/n) + (((t^2)*r)/n)))))

  output <- matrix(round(c(mean_est, lower, upper), 5), 1, 3)
  colnames(output) <- c("Prop", "CI L Wilson", "CI U Wilson")
  class(output) <- 'mipool'
  return(output)
}
