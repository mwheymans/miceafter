#' Calculates the pooled single proportion confidence intervals according
#'  to Wilson across multiply imputed datasets.
#'
#' \code{pool_prop_wilson} Calculates the pooled single proportion and
#'  confidence intervals according to Wald across multiply imputed datasets.
#'
#' @param object An object of class 'raami' (repeated analysis after
#'   multiple imputation) after using \code{with.aftermi}.
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @details Before pooling, the proportions will be naturally log transformed and
#'  the pooled estimates back transformed to the original scale.
#'
#' @return The proportion and the 95% Confidence interval according to Wilson.
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
                      logit_trans=FALSE, conf.level = conf.level, dfcom=ra$dfcom[1])

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

  output <- round(c(mean_est, lower, upper), 4)
  names(output) <- c("Prop", "CI L Wilson", "CI U Wilson")
  class(output) <- 'mipool'
  return(output)
}
