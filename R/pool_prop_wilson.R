#' Calculates the pooled single proportion confidence intervals according
#'  to Wilson across multiply imputed datasets.
#'
#' \code{pool_prop_wilson} Calculates the pooled single proportion and
#'  confidence intervals according to Wald across multiply imputed datasets
#'  and using Rubin's Rules.
#'
#' @param object An object of class 'raami' (repeated analysis after
#'   multiple imputation) after using \code{with.aftermi}.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
#'
#' @details Before pooling, the proportions will be naturally log transformed and
#'  the pooled estimates back transformed to the original scale.
#'
#' @return The proportion and the 95% Confidence interval according to Wilson.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#' imp_dat <- make_mids(lbpmilr, impvar="Impnr")
#' ra <- with.miceafter(imp_dat, expr=prop_wald(Chronic ~ 1))
#' res <- pool_prop_wilson(ra)
#' res
#'
#' @export
pool_prop_wilson <- function(object, conf.level=0.95){

  if(all(class(object)!="raami"))
    stop("object must be of class 'raami'")
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

  reswilson <- round(c(mean_est, lower, upper), 4)
  names(reswilson) <- c("Prop", "CI L Wilson", "CI U Wilson")
  return(reswilson)
}
