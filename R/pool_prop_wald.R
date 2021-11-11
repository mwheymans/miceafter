#' Calculates the pooled proportion and standard error according
#'  to Wald across multiply imputed datasets.
#'
#' \code{pool_prop_wald} Calculates the pooled proportion and
#'  standard error according to Wald across multiply imputed datasets
#'  and using Rubin's Rules.
#'
#' @param object An object of class 'raami' after using \code{with.aftermi}.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
#'
#' @details Before pooling, the proportions will be naturally log transformed and
#'  the pooled estimates back transformed to the original scale.
#'
#' @return The proportion and the 95% Confidence interval.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
pool_prop_wald <- function(object, conf.level=0.95){

  if(all(class(object)!="raami"))
    stop("object must be of class 'raami'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("est", "se", "df_com")

  pool_est <- pool_RR(est=ra$est, se=ra$se,
                      log_trans=TRUE, conf.level = conf.level, df_com=ra$df_com[1])

  res <-
    back_trans(pool_est$pool_est, pool_est$pool_se, pool_est$t)
  dimnames(res) <-
    list(NULL, c("Prop Wald", "95%CI L", "95%CI U "))
  return(res)
}
