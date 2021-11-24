#' Calculates the pooled proportion and standard error according
#'  to Wald across multiply imputed datasets.
#'
#' \code{pool_prop_wald} Calculates the pooled proportion and
#'  standard error according to Wald across multiply imputed datasets
#'  and using Rubin's Rules.
#'
#' @param object An object of class 'raami' after using \code{with.aftermi}.
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
#' @references Raghunathan, T. (2016). Missing Data Analysis in Practice.
#'  Boca Raton, FL: Chapman and Hall/CRC.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'  lbpmilr <- psfmi::lbpmilr
#'
#'  imp_dat <- make_mids(lbpmilr, impvar='Impnr')
#'
#'  ra <- miceafter::with(imp_dat, expr=prop_nna(Radiation))
#'
#'  pool_prop_na(ra)
#'
#' @export
pool_prop_nna <- function(object, conf.level=0.95){

  if(all(class(object)!="raami"))
    stop("object must be of class 'raami'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <- data.frame(do.call("rbind", object$statistics))
  colnames(ra) <- c("e", "u")

  m <- length(ra[, 1])
  e_m <- mean(ra[, 1])
  u_m <- mean(ra$u) # Within Variance
  b_m <- var(ra$e) # Between Variance
  T_m <- u_m + (1 + (1/m))*b_m # Total variance

  a <- e_m*(((e_m*(1-e_m)) / (T_m)) -1)
  b <- (1-e_m)*(((e_m*(1-e_m))/(T_m)) -1)

  obj <- matrix(qbeta(c(0.5, 0.025,0.975), a, b), 1, 3)

  dimnames(obj) <-
    list(NULL, c("Prop nna", "95%CI L", "95%CI U "))
  class(obj) <- 'paami'
  return(obj)
}