#' Calculates the pooled proportion and standard error according
#'  to Wald across multiply imputed datasets.
#'
#' \code{pool_prop_wald} Calculates the pooled proportion and
#'  standard error according to Wald across multiply imputed datasets
#'  and using Rubin's Rules.
#'
#' @param object An object of class 'raami' (repeated analysis after
#'   multiple imputation) after using \code{with.aftermi}.
#' @param conf.level Confidence level of the confidence intervals.
#'
#' @details Before pooling, the proportions will be naturally log transformed and
#'  the pooled estimates back transformed to the original scale.
#'
#' @return The proportion and the 95% Confidence interval.
#'
#' @references Raghunathan, T. (2016). Missing Data Analysis in Practice.
#'  Boca Raton, FL: Chapman and Hall/CRC. (paragr 4.6.2)
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{prop_nna}}
#'
#' @examples
#'  imp_dat <- make_mids(lbpmilr, impvar='Impnr')
#'  ra <- with(imp_dat, expr=prop_nna(Radiation))
#'  res <- pool_prop_nna(ra)
#'  res
#'
#' @export
pool_prop_nna <- function(object, conf.level=0.95){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
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

  obj <- matrix(qbeta(c(0.5, (1-conf.level)/2,
                        (1-(1-conf.level)/2)), a, b), 1, 3)

  dimnames(obj) <-
    list(NULL, c("Prop nna", "95%CI L", "95%CI U "))
  class(obj) <- 'mipool'
  return(obj)
}
