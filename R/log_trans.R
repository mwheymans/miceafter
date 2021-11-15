#' Natural Log transformation of parameter estimates
#'
#' \code{log_transform} Natural Log transformation of parameter
#'  estimate and standard error.
#'
#' @param est A numeric vector of values that have to be log transformed
#' @param se A numeric vector of standard error values
#'
#' @return The natural log transformed values.
#'
#' @details Function is used to naturally log transform parameters
#'  and standard errors before they can be pooled using function
#'  \code{pool_RR} before pooling with Rubin's Rules. To naturally
#'  log transform the standard error the Delta method is used.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
log_trans <- function(est, se){
  if(length(est)==1)
    stop("est must contain >1 value")
  est_log <-
    log(est/(1-est))
  se_log <-
    se / (est * (1-est))
  obj <- matrix(c(est_log, se_log),
                length(est), 2)
  return(obj)
}
