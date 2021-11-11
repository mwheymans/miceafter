#' Natural Log transformation of input parameters
#'
#' \code{log_transform} Natural Log transformation of parameter
#'  estimate and standard error.
#'
#' @param est A numeric vector of values that have to be log transformed
#' @param se A numeric vector of standard error values that have to be log transformed
#'
#' @return The natural log transformed values.
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
