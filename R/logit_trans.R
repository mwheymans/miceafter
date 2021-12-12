#' Logit transformation of parameter estimates
#'
#' \code{logit_trans} Logit transformation of parameter
#'  estimate and standard error.
#'
#' @param est A numeric vector of values.
#' @param se A numeric vector of standard error values.
#'
#' @return The logit transformed values.
#'
#' @details Function is used to logit transform parameters
#'  and standard errors. For the standard error the
#'  Delta method is used.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
logit_trans <- function(est,
                        se){
  est_logit <-
    log(est/(1-est))
  se_logit <-
    se / (est * (1-est))
  obj <-
    matrix(c(est_logit, se_logit),
                length(est), 2)
  return(obj)
}
