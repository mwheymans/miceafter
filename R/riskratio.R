#' Calculates the risk ratio and standard error.
#'
#' \code{riskratio} Calculates the risk ratio and standard error.
#'
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
#'
#' @return The risk ratio and related standard error and degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
riskratio <- function(formula, data){

  mf_call <- match.call()
  mf_call[[1L]] <- quote(stats::model.frame)
  mf_call <- eval(mf_call, parent.frame())
  X <- model.frame(mf_call)

  fm <- terms(mf_call)
  outcome <- attr(fm, "variables")[[2]]
  group <- attr(fm, "variables")[[3]]

  sub1 <- subset(X, get(group)==1)
  sub0 <- subset(X, get(group)==0)

  n1 <- nrow(sub1)
  n0 <- nrow(sub0)

  x1 <- nrow(subset(sub1, get(outcome)==1))
  x0 <- nrow(subset(sub0, get(outcome)==1))

  p1hat <- x1/n1
  p0hat <- x0/n0

  dfcom <- c(n1+n0)-1
  rr <-
    p1hat / p0hat
  rr_se <-
    sqrt(1/x1 - 1/n1 + 1/x0 - 1/n0)
  return(c(rr, rr_se, dfcom))
}

