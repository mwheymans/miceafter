#' Calculates the difference between proportions according to Wald
#'
#' \code{prop_diff_wald} Calculates the difference between proportions
#'  acoording to Wald to be used in \code{with.aftermi}.
#'
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{mids}, created by
#'  \code{make_mids} or after a call to function \code{mice}.
#'  If \code{data} is of type \code{data.frame}, use
#'  \code{make_mids} to convert to \code{mids} object.
#'
#' @return The difference between proportions and the standard error.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
prop_diff_wald <- function(formula, data){

  mf_call <- match.call()
  mf_call[[1L]] <- quote(stats::model.frame)
  mf_call <- eval(mf_call, parent.frame())
  X <- model.frame(mf_call)

  fm <- terms(mf_call)
  outcome <- attr(fm, "variables")[[2]]
  group <- attr(fm, "variables")[[3]]

  sub0 <- subset(X, get(group)==0)
  sub1 <- subset(X, get(group)==1)

  n0 <- nrow(sub0)
  n1 <- nrow(sub1)

  x0 <- nrow(subset(sub0, get(outcome)==1))
  x1 <- nrow(subset(sub1, get(outcome)==1))

  p0hat <- x0/n0
  p1hat <- x1/n1

  dfcom <- (n0+n1)-1
  # Proportion and Standard Error according to Wald
  phat_diff <-
    p1hat - p0hat
  se_phat_diff <-
    sqrt((p0hat * (1 - p0hat)/n0) + (p1hat * (1 - p1hat)/n1))
  return(c(phat_diff, se_phat_diff, dfcom))
}
