#' Calculates the difference between proportions according to Wald
#'
#' \code{prop_diff_wald} Calculates the difference between proportions
#'  acoording to Wald.
#'
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
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

  # Proportion and Standard Error according to Wald
  phat_diff <-
    p1hat - p0hat
  se_phat_diff <-
    sqrt((p0hat * (1 - p0hat)/n0) + (p1hat * (1 - p1hat)/n1))
  return(c(phat_diff, se_phat_diff))
}
