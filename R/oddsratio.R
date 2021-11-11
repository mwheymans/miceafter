#' Calculates the odds ratio and standard error.
#'
#' \code{oddsratio} Calculates the log odds ratio and standard error.
#'
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
#'
#' @return The odds ratio, related standard error and degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
oddsratio <- function(formula, data){

  mf_call <- match.call()
  mf_call[[1L]] <- quote(stats::model.frame)
  mf_call <- eval(mf_call, parent.frame())

  fit <- glm(mf_call, family=binomial)
  OR <- exp(coef(fit)[2])
  OR_se <- sqrt(diag(vcov(fit))[2])
  dfcom <- df.residual(fit)
  return(c(OR, OR_se, dfcom))
}

