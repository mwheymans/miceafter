#' Calculates the proportion and standard error
#'
#' \code{prop_wald} Calculates the proportion and
#'  related standard error according to Wald.
#'
#' @param x name of variable to calculate proportion.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
#'
#' @return The proportion and the standard error.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
prop_wald <- function(x, formula, data){

  call <- match.call()

  if(!inherits(x,"formula")){
    eval_prop <- eval(call[[1]], parent.frame())
    if(!all(x==1 | x==0))
      stop("x variable should be a 0 - 1 variable")

    p <- sum(x)
    n <- length(x)

    # Proportion and Standard Error according to Wald
    phat <- p/n
    se_phat <- sqrt(phat*((1-phat)/n))
    dfcom <- n-1
  } else{
    eval_prop <- eval(call[[2]], parent.frame())
    # Proportion and Standard Error via formula
    fit <- glm(eval_prop, family=binomial)
    dfcom <- df.residual(fit)
    phat <- exp(coef(fit)) / (1 + exp(coef(fit)))
    se_phat <- vcov(fit)
  }
  obj <- c(phat, se_phat, dfcom)
  obj
}
