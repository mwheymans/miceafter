#' Calculates the proportion and standard error
#'
#' \code{prop_wald} Calculates the proportion and
#'  related standard error according to Wald.
#'
#' @param x name of variable to calculate proportion.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{mids}, created by
#'  \code{make_mids} or after a call to function \code{mice}.
#'  If \code{data} is of type \code{data.frame}, use
#'  \code{make_mids} to convert to \code{mids} object.
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
