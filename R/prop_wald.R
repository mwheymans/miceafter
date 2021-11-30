#' Calculates a single proportion and related standard error
#'  according to Wald
#'
#' \code{prop_wald} Calculates a single proportion and
#'  related standard error according to Wald and
#'  provides degrees of freedom to be used
#'  in function \code{with.miceafter}.
#'
#' @param x name of variable to calculate proportion.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#'
#' @return The proportion and the standard error.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_prop_wald}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=prop_wald(Chronic ~ 1))
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
    se_phat <- sqrt((phat*(1-phat))/n)
    dfcom <- n-1
  } else{
    eval_prop <- eval(call[[2]], parent.frame())
    # Proportion and Standard Error via formula
    fit <- glm(eval_prop, family=binomial)
    if(!is_empty(attr(fit$terms, "term.labels")))
      stop("Model can only include an outcome variable,
           Use '1' for the independent variable")
    dfcom <- df.residual(fit)
    phat <- exp(coef(fit)) / (1 + exp(coef(fit)))
    se_phat <- sqrt((phat*(1-phat))/(dfcom+1))
  }
  obj <- c(phat, se_phat, dfcom)
  obj
}
