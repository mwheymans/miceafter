#' Calculates the c-index and standard error
#'
#' \code{cindex} Calculates the c-index and standard error for
#'  logistic and Cox regression models and the degrees of freedom
#'  to be further used in function \code{with.milist}.
#'
#' @param formula A formula object to specify the model as
#'  normally used by glm or coxph.
#' @param data An object of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#'
#' @return The c-index, related standard error and
#'  complete data degrees of freedom (dfcom) as n-1.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_cindex}}
#'
#' @examples
#'
#'  imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#'  ra <- with(data=imp_dat,
#'  expr = cindex(glm(Chronic ~ Gender + Radiation,
#'  family=binomial)))
#'
#' @export
cindex <- function(formula,
                   data){

  mf_call <- match.call()
  fit <- eval(mf_call[[2L]], parent.frame())

  if(any(class(formula)!="coxph")){
    predfit <-
      predict(fit, type="response")
    can <-
      pROC::roc(fit$y, predfit, quiet=TRUE)
    cest <-
      can$auc
    cse <-
      sqrt(var(can))
    n <-
      length(predfit)
  }
  else {
    cfit <-
      concordance(fit)
    cest <-
      cfit$concordance
    cse <-
      sqrt(cfit$var)
    n <-
      fit$n
  }
  dfcom <- n-1
  output <-
    matrix(c(cest, cse, dfcom), 1, 3)
  colnames(output) <-
    c("c-index", "se", "dfcom")
  return(output)
}
