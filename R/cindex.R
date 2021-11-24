#' Calculates the c-index and standard error
#'
#' \code{cindex} Calculates the c-index and standard error for
#'  logistic and Cox regression models and degrees of freedom
#'  to be used in function \code{with.miceafter}.
#'
#' @param formula A formula object to specify the model as
#'  normally used by glm or coxph.
#' @param data An objects of class \code{mids}, created by
#'  \code{make_mids} or after a call to function \code{mice}.
#'  If \code{data} is of type \code{data.frame}, use
#'  \code{make_mids} to convert to \code{mids} object.
#'
#' @return The c-index, related standard error and
#'  degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.miceafter}}
#'
#' @examples
#'  imp_list <- lbpmilr %>%
#'   group_split(Impnr, .keep = FALSE)
#'
#'  imp_data <- make_mids(imp_list)
#'
#'  ra <- with.miceafter(data=imp_data,
#'   expr = cindex(glm(Chronic ~ Gender + Radiation,
#'   family="binomial")))
#'
#' @export
cindex <- function(formula, data){

  mf_call <- match.call()
  fit <- eval(mf_call[[2L]], parent.frame())

  if(any(class(formula)!="coxph")){
    predfit <- predict(fit, type="response")
    can <- pROC::roc(fit$y, predfit, quiet=TRUE)
    cest <- can$auc
    cse <- sqrt(var(can))
    n <- length(predfit)
  }
  else {
    cfit <- concordance(fit)
    cest <- cfit$concordance
    cse <- sqrt(cfit$var)
    n <- fit$n
  }
  dfcom <- n-1
  output <- matrix(c(cest, cse, dfcom), 1, 3)
  colnames(output) <- c("c-index", "se", "dfcom")
  return(output)
}
