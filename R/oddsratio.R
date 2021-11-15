#' Calculates the odds ratio and standard error.
#'
#' \code{oddsratio} Calculates the log odds ratio and standard error.
#'
#' @param x 0-1 binary independent variable.
#' @param y 0-1 binary response variable.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{mids}, created by
#'  \code{make_mids} or after a call to function \code{mice}.
#'  If \code{data} is of type \code{data.frame}, use
#'  \code{make_mids} to convert to \code{mids} object.
#'
#' @return The odds ratio, related standard error and degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
oddsratio <- function(y, x, formula, data){

  call <- match.call()

  if(!inherits(y,"formula")){
    eval_prop <- eval(call[[1L]], parent.frame())
    X <- data.frame(y, x)
    if(!all(X$x==1 | X$x==0))
      stop("x variable should be a 0 - 1 variable")
    if(!all(X$y==1 | X$y==0))
      stop("y variable should be a 0 - 1 variable")

    sub1 <- subset(X, x==1)
    sub0 <- subset(X, x==0)

    n1 <- nrow(sub1)
    n0 <- nrow(sub0)

    xa <- nrow(subset(sub1, y==1))
    xb <- nrow(subset(sub1, y==0))
    xc <- nrow(subset(sub0, y==1))
    xd <- nrow(subset(sub0, y==0))

    OR <- (xa *xd) / (xb * xc)
    log_se <- sqrt(1/xa + 1/xb + 1/xc + 1/xd)
    dfcom <- nrow(X)-1
    c(OR, log_se, dfcom)
  } else {
    eval_prop <- eval(call[[2]], parent.frame())
    fit <- glm(eval_prop, y=TRUE, x=TRUE, family = binomial)
    OR <- exp(coef(fit)[2])
    OR_se <- sqrt(diag(vcov(fit))[2])
    dfcom <- df.residual(fit)+1
    c(OR, OR_se, dfcom)
  }
}
