#' Calculates the odds ratio (OR) and standard error.
#'
#' \code{oddsratio} Calculates the odds ratio and standard error
#'  and degrees of freedom to be used in function \code{with.miceafter}.
#'
#' @param x 0-1 binary independent variable.
#' @param y 0-1 binary response variable.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{df2milist} or \code{mids2milist}.
#'
#' @details Note that the standard error of the OR is on the
#'  logit scale.
#'
#' @return The odds ratio, related standard error and degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_oddsratio}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=oddsratio(Chronic ~ Radiation))
#'
#' @export
oddsratio <- function(y, x, formula, data){

  call <- match.call()

  if(!inherits(y,"formula")){
    names_var <- all.vars(call, functions = FALSE)
    if(length(names_var) > 2)
      stop("x should include one variable")
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
    dfcom <- nrow(X)-2
    c(OR, log_se, dfcom)
  } else {
    eval_prop <- eval(call[[2]], parent.frame())
    fit <- glm(eval_prop, y=TRUE, x=TRUE, family = binomial)
    nr_var <- attr(fit$terms, "term.labels")
    if(length(nr_var) > 1)
      stop("Include single independent variable only or use function psfmi_lr")
    OR <- exp(coef(fit)[2])
    OR_se <- sqrt(diag(vcov(fit))[2])
    dfcom <- df.residual(fit)
    output <- matrix(c(OR, OR_se, dfcom), 1, 3)
    colnames(output) <- c("OR", "SE", "dfcom")
    output
  }
}
