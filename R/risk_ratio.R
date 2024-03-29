#' Calculates the risk ratio (RR) and standard error.
#'
#' \code{risk_ratio} Calculates the risk ratio and standard error.
#'
#' @param x 0-1 binary independent variable.
#' @param y 0-1 binary response variable.
#' @param formula A formula object to specify the model as
#'  normally used by glm.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#'
#' @details Note that the standard error of the RR is in fact the
#'  standard error of the (natural) risk ratio.
#'
#' @return The risk ratio, related standard error and
#'  complete data degrees of freedom (dfcom) as n-2.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=risk_ratio(Chronic ~ Radiation))
#'
#' @export
risk_ratio <- function(y, x, formula, data){

  call <- match.call()

  names_var <-
    all.vars(call, functions = FALSE)
  if(length(names_var) > 2)
    stop("Include only one independent variable in x or formula")

  if(!inherits(y,"formula")){
    eval_prop <-
      eval(call[[1L]], parent.frame())
    X <-
      data.frame(y, x)
  } else {
    call <-
      eval(call[[2L]], parent.frame())
    X <-
      model.frame(call)
    colnames(X) <-
      c('y', 'x')
  }
  if(!all(X$x==1 | X$x==0))
    stop("x variable should be a 0 - 1 variable")
  if(!all(X$y==1 | X$y==0))
    stop("y variable should be a 0 - 1 variable")

  sub1 <-
    subset(X, x==1)
  sub0 <-
    subset(X, x==0)

  n1 <-
    nrow(sub1)
  n0 <-
    nrow(sub0)

  x1 <-
    nrow(subset(sub1, y==1))
  x0 <-
    nrow(subset(sub0, y==1))

  p1hat <-
    x1/n1
  p0hat <-
    x0/n0

  dfcom <-
    (n1+n0)-2
  rr <-
    p1hat / p0hat
  rr_se <-
    sqrt(1/x1 - 1/n1 + 1/x0 - 1/n0)
  output <-
    matrix(c(rr, rr_se, dfcom), 1, 3)
  colnames(output) <-
    c("RR", "SE", "dfcom")
  output
}

