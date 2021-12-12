#' Calculates the difference between proportions and standard error
#'  according to Wald
#'
#' \code{propdiff_wald} Calculates the difference between proportions and
#'  standard error according to Wald and degrees of freedom to
#'  be used in function \code{with.miceafter}.
#'
#' @param y 0-1 binary response variable.
#' @param x 0-1 binary independent variable.
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#' @param strata If TRUE the proportion, se and n of each group is provided.
#'  Default is FALSE. Has to be used in combination with function
#'  \code{pool_propdiff_wilson}
#'
#' @return The difference between proportions, standard error and complete
#'  data degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_propdiff_nw}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=propdiff_wald(Chronic ~ Radiation))
#'
#' # proportions in each subgroup
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=propdiff_wald(Chronic ~ Radiation, strata=TRUE))
#'
#' @export
propdiff_wald <- function(y,
                          x,
                          formula,
                          data,
                          strata=FALSE){

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

  # option strata
  if(strata==FALSE){
    p0hat <-
      x0/n0
    p1hat <-
      x1/n1

    dfcom <-
      (n0+n1)-1
    # Proportion and Standard Error according to Wald
    phat_diff <-
      p1hat - p0hat
    se_phat_diff <-
      sqrt((p0hat * (1 - p0hat)/n0) + (p1hat * (1 - p1hat)/n1))
    output <-
      matrix(c(phat_diff, se_phat_diff, dfcom), 1, 3)
    colnames(output) <-
      c("Prop diff", "SE", "dfcom")
    return(output)
  } else {
    # Proportion and SE within each group
    one_prop <- function(x, n){
      phat <- x/n
      se_phat <- sqrt((phat*(1-phat))/n)
      output <- c(phat, se_phat, n)
      return(output)
    }

    # Wald prop + se in each group
    est_gr0 <-
      one_prop(x0, n0)
    est_gr1 <-
      one_prop(x1, n1)
    output <-
      matrix(c(est_gr1, est_gr0), 1, 6)
    colnames(output) <-
      c("prop1", "se1", "n1",
                          "prop0", "se0", "n0")
    return(output)
  }
}
