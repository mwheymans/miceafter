#' Calculates the Levene's test
#'
#' \code{levene_test} Calculates the Levene's test for homogeneity
#'  of variance across groups, model coefficients, the
#'  variance-covariance matrix and the degrees of freedom.
#'
#' @param x categorical group variable.
#' @param y numeric (continuous) response variable.
#' @param formula A formula object to specify the model as normally
#'  used by glm. Use 'factor' to define the grouping x variable. Only
#'  one grouping variable is allowed.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#'
#' @details The Levene's test centers on group means to calculate
#'  outcome residuals, the Brown-Forsythe test on the median.
#'
#'@return An object from which the following objects are extracted:
#'  \itemize{
#'  \item  \code{fstats} F-test value, including numerator and
#'   denominator degrees of freedom.
#'  \item  \code{qhat} model coefficients.
#'  \item  \code{vcov} variance-covariance matrix.
#'  \item  \code{dfcom} degrees of freedom obtained from \code{df.residual}.
#'}
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_levenetest}}, \code{\link{bf_test}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=levene_test(Pain ~ factor(Carrying)))
#'
#' @export
levene_test <- function(y,
                        x,
                        formula,
                        data){

  call <- match.call()

  if(!inherits(y,"formula")){
    eval_prop <-
      eval(call[[1L]], parent.frame())
    df <-
      data.frame(y, x)
  } else {
    eval_prop <-
      eval(call[[2]], parent.frame())
    fit <-
      lm(eval_prop, y=TRUE, x=TRUE)
    nr_var <-
      attr(fit$terms, "term.labels")
    if(length(nr_var) > 1)
      stop("Include only one independent categorical variable")
    df <-
      fit$model
    names(df) <-
      c("y", "x")
  }
  df$x <-
    factor(df$x)
  # center on subgroup means
  df_new <- df %>%
    group_by(x) %>%
    mutate(resd = abs(y-mean(y))) %>%
    dplyr::select(x, .data$resd)
  fit_new <-
    lm(resd ~ x, data=df_new)
  fstats <-
    summary(fit_new)$'fstatistic'
  qhat <-
    coef(fit_new)
  ui <-
    vcov(fit_new)
  dfcom <-
    df.residual(fit_new)
  obj <-
    list(fstats=fstats, qhat=qhat, vcov=ui, dfcom=dfcom)
  return(obj)
}
