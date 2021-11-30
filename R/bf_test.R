#' Calculates the Brown-Forsythe test.
#'
#' \code{bf_test} Calculates the Brown-Forsythe test for homogeneity
#'  of variance across groups, coefficients, variance-covariance matrix,
#'  and degrees of freedom.
#'
#' @param y numeric response variable.
#' @param x categorical variable.
#' @param formula A formula object to specify the model as normally
#'  used by glm. Use 'factor' to define the grouping variable.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#'
#' @details The Levene's test centers around means to calculate
#'  outcome residuals, the Brown-Forsythe test on the median.
#'
#'@return An object containing the following objects are extracted:
#'  \itemize{
#'  \item  \code{fstats} F-test value, including numerator and
#'   denominator degrees of freedom.
#'  \item  \code{qhat} pooled coefficients from fit.
#'  \item  \code{vcov} variance-covariance matrix.
#'  \item  \code{dfcom} degrees of freedom obtained from \code{df.residual}.
#'}
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#'  ra <- with(imp_dat, expr=bf_test(Pain ~ factor(Carrying)))
#'
#' @export
bf_test <- function(y, x, formula, data){

  call <- match.call()

  if(!inherits(y,"formula")){
    eval_prop <- eval(call[[1L]], parent.frame())
    df <- data.frame(y, x)
  } else {
    eval_prop <- eval(call[[2]], parent.frame())
    fit <- lm(eval_prop, y=TRUE, x=TRUE)
    nr_var <- attr(fit$terms, "term.labels")
    if(length(nr_var) > 1)
      stop("Include single independent categorical variable only")
    df <- fit$model
    names(df) <- c("y", "x")
  }
  df$x <- factor(df$x)
  # center on subgroup means
  df_new <- df %>%
    group_by(x) %>%
    mutate(resd = abs(y-median(y))) %>%
    dplyr::select(x, .data$resd)
  fit_new <- lm(resd ~ x, data=df_new)
  fstats <- summary(fit_new)$'fstatistic'
  qhat <- coef(fit_new)
  ui <- vcov(fit_new)
  dfcom <- df.residual(fit_new)
  obj <- list(fstats=fstats, qhat=qhat, vcov=ui, dfcom=dfcom)
  return(obj)
}
