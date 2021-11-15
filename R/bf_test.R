#' Calculates the Brown-Forsythe test.
#'
#' \code{bf_test} Calculates the Brown-Forsythe test.
#'
#' @param x categorical variable.
#' @param y numeric response variable.
#' @param formula A formula object to specify the model as normally
#'  used by glm. Use 'factor' to define the grouping variable.
#'
#'@return An object containing the following objects:
#'  \itemize{
#'  \item  \code{fstats} F-test value, including numerator and
#'   denominator degrees of freedom.
#'  \item  \code{qhat} pooled coefficients from fit
#'  \item  \code{vcov} variance-covariance matrix
#'}
#'
#' @author Martijn Heymans, 2021
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
    df <- fit$model
    names(df) <- c("y", "x")
  }
  df$x <- factor(df$x)
  # center on subgroup means
  df_new <- df %>%
    group_by(x) %>%
    mutate(resd = abs(y-median(y))) %>%
    dplyr::select(x, resd)
  fit_new <- lm(resd ~ x, data=df_new)
  fstats <- summary(fit_new)$'fstatistic'
  qhat <- coef(fit_new)
  ui <- vcov(fit_new)
  dfcom <- df.residual(fit_new)
  obj <- list(fstats=fstats, qhat=qhat, vcov=ui)
  return(obj)
}
