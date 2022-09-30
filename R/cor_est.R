#' Calculates the correlation coefficient
#'
#' \code{cor_est} Calculates the correlation coefficient and
#'  standard error to be used in function \code{with.miceafter}.
#'
#' @param y name of numeric vector variable.
#' @param x name of numeric vector variable.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#' @param method a character string indicating which correlation coefficient
#'  is used for the test. One of "pearson" (default), "kendall", or "spearman".
#' @param se_method Method to calculate standard error. If "normal" than
#'  the basic method is used, if "fieler" than the method of Fieler is used.
#'  See details.
#'
#' @details The basic method to calculate the standard error is by:
#'
#'  \deqn{se = \sqrt(\frac{1}{n-3})}
#'
#'  For the Spearman correlation coefficients se_method "fieller"
#'  is calculated as:
#'
#'  \deqn{se = \sqrt(\frac{1.06}{n-3})}
#'
#'  For the Kendall correlation coefficients se_method "fieller"
#'  is calculated as:
#'
#'  \deqn{se = \sqrt(\frac{0.437}{n-4})}
#'
#' @return The correlation coefficient, standard error and complete data
#'  degrees of freedom (dfcom).
#'
#' @author Martijn Heymans, 2022
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_cor}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=cor_est(y=BMI, x=Age))
#'
#' @export
cor_est <- function(y,
                    x,
                    data,
                    method="pearson",
                    se_method="normal"){

  call <- match.call()
  eval_prop <-
    eval(call[[1L]], parent.frame())

  n <- length(x)

  if(se_method=="normal")
    if(n<4)
      stop("Sample size must be > 3")
  secor <- sqrt(1/(n-3)) # normal cor2fz approximation method

  corval <- cor(x, y, method = method)

  if(method=="spearman") {
    #secor <- sqrt((1+(corval^2/2))/(n-3)) Method BW
    if(se_method=="fieller")
      secor <- sqrt(1.06/(n-3))
  }
  if(method=="kendall") {
    #secor <- sqrt((2*(2*n+5))/ (9*n*(n-1))) Siegel (1956)
    if(se_method=="fieller")
      secor <- sqrt(0.437/(n-4))
  }
  output <- c(corval, secor, n)
  return(output)
}

