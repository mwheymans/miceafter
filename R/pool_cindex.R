#' Calculates the pooled C-index and Confidence intervals
#'
#' \code{pool_cindex} Calculates the pooled C-index and Confidence intervals.
#'
#' @param data An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis'.) or a m x 2 matrix with correlation
#'  coefficients and standard errors in the first and second column.
#'  For the latter option dfcom has to be provided.
#' @param conf.level conf.level Confidence level of the confidence intervals.
#' @param dfcom Number of completed-data analysis degrees of freedom.
#'  Default number is taken from function \code{cindex}
#'
#' @details Rubin's Rules are used for pooling. The C-index values are log
#'  transformed before pooling and finally back transformed.
#'
#' @return The pooled c-index value and the confidence intervals.
#'
#' @author Martijn Heymans, 2021
#'
#'
#' @section Vignettes:
#'   https://mwheymans.github.io/miceafter/articles/pooling_cindex.html
#'
#' @seealso \code{\link{with.milist}}, \code{\link{cindex}}
#'
#' @examples
#'
#'  # Logistic Regression
#'  imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#'  res_stats <- with(data=imp_dat,
#'   expr = cindex(glm(Chronic ~ Gender + Radiation,
#'   family=binomial)))
#'  res <- pool_cindex(res_stats)
#'  res
#'
#'  # Cox regression
#'  library(survival)
#'  imp_dat <- df2milist(lbpmicox, impvar="Impnr")
#'  res_stats <- with(data=imp_dat,
#'    expr = cindex(coxph(Surv(Time, Status) ~ Pain + Radiation)))
#'  res <- pool_cindex(res_stats)
#'  res
#'
#' @export
pool_cindex <- function(data,
                        conf.level = 0.95,
                        dfcom=NULL)
{

  if(inherits(data, 'mistats')){
    ra <-
      data.frame(do.call("rbind", data$statistics))
    colnames(ra) <-
      c("est", "se", "dfcom")
  } else {
    if (!all(!is.data.frame(data) | !is.matrix(data)))
      stop("Data should be a data frame or matrix")
    ra <-
      data
    if(is_empty(dfcom))
      stop("Include number of complete data degrees of freedom")
  }

  if(is_empty(dfcom)){
    dfcom <- ra[,3][1]
  } else {
    dfcom <- dfcom
  }

  est_log <-
    pool_scalar_RR(est=ra[, 1], se=ra[, 2], logit_trans=TRUE,
                            conf.level=conf.level, dfcom=dfcom)
  est_orig <-
    invlogit_ci(est=est_log$pool_est,
                         se=est_log$pool_se,
                         crit.value = est_log$t)

  output <- matrix(est_orig, 1, 4)
  if(output[4] > 1) output[4] <- 1.00
  if(output[3] < 0) output[3] <- 0.00

  colnames(output) <- c("C-index", "Critical value",
                        c(paste(conf.level*100, "CI low"),
                          paste(conf.level*100, "CI high")))
  class(output) <- "mipool"
  return(output)
}
