#' Calculates the pooled correlation coefficient and Confidence intervals
#'
#' \code{pool_cor} Calculates the pooled correlation coefficient and
#'  Confidence intervals.
#'
#' @param data An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis'.) or a m x 2 matrix with C-index
#'  values and standard errors in the first and second column.
#'  For the latter option dfcom has to be provided.
#' @param conf.level conf.level Confidence level of the confidence intervals.
#' @param dfcom Number of completed-data analysis degrees of freedom.
#'  Default number is taken from function \code{cindex}
#' @param statistic if TRUE (default) the test statistic and p-value are
#'  provided, if FALSE these are not shown. See details.
#' @param df_small if TRUE (default) the (Barnard & Rubin) small sample
#'  correction for the degrees of freedom is applied, if FALSE the old
#'  number of degrees of freedom is calculated.
#' @param approxim if "tdistr" a t-distribution is used (default), if "zdistr"
#'  a z-distribution is used to derive a p-value for the test statistic.
#'
#' @details Rubin's Rules are used for pooling. The correlation coefficient is
#'  first transformed using Fisher z transformation (function \code{cor2fz}) before
#'  pooling and finally back transformed (function \code{fz2cor}). The test
#'  statistic and p-values are obtained using the Fisher z transformation.
#'
#'@return An object of class \code{mipool} from which the following objects
#' can be extracted:
#'  \itemize{
#'  \item  \code{cor} correlation coefficient
#'  \item  \code{SE} standard error
#'  \item  \code{t} t-value (for confidence interval)
#'  \item  \code{low_r} lower limit of confidence interval
#'  \item  \code{high_r} upper limit of confidence interval
#'  \item  \code{statistic} test statistic
#'  \item  \code{pval} p-value
#' }
#' @author Martijn Heymans, 2022
#'
#' @seealso \code{\link{with.milist}}, \code{\link{cor_est}}
#'
#' @examples
#'
#'  imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#'  res_stats <- with(data=imp_dat,
#'   expr = cor_est(y=BMI, x=Age))
#'  res <- pool_cor(res_stats)
#'  res
#'
#' @export
pool_cor <- function(data,
                     conf.level = 0.95,
                     dfcom=NULL,
                     statistic=TRUE,
                     df_small=TRUE,
                     approxim="tdistr")
{

  #if(data$call$expr$method=="kendall")
  #  statistic=FALSE
  if(inherits(data, 'mistats')){
    ra <- data.frame(do.call("rbind", data$statistics))
    colnames(ra) <- c("est", "se", "dfcom")
  } else {
    if (!all(!is.data.frame(data) | !is.matrix(data)))
      stop("Data should be a data frame or matrix")
    ra <- data
    if(is_empty(dfcom))
      stop("Include number of complete data degrees of freedom")
  }

  if(is_empty(dfcom)){
    dfcom <- ra[,3][1]
  } else {
    dfcom <- dfcom
  }

  # Fisher z transformation
  ra[,1] <- cor2fz(ra[, 1])

  # Pooling Fisher z values
  pool_est <- pool_scalar_RR(est=ra[, 1],
                             se=ra[, 2],
                             logit_trans=FALSE,
                             conf.level=0.95,
                             statistic = statistic,
                             dfcom=ra[,3][1],
                             df_small=df_small,
                             approxim=approxim)

  low_z <-
    pool_est$pool_est - (pool_est$t * pool_est$pool_se)
  high_z <-
    pool_est$pool_est + (pool_est$t * pool_est$pool_se)

  # Back transform
  rval <-
    fz2cor(pool_est$pool_est)
  low_r <-
    fz2cor(low_z)
  high_r <-
    fz2cor(high_z)

  if(statistic){
    output <- matrix(c(rval, pool_est$pool_se,
                       pool_est$t, low_r, high_r,
                       pool_est$statistic,
                       pool_est$pval), 1, 7)
    colnames(output) <- c("Cor", "SE", "t",
                          c(paste(conf.level*100, "CI low"),
                            paste(conf.level*100, "CI high")),
                          "statistic", "pval")

  } else {
    output <- matrix(c(rval, pool_est$pool_se,
                       pool_est$t, low_r, high_r), 1, 5)
    colnames(output) <- c("Cor", "SE", "t",
                          c(paste(conf.level*100, "CI low"),
                            paste(conf.level*100, "CI high")))
  }
  class(output) <- 'mipool'
  return(output)
}
