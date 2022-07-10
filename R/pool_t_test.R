#' Calculates the pooled t-test and Confidence intervals
#'
#' \code{pool_t_test} Calculates the pooled t-test, confidence intervals
#'  and p-value.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis'.)
#' @param conf.level conf.level Confidence level of the confidence intervals.
#' @param dfcom Number of completed-data analysis degrees of freedom.
#'  Default number is taken from function \code{cindex}.
#' @param statistic if TRUE (default) the test statistic and p-value are
#'  provided, if FALSE these are not shown.
#'
#'@return An object of class \code{mipool} from which the following objects
#' can be extracted:
#'  \itemize{
#'  \item  \code{Mean diff} Difference between means
#'  \item  \code{SE} standard error
#'  \item  \code{t} t-value (for confidence interval)
#'  \item  \code{low_r} lower limit of confidence interval
#'  \item  \code{high_r} upper limit of confidence interval
#'  \item  \code{statistic} test statistic
#'  \item  \code{pval} p-value
#' }
#' @author Martijn Heymans, 2022
#'
#' @seealso \code{\link{with.milist}}, \code{\link{t_test}}
#'
#' @examples
#'
#'  imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#'  res_stats <- with(data=imp_dat,
#'   expr = t_test(Pain ~ Gender, var_equal=TRUE, paired=FALSE))
#'  res <- pool_t_test(res_stats)
#'  res
#'
#' @export
pool_t_test <- function(object,
                        conf.level=0.95,
                        dfcom=NULL,
                        statistic=FALSE){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  ra <-
    data.frame(do.call("rbind", object$statistics))
  colnames(ra) <-
    c("est", "se", "dfcom")

  if(is_empty(dfcom)){
    dfcom <- mean(ra$dfcom)
  } else {
    dfcom <- dfcom
  }

  pool_est <-
    pool_scalar_RR(est=ra$est,
                   se=ra$se,
                   logit_trans=FALSE,
                   conf.level = conf.level,
                   statistic=statistic,
                   dfcom=dfcom,
                   df_small=TRUE,
                   approxim="tdistr")

  low_ci <-
    pool_est$pool_est - pool_est$t*pool_est$pool_se
  high_ci <-
    pool_est$pool_est + pool_est$t*pool_est$pool_se

  if(statistic){
    output <- matrix(c(pool_est$pool_est, pool_est$pool_se,
                       pool_est$t, low_ci, high_ci,
                       pool_est$statistic,
                       pool_est$pval), 1, 7)
    colnames(output) <- c("Mean diff", "SE", "t",
                          c(paste(conf.level*100, "CI low"),
                            paste(conf.level*100, "CI high")),
                          "statistic", "pval")

  } else {
    output <- matrix(c(pool_est$pool_est, pool_est$pool_se,
                       pool_est$t, low_ci, high_ci), 1, 5)
    colnames(output) <- c("Mean diff", "SE", "t",
                          c(paste(conf.level*100, "CI low"),
                            paste(conf.level*100, "CI high")))
  }
  class(output) <- 'mipool'
  return(output)
}
