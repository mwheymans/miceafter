#'  Rubin's Rules for scalar estimates
#'
#' \code{pool_scalar_RR} Applies Rubin's pooling Rules for scalar
#'  estimates
#'
#' @param est a numerical vector of parameter estimates.
#' @param se a numerical vector of standard error estimates.
#' @param logit_trans If TRUE logit transformation of parameter values
#'  is applied before pooling, if FALSE (default), pooling is done
#'  on the original parameter scale.
#' @param conf.level Used to calculate quantile of t or z distribution. Can
#'  be used to calculate confidence intervals. Default is for
#'  95% Confidence interval. See Details.
#' @param dfcom The complete data analysis degrees of freedom.
#'  (dfcom=n-k, with n the hypothetical complete data and k the number of
#'  fitted parameters).
#' @param statistic if TRUE the test statistic and p-value are
#'  provided, if FALSE (default) these are not shown.
#' @param df_small if TRUE (default) the (Barnard & Rubin) small sample
#'  correction for the degrees of freedom is applied, if FALSE the old
#'  number of degrees of freedom is calculated.
#' @param approxim if "tdistr" a t-distribution is used (default), if "zdistr"
#'  a z-distribution is used to derive a p-value according to the test statistic.
#'
#'@return A list object from which the following objects are extracted:
#'  \itemize{
#'  \item  \code{pool_est} the pooled parameter value.
#'  \item  \code{pool_se} the pooled standard error value.
#'  \item  \code{t} quantile of the t-distribution (to calculate
#'   confidence intervals).
#'  \item  \code{r} the relative increase in variance due to missing data.
#'  \item  \code{dfcom} complete data degrees of freedom.
#'  \item  \code{v_adj} adjusted degrees of freedom (according to
#'   Barnard and Rubin 1999)
#'}
#'
#' @details
#'  The t-value is the quantile value of the t-distribution that can
#'  be used to calculate confidence intervals according to
#'  \eqn{est_{pooled} +/- t_{1-\alpha/2} * se_{pooled}}. When statistic is
#'  TRUE the test statistic is calculated as
#'  \eqn{statistic = est{pooled}/se{pooled}}. The p-value is than
#'  derived using the t-distribution and adjusted degrees of freedom.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#' est <- c(0.4, 0.6, 0.8)
#' se <- c(0.02, 0.05, 0.03)
#' res <- pool_scalar_RR(est, se, dfcom=500)
#' res
#'
#' @export
pool_scalar_RR <- function(est,
                           se,
                           logit_trans=FALSE,
                           conf.level=0.95,
                           statistic=FALSE,
                           dfcom=NULL,
                           df_small=TRUE,
                           approxim="tdistr")
{
  call <- match.call()
  if(is_empty(dfcom))
    stop("dfcom is not defined")
  if(approxim=="zdistr")
    df_small=FALSE
  if(logit_trans){
    est_logit <- logit_trans(est, se)
    est <- est_logit[, 1]
    se <- est_logit[, 2]
  }
  m <- length(est)
  mean_est <- mean(est)
  var_w <-
    mean(se^2) # within variance
  var_b <-
    var(est) # between variance
  var_T <-
    var_w + (1 + 1/m) * var_b # total variance
  se_total <-
    sqrt(var_T)
  r <-
    (1 + (1 / m)) * (var_b / var_w)
  v_old <-
    (m - 1) * (1 + (1/r))^2
  lambda <-
    (var_b + (var_b/m))/var_T
  if(df_small){
    # Adjustment according to Barnard & Rubin (1999)
    v_obs <-
      (((dfcom) + 1) / ((dfcom) + 3)) * (dfcom) * (1-lambda)
    v_adj <-
      (v_old * v_obs) / (v_old + v_obs)
  }
  if(var_b == 0){
    v_adj <- 100000000
  }
  alpha <- 1 - (1 - conf.level)/2
  if(approxim=="tdistr")
    if(df_small){
      t <- qt(alpha, v_adj)
    } else{
      t <- qt(alpha, v_old)
    }
  if(approxim=="zdistr")
    t <- qnorm(alpha)
  if(var_b == 0){
    obj <- list(pool_est=mean_est, pool_se=se_total,
                t=t, dfcom=dfcom)
  } else {
    if(df_small) {
      obj <- list(pool_est=mean_est, pool_se=se_total,
                  t=t, r=r, dfcom=dfcom, v_adj=v_adj)
    } else {
      obj <- list(pool_est=mean_est, pool_se=se_total,
                  t=t, r=r, dfcom=dfcom, v_old=v_old)
    }
  }
  if(statistic){
      statistic <- mean_est/se_total
    if(approxim=="tdistr")
      if(df_small){
        pval <- 2*(1-pt(abs(statistic), v_adj))
      } else{
        pval <- 2*(1-pt(abs(statistic), v_old))
      }
    if(approxim=="zdistr")
      pval <- 2*(1-pnorm(abs(statistic)))
    if(var_b==0){
      obj <- list(pool_est=mean_est, pool_se=se_total,
                  t=t, dfcom=dfcom,
                  statistic=statistic, pval=pval)
    } else {
      if(df_small) {
        obj <- list(pool_est=mean_est, pool_se=se_total,
                    t=t, r=r, dfcom=dfcom, v_adj=v_adj,
                    statistic=statistic, pval=pval)
      } else {
        obj <- list(pool_est=mean_est, pool_se=se_total,
                    t=t, r=r, dfcom=dfcom, v_old=v_old,
                    statistic=statistic, pval=pval)
      }
    }
  }
  return(obj)
}
