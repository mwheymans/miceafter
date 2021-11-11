pool_RR <- function(est, se, log_trans=FALSE, conf.level=0.95, df_com=NULL){
  call <- match.call()
  if(log_trans){
    source("log_trans.R")
    est_log <- log_trans(est, se)
    est <- est_log[, 1]
    se <- est_log[, 2]
  }
  m <- length(est)
  mean_est <- mean(est)
  var_w <-
    mean(se^2) # within variance
  var_b <-
    var(est) # between variance
  var_T <-
    var_w + (1 + (1/m)) * var_b # total variance
  se_total <-
    sqrt(var_T)
  r <-
    (1 + 1 / m) * (var_b / var_w)
  v_old <-
    (m - 1) * (1 + (1/r))^2
  lambda <-
    (var_b + (var_b/m))/var_T
  # Adjustment according to Barnard & Rubin (1999)
  v_obs <-
    (((df_com) + 1) / ((df_com) + 3)) * (df_com) * (1-lambda)
  v_adj <-
    (v_old * v_obs) / (v_old + v_obs)
  alpha <- 1 - (1 - conf.level)/2
  t <- qt(alpha, v_adj)
  obj <- list(pool_est=mean_est, pool_se=se_total,
              t=t, r=r, df_com=df_com)
  return(obj)
}
