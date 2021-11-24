pool_scalar_RR <- function(est,
                           se,
                           logit_trans=FALSE,
                           conf.level=0.95,
                           dfcom=NULL){
  call <- match.call()
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
    (((dfcom) + 1) / ((dfcom) + 3)) * (dfcom) * (1-lambda)
  v_adj <-
    (v_old * v_obs) / (v_old + v_obs)
  alpha <- 1 - (1 - conf.level)/2
  t <- qt(alpha, v_adj)
  obj <- list(pool_est=mean_est, pool_se=se_total,
              t=t, r=r, dfcom=dfcom)
  return(obj)
}
