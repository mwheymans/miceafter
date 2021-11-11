back_trans <- function(est, se, t=NULL){
  # Backtransform
  inv.est <- exp(est) / (1 + exp(est))
  inv.est.u <- exp(est + (t*se)) /
    (1 + exp(est + (t*se)))
  inv.est.l <- exp(est - (t*se)) /
    (1 + exp(est - (t*se)))
  obj <- matrix(c(inv.est, inv.est.l, inv.est.u),
                1, 3, byrow = T)
  return(obj)
}
