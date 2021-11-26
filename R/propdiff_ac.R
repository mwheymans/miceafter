#' Calculates the difference between proportions and standard error
#'  according to method Agresti-Caffo
#'
#' \code{propdiff_ac} Calculates the difference between proportions
#'  and standard error according to method Agresti-Caffo and
#'  degrees of freedom to be used in function \code{with.miceafter}
#'
#' @param formula A formula object to specify the model as normally used by glm.
#' @param data An objects of class \code{mids}, created by
#'  \code{make_mids} or after a call to function \code{mice}.
#'  If \code{data} is of type \code{data.frame}, use
#'  \code{make_mids} to convert to \code{mids} object.
#'
#' @return The Agresti-Caffo difference between proportions and the standard error.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.miceafter}}, \code{\link{pool_propdiff_ac}}
#'
#' @examples
#' imp_dat <- make_mids(lbpmilr, impvar="Impnr")
#' ra <- with.miceafter(imp_dat, expr=propdiff_ac(Chronic ~ Gender))
#'
#' @export
propdiff_ac <- function(formula, data){

  mf_call <- match.call()
  mf_call[[1L]] <- quote(stats::model.frame)
  mf_call <- eval(mf_call, parent.frame())
  X <- model.frame(mf_call)

  fm <- terms(mf_call)
  outcome <- attr(fm, "variables")[[2]]
  group <- attr(fm, "variables")[[3]]

  sub0 <- subset(X, get(group)==0)
  sub1 <- subset(X, get(group)==1)

  n0 <- nrow(sub0)
  n1 <- nrow(sub1)

  x0 <- nrow(subset(sub0, get(outcome)==1))
  x1 <- nrow(subset(sub1, get(outcome)==1))

  dfcom <- c(n0+n1)-1

  # Agresti-Caffo
  p0hat_ac <- (x0 + 1)/(n0 + 2)
  p1hat_ac <- (x1 + 1)/(n1 + 2)

  phat_diff_ac <- p1hat_ac - p0hat_ac
  se_phat_diff_ac <-
    sqrt((p0hat_ac * (1 - p0hat_ac)/(n0 + 2)) + (p1hat_ac * (1 - p1hat_ac)/(n1 + 2)))
  return(c(phat_diff_ac, se_phat_diff_ac, dfcom))
}
