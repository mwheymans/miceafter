#' Calculates the difference between proportions and standard error
#'  according to method Agresti-Caffo
#'
#' \code{propdiff_ac} Calculates the difference between proportions
#'  and standard error according to method Agresti-Caffo and
#'  degrees of freedom to be used in function \code{with.miceafter}
#'
#' @param y 0-1 binary response variable.
#' @param x 0-1 binary independent variable.
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
#' @seealso \code{\link{with.milist}}, \code{\link{pool_propdiff_ac}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=propdiff_ac(Chronic ~ Radiation))
#'
#' @export
propdiff_ac <- function(y, x, formula, data){

  call <- match.call()

  names_var <- all.vars(call, functions = FALSE)
  if(length(names_var) > 2)
    stop("Include only one independent variable in x or formula")

  if(!inherits(y,"formula")){
    eval_prop <- eval(call[[1L]], parent.frame())
    X <- data.frame(y, x)
    if(!all(X$x==1 | X$x==0))
      stop("x variable should be a 0 - 1 variable")
    if(!all(X$y==1 | X$y==0))
      stop("y variable should be a 0 - 1 variable")

    sub1 <- subset(X, x==1)
    sub0 <- subset(X, x==0)

    n1 <- nrow(sub1)
    n0 <- nrow(sub0)

    x1 <- nrow(subset(sub1, y==1))
    x0 <- nrow(subset(sub0, y==1))
  } else {
    call <- eval(call[[2L]], parent.frame())
    X <- model.frame(call)

    fm <- terms(call)
    outcome <- attr(fm, "variables")[[2]]
    group <- attr(fm, "variables")[[3]]

    sub1 <- subset(X, get(group)==1)
    sub0 <- subset(X, get(group)==0)

    n1 <- nrow(sub1)
    n0 <- nrow(sub0)

    x1 <- nrow(subset(sub1, get(outcome)==1))
    x0 <- nrow(subset(sub0, get(outcome)==1))
  }

  dfcom <- c(n0+n1)-1

  # Agresti-Caffo
  p0hat_ac <- (x0 + 1)/(n0 + 2)
  p1hat_ac <- (x1 + 1)/(n1 + 2)

  phat_diff_ac <- p1hat_ac - p0hat_ac
  se_phat_diff_ac <-
    sqrt((p0hat_ac * (1 - p0hat_ac)/(n0 + 2)) + (p1hat_ac * (1 - p1hat_ac)/(n1 + 2)))
  output <- matrix(c(phat_diff_ac, se_phat_diff_ac, dfcom), 1, 3)
  colnames(output) <- c("Prop diff AC", "SE", "dfcom")
  return(output)
}
