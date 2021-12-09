#' Takes the inverse of a logit transformed value
#'
#' \code{invlogit} Takes the inverse of a logit transformed
#'  value
#'
#' @param est A parameter estimate on the logit scale.
#'
#' @return back transformed value.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'  invlogit(est=1.39)
#'
#' @export
invlogit <- function(est){
  inv.est <- exp(est) / (1 + exp(est))
  return(inv.est)
}
