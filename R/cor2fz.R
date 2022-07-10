#' Fisher z transformation of correlation coefficient
#'
#' \code{cor2fz} Fisher z transformation of correlation coefficient
#'
#' @param r value for the correlation coefficient.
#'
#' @return correlation coefficient on z scale.
#'
#' @author Martijn Heymans, 2022
#'
#' @examples
#'  cor2fz(r=0.65)
#'
#' @export
cor2fz <- function(r){
  z <- 1/2* log(( 1 + r) / ( 1 - r ))
  return(z)
}
