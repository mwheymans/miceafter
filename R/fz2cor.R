#' Fisher z back transformation of correlation coefficient
#'
#' \code{fz2cor} Fisher z back transformation of correlation coefficient
#'
#' @param z value of the correlation coefficient on z scale.
#'
#' @return correlation coefficient on correlation scale.
#'
#' @author Martijn Heymans, 2022
#'
#' @examples
#'  fz2cor(z=0.631)
#'
#' @export
fz2cor <- function(z){
  r <- (exp(2*z) - 1) / (exp(2*z) + 1)
  return(r)
}
