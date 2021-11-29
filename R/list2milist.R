#'  Turns a list object with multiply imputed datasets into an object
#'   of class 'milist'.
#'
#' \code{list2milist} Turns a list with multiply imputed datasets
#'  into an object of class 'milist' to be further used by 'with.milist'
#'
#' @param data an object of class 'list'.
#'
#' @return an object of class 'milist'
#'
#' @author Martijn Heymans, 2021
#'
#' @export
list2milist <- function(data){
  if (!class(data)=="list")
    stop("object must be of class 'list'")
  if (length(data)<2)
    stop("List must contain more than 1 imputed dataset")

  class(data) <- "milist"
  return(data)
}
