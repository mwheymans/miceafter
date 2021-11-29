#' Turns a 'mice::mids' object into an object of class 'milist'
#'  to be further used by 'miceafter::with'
#'
#' \code{mids2milist} Turns a 'mice::mids' object into an object
#'  of class 'milist' to be further used by 'miceafter::with'
#'
#' @param data a 'mice::mids' object
#' @param keep if TRUE the grouping column is kept, if FALSE
#'  (default) the grouping column is not kept.
#'
#' @return an object of class 'milist'
#'
#' @author Martijn Heymans, 2021
#'
#' @export
mids2milist <- function(data, keep=FALSE){
  if (!is.mids(data))
    stop("object must be of class 'mids'")

  df <- mice::complete(data, action="long",
                       include=FALSE)
  imp_list <- df %>%
    group_split(df$.imp, .keep = keep)
  class(imp_list) <- "milist"
  return(imp_list)
}
