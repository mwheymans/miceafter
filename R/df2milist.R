#'  Turns an object of class 'data.frame', 'tbl_df' or 'tbl' into
#'  an object of class 'milist' to be further used
#'  by 'miceafter::with'
#'
#' \code{df2milist} Turns an object of class 'data.frame', 'tbl_df'
#'  or 'tbl' into an object of class 'milist' to be further used
#'  by 'miceafter::with'
#'
#' @param data an object of class 'data.frame', 'tbl_df' or 'tbl'
#' (tibble).
#' @param impvar A character vector. Name of the variable that
#'  distinguishes the imputed datasets.
#' @param keep if TRUE the grouping column is kept, if FALSE (default)
#'  the grouping column is not kept.
#'
#' @return an object of class 'milist'
#'
#' @author Martijn Heymans, 2021
#'
#' @export
df2milist <- function(data, impvar, keep=FALSE){
  if (!any(class(data)=="data.frame"))
    stop("object must be of class 'data.frame'")

  imp_list <- data %>%
    group_split(get(impvar), .keep = keep)
  class(imp_list) <- "milist"
  return(imp_list)
}
