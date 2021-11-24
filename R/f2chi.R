#' Converts F-values into Chi Square values
#'
#' \code{f2chi} convert F to Chi-square values.
#'
#' @param f a vector of F values.
#' @param df_num single value for the numerator degrees of freedom of the F test.
#'
#' @return The Chi square values.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'   f2chi(c(5.83, 4.95, 3.24, 6.27, 4.81), 5)
#'
#' @export
f2chi <- function(f, df_num){
  df_num*f
}

