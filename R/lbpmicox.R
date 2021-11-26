#' Survival data of 265 Low Back Pain Patients
#'
#' A data frame with 10 multiply imputed datasets of 265 observations each on
#'  17 variables related to low back pain.
#'
#' @format A data frame with 2650 observations on the following 18 variables.
#'  \describe{
#'    \item{Impnr}{a numeric vector}
#'    \item{patnr}{a numeric vector}
#'    \item{Status}{dichotomous event}
#'    \item{Time}{continuous follow up time variable}
#'    \item{Duration}{continuous}
#'    \item{Previous}{dichotomous}
#'    \item{Radiation}{dichotomous}
#'    \item{Onset}{dichotomous}
#'    \item{Age}{continuous}
#'    \item{Tampascale}{continuous}
#'    \item{Pain}{continuous}
#'    \item{Function}{continuous}
#'    \item{Satisfaction}{categorical}
#'    \item{JobControl}{continuous}
#'    \item{JobDemand}{continuous}
#'    \item{Social}{continuous}
#'    \item{Expectation}{a numeric vector}
#'    \item{Expect_cat}{categorical}
#'}
#'
#' @examples
#'  data(lbpmicox)
#'  ## maybe str(lbpmicox)
#'
"lbpmicox"
