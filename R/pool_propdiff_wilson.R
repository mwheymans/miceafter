#' Calculates the pooled single proportion confidence intervals according
#'  to Wilson across multiply imputed datasets.
#'
#' \code{pool_prop_wilson} Calculates the pooled single proportion and
#'  confidence intervals according to Wald across multiply imputed datasets
#'  and using Rubin's Rules.
#'
#' @param object An object of class 'raami' (repeated analysis after
#'   multiple imputation) after using \code{with.aftermi}.
#' @param conf.level Confidence level of the confidence intervals. Mostly
#'  set at 0.95.
#'
#' @details The \code{pool_propdiff_wilson} function uses information from separate
#'  exposure groups. It is therefore important to first use the \code{propdiff_wald}
#'  function and to set strata = TRUE in that function.
#'
#' @return The Proportion and the Confidence intervals according to Wilson.
#'
#' @references Yulia Sidi & Ofer Harel (2021): Difference Between Binomial
#'  Proportions Using Newcombe’s Method With Multiple Imputation for Incomplete
#'  Data, The American Statistician, DOI:10.1080/00031305.2021.1898468
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#' imp_list <- lbpmilr %>%
#'  group_split(Impnr, .keep = FALSE)
#'
#' res <- with.miceafter(imp_list,
#'   expr=propdiff_wald(Chronic ~ Radiation, strata = TRUE))
#' res <- pool_propdiff_wilson(res)
#' res
#'
#' @export
pool_propdiff_wilson <- function(object, conf.level=0.95)
{

  if(all(class(object)!="raami"))
    stop("object must be of class 'raami'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  obj1 <- lapply(object$statistics,
                 function(x) x[, c("prop1", "se1", "n1")])
  obj0 <- lapply(object$statistics,
                 function(x) x[, c("prop0", "se0", "n0")])

  obj1 <- list(statistics=obj1)
  obj0 <- list(statistics=obj0)
  class(obj0) <- class(obj1) <- 'raami'

  p0_pool <- pool_prop_wilson(obj0, conf.level=conf.level)
  p1_pool <- pool_prop_wilson(obj1, conf.level=conf.level)

  w0 <- p0_pool[-1]
  w1 <- p1_pool[-1]

  l0 <- w0[1]
  u0 <- w0[2]
  l1 <- w1[1]
  u1 <- w1[2]

  phat0 <- p0_pool[1]
  phat1 <- p1_pool[1]

  prop_diff <- phat1 - phat0

  lower <- prop_diff - sqrt((phat0-l0)^2 + (u1-phat1)^2)
  upper <- prop_diff + sqrt((u0-phat0)^2 + (phat1-l1)^2)
  output <- round(matrix(c(prop_diff, lower, upper), 1, 3), 4)
  colnames(output) <- c("Prop diff", "CI L Wilson", "CI U Wilson")
  return(output)
}
