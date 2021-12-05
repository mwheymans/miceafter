#' Pools Linear and Logistic regression models across multiply imputed data.
#'
#' \code{pool_glm} Pools Linear and Logistic regression models across multiply
#'  imputed data, using pooling methods RR, D1, D2, D3, D4 and MPR.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analysis').
#' @param method A character vector to indicate the pooling method for p-values
#'  to pool the total model or used during predictor selection. This can be
#'  "RR", D1", "D2", "D3", "D4", or "MPR". See details for more information.
#'  Default is "RR".
#'
#' @details The basic pooling procedure to derive pooled coefficients, standard errors, 95
#'  confidence intervals and p-values is Rubin's Rules (RR). However, RR is only possible when
#'  the model includes continuous and dichotomous variables. Specific procedures are
#'  available when the model also included categorical (> 2 categories) or restricted cubic spline
#'  variables. These pooling methods are: “D1” is pooling of the total covariance matrix,
#'  ”D2” is pooling of Chi-square values, “D3” and "D4" is pooling Likelihood ratio statistics
#'  (method of Meng and Rubin) and “MPR” is pooling of median p-values (MPR rule).
#'  Spline regression coefficients are defined by using the rcs function for restricted cubic
#'  splines of the rms package. A minimum number of 3 knots as defined under knots is required.
#'
#'@return An object of class \code{mipool} (multiply imputed pooled models) from
#'  which the following objects can be extracted:
#'  \itemize{
#'  \item  \code{pmodel} pooled model
#'  \item  \code{pmultiparm} pooled p-values according to pooling method
#' }
#'
#' @references Eekhout I, van de Wiel MA, Heymans MW. Methods for significance testing of categorical
#'   covariates in logistic regression models after multiple imputation: power and applicability
#'   analysis. BMC Med Res Methodol. 2017;17(1):129.
#' @references Enders CK (2010). Applied missing data analysis. New York: The Guilford Press.
#' @references Meng X-L, Rubin DB. Performing likelihood ratio tests with multiply-imputed data sets.
#'   Biometrika.1992;79:103-11.
#' @references van de Wiel MA, Berkhof J, van Wieringen WN. Testing the prediction error difference between
#'   2 predictors. Biostatistics. 2009;10:550-60.
#' @references Marshall A, Altman DG, Holder RL, Royston P. Combining estimates of interest in prognostic
#'   modelling studies after multiple imputation: current practice and guidelines. BMC Med Res Methodol.
#'   2009;9:57.
#' @references Van Buuren S. (2018). Flexible Imputation of Missing Data. 2nd Edition. Chapman & Hall/CRC
#'   Interdisciplinary Statistics. Boca Raton.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'   dat_list <- df2milist(lbpmilr, impvar="Impnr")
#'   ra <- with(data=dat_list, expr = glm(Chronic ~ factor(Carrying) + Radiation + Age))
#'   poolm <- pool_glm(ra, method="D1")
#'   poolm
#'
#' @export
pool_glm <- function(object, method="D1")
{

  call <- match.call()

  if(!class(object)=='mistats')
    stop("Object must be of class 'mistats'")

  # set regression formula fm
  fm <-
    formula(object$call[[3]][[2]])
  nimp <- length(object$statistics)

  imp_dat <- list()
  for (i in 1:nimp) {
    imp_dat[[i]] <-
      object$statistics[[i]]$model
  }
  names_temp <- clean_P(names(imp_dat[[1]]))

  imp_id <- rep(1:nimp, each=nrow(imp_dat[[1]]))
  imp_dat <- data.frame(imp_id, do.call("rbind", imp_dat))
  names(imp_dat) <- c("imp_id", names_temp)

  if(object$statistics[[1]]$family[[1]]=="binomial"){

    pmodel <- glm_mi(data=imp_dat, formula = fm,
                     p.crit = 1, direction=NULL, nimp=nimp, impvar="imp_id",
                     keep.predictors = NULL, method=method, model_type="binomial")
  }
  if(object$statistics[[1]]$family[[1]]=="gaussian"){

    pmodel <- glm_mi(data=imp_dat, formula = fm,
                     p.crit = 1, direction=NULL, nimp=nimp, impvar="imp_id",
                     keep.predictors = NULL, method=method, model_type="linear")
  }

  output <- list(pmodel=pmodel$RR_model_final[[1]],
                 pmultiparm=pmodel$multiparm_final[[1]])
  class(output) <- 'mipool'
  return(output)
}
