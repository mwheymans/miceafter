#' Pools and selects Linear and Logistic regression models across multiply imputed data.
#'
#' \code{pool_glm} Pools and selects Linear and Logistic regression models across multiply
#'  imputed data, using pooling methods RR, D1, D2, D3, D4 and MPR.
#'
#' @param object An object of class 'mistats' ('Multiply Imputed
#'  Statistical Analyses').
#' @param method A character vector to indicate the multiparameter pooling method to pool
#'  the total model or used during model selection. This can be "RR", D1", "D2", "D3",
#'  "D4", or "MPR". See details for more information. Default is "RR".
#' @param p.crit A numerical scalar. P-value selection criterium. A value of 1
#'   provides the pooled model without selection.
#' @param keep.predictors A single string or a vector of strings including the variables that are forced
#'   in the model during model selection. All type of variables are allowed.
#' @param direction The direction for model selection, "BW" means backward selection and "FW"
#'   means forward selection.
#'
#' @details The basic pooling procedure to derive pooled coefficients, standard errors, 95
#'  confidence intervals and p-values is Rubin's Rules (RR). However, RR is only possible when
#'  the model includes continuous and dichotomous variables. Multiparameter pooling methods are
#'  available when the model also included categorical (> 2 categories) variables.
#'  These pooling methods are: “D1” is pooling of the total covariance matrix,
#'  ”D2” is pooling of Chi-square values, “D3” and "D4" is pooling Likelihood ratio statistics
#'  (method of Meng and Rubin) and “MPR” is pooling of median p-values (MPR rule).
#'  For pooling restricted cubic splines using the 'rcs' function of of the rms package,
#'  use function 'glm_mi'.
#'
#'  A typical formula object has the form \code{Outcome ~ terms}. Categorical variables has to
#'  be defined as \code{Outcome ~ factor(variable)}. Interaction terms can be defined as
#'  \code{Outcome ~ variable1*variable2} or \code{Outcome ~ variable1 + variable2 + variable1:variable2}.
#'  All variables in the terms part have to be separated by a "+".
#'
#'@return An object of class \code{mipool} (multiply imputed pooled models) from
#'  which the following objects can be extracted:
#'  \itemize{
#'  \item  \code{pmodel} pooled model (at last selection step)
#'  \item  \code{pmultiparm} pooled p-values according to multiparameter test method
#'   (at last selection step)
#'  \item  \code{pmodel_step} pooled model (at each selection step)
#'  \item  \code{pmultiparm_step} pooled p-values according to multiparameter test method
#'   (at each selection step)
#'  \item  \code{multiparm_final} pooled p-values at final step according to pooling method
#'  \item  \code{multiparm_out} (only when direction = "FW") pooled p-values of removed predictors
#'  \item  \code{formula_final} formula object at final step
#'  \item  \code{formula_initial} formula object at final step
#'  \item  \code{predictors_in} predictors included at each selection step
#'  \item  \code{predictors_out} predictors excluded at each step
#'  \item  \code{impvar} name of variable used to distinguish imputed datasets
#'  \item  \code{nimp} number of imputed datasets
#'  \item  \code{Outcome} name of the outcome variable
#'  \item  \code{method} selection method
#'  \item  \code{p.crit} p-value selection criterium
#'  \item  \code{call} function call
#'  \item  \code{model_type} type of regression model used
#'  \item  \code{direction} direction of predictor selection
#'  \item  \code{predictors_final} names of predictors in final selection step
#'  \item  \code{predictors_initial} names of predictors in start model
#'  \item  \code{keep.predictors} names of predictors that were forced in the model
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
pool_glm <- function(object,
                     method="D1",
                     p.crit=1,
                     keep.predictors=NULL,
                     direction=NULL)
{

  call <- match.call()

  if(!class(object)=='mistats')
    stop("Object must be of class 'mistats'")
  names_model <- names(object$statistics[[1]]$model)
  if(any(grepl("rcs", names_model)))
    stop("To pool restricted cubic spline variables use function glm_mi")
  if(any(grepl("ns", names_model)))
    stop("To pool spline variables use function glm_mi")

  # set regression formula fm
  fm <-
    formula(object$call[[3]][[2]])
  nimp <-
    length(object$statistics)

  imp_dat <- list()
  for (i in 1:nimp) {
    imp_dat[[i]] <-
      object$statistics[[i]]$model
  }
  names_temp <-
    clean_P(names(imp_dat[[1]]))

  imp_id <-
    rep(1:nimp, each=nrow(imp_dat[[1]]))
  imp_dat <-
    data.frame(imp_id, do.call("rbind", imp_dat))
  names(imp_dat) <-
    c("imp_id", names_temp)

  if(object$statistics[[1]]$family[[1]]=="binomial"){
    pmodel <-
      glm_mi(data=imp_dat, formula = fm,
                     p.crit = p.crit, direction=direction, nimp=nimp, impvar="imp_id",
                     keep.predictors = keep.predictors, method=method, model_type="binomial")
  }
  if(object$statistics[[1]]$family[[1]]=="gaussian"){
    pmodel <-
      glm_mi(data=imp_dat, formula = fm,
                     p.crit = p.crit, direction=direction, nimp=nimp, impvar="imp_id",
                     keep.predictors = keep.predictors, method=method, model_type="linear")
  }

  output <-
    list(pmodel=pmodel$RR_model_final[[1]],
         pmultiparm=pmodel$multiparm_final[[1]],
         pmodel_step=pmodel$RR_model,
         pmultiparm_step = pmodel$multiparm,
         formula_final = pmodel$formula_final,
         formula_initial = pmodel$formula_initial,
         predictors_in = pmodel$predictors_in,
         predictors_out = pmodel$predictors_out,
         nimp = pmodel$nimp, Outcome = pmodel$Outcome,
         method = pmodel$method, p.crit = pmodel$p.crit,
         call = pmodel$call, model_type=pmodel$model_type,
         direction = pmodel$direction,
         predictors_final = pmodel$predictors_final,
         predictors_initial = pmodel$predictors_initial,
         keep.predictors = pmodel$keep.predictors)
  class(output) <- 'mipool'
  return(output)
}
