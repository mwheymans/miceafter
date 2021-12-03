#' Pooling and backward or forward selection of Linear and Logistic regression
#'  models across multiply imputed data.
#'
#' \code{glm_mi} Pooling and backward or forward selection of Linear and Logistic regression
#'  models across multiply imputed data using selection methods RR, D1, D2, D3, D4 and MPR.
#'
#' @param data Data frame with stacked multiple imputed datasets.
#'   The original dataset that contains missing values must be excluded from the
#'   dataset. The imputed datasets must be distinguished by an imputation variable,
#'   specified under impvar, and starting by 1.
#' @param formula A formula object to specify the model as normally used by glm.
#'   See under "Details" and "Examples" how these can be specified. If a formula
#'   object is used set predictors, cat.predictors, spline.predictors or int.predictors
#'   at the default value of NULL.
#' @param nimp A numerical scalar. Number of imputed datasets. Default is 5.
#' @param impvar A character vector. Name of the variable that distinguishes the
#' imputed datasets.
#' @param keep.predictors A single string or a vector of strings including the variables that are forced
#'   in the model during predictor selection. All type of variables are allowed.
#' @param p.crit A numerical scalar. P-value selection criterium. A value of 1
#'   provides the pooled model without selection.
#' @param method A character vector to indicate the pooling method for p-values to pool the
#'   total model or used during predictor selection. This can be "RR", D1", "D2", "D3", "D4", or "MPR".
#'   See details for more information. Default is "RR".
#' @param direction The direction of predictor selection, "BW" means backward selection and "FW"
#'   means forward selection.
#' @param model_type A character vector for type of model, "binomial" is for logistic regression and
#'   "linear" is for linear regression models.
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
#'  A typical formula object has the form \code{Outcome ~ terms}. Categorical variables has to
#'  be defined as \code{Outcome ~ factor(variable)}, restricted cubic spline variables as
#'  \code{Outcome ~ rcs(variable, 3)}. Interaction terms can be defined as
#'  \code{Outcome ~ variable1*variable2} or \code{Outcome ~ variable1 + variable2 + variable1:variable2}.
#'  All variables in the terms part have to be separated by a "+". If a formula
#'  object is used set predictors, cat.predictors, spline.predictors or int.predictors
#'  at the default value of NULL.
#'
#'@return An object of class \code{pmods} (multiply imputed models) from
#'  which the following objects can be extracted:
#'  \itemize{
#'  \item  \code{data} imputed datasets
#'  \item  \code{RR_model} pooled model at each selection step
#'  \item  \code{RR_model_final} final selected pooled model
#'  \item  \code{multiparm} pooled p-values at each step according to pooling method
#'  \item  \code{multiparm_final} pooled p-values at final step according to pooling method
#'  \item  \code{multiparm_out} (only when direction = "FW") pooled p-values of removed predictors
#'  \item  \code{formula_step} formula object at each step
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
#' @references EW. Steyerberg (2019). Clinical Prediction MOdels. A Practical Approach
#'  to Development, Validation, and Updating (2nd edition). Springer Nature Switzerland AG.
#'
#' @references http://missingdatasolutions.rbind.io/
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'   pool_lr <- glm_mi(data=lbpmilr, formula = Chronic ~ Pain +
#'   factor(Satisfaction) + rcs(Tampascale,3) + Radiation +
#'   Radiation*factor(Satisfaction) + Age + Duration + BMI,
#'   p.crit = 0.05, direction="FW", nimp=5, impvar="Impnr",
#'   keep.predictors = c("Radiation*factor(Satisfaction)", "Age"),
#'   method="D1", model_type="binomial")
#'
#'   pool_lr$RR_model_final
#'
#' @export
glm_mi <- function(data,
                     formula = NULL,
                     nimp=5,
                     impvar=NULL,
                     keep.predictors=NULL,
                     p.crit=1,
                     method="RR",
                     direction=NULL,
                     model_type=NULL)
{

  call <- match.call()

  m_check <- check_model(data=data, formula = formula,
                         keep.predictors = keep.predictors,
                         impvar=impvar, p.crit=p.crit, method=method,
                         nimp=nimp, direction=direction,
                         model_type=model_type)

  P <- m_check$P
  keep.P <- m_check$keep.P
  Outcome <- m_check$Outcome

  if(p.crit==1){
    if(model_type=="binomial")
      pobjpool <-
        glm_lr_bw(data = data, nimp=nimp, impvar = impvar, Outcome= Outcome,
                    P = P, p.crit = p.crit, method = method, keep.P = keep.P)
    if(model_type=="linear")
      pobjpool <-
        glm_lm_bw(data = data, nimp=nimp, impvar = impvar, Outcome= Outcome,
                    P = P, p.crit = p.crit, method = method, keep.P = keep.P)
    class(pobjpool) <-
      "pmods"
    return(pobjpool)
  }
  if(direction=="FW"){
    if(model_type=="binomial")
      pobjfw <-
        glm_lr_fw(data = data, nimp = nimp, impvar = impvar, Outcome = Outcome, p.crit = p.crit,
                    P = P, keep.P = keep.P, method = method)
    if(model_type=="linear")
      pobjfw <-
        glm_lm_fw(data = data, nimp = nimp, impvar = impvar, Outcome = Outcome, p.crit = p.crit,
                    P = P, keep.P = keep.P, method = method)
    class(pobjfw) <-
      "pmods"
    return(pobjfw)
  }
  if(direction=="BW"){
    if(model_type=="binomial")
      pobjbw <-
        glm_lr_bw(data = data, nimp = nimp, impvar = impvar, Outcome = Outcome, p.crit = p.crit,
                    P = P, keep.P = keep.P, method = method)
    if(model_type=="linear")
      pobjbw <-
        glm_lm_bw(data = data, nimp = nimp, impvar = impvar, Outcome = Outcome, p.crit = p.crit,
                    P = P, keep.P = keep.P, method = method)
    class(pobjbw) <-
      "pmods"
    return(pobjbw)
  }
}
