#' Function to check input data for function \code{psfmi_lr}
#'
#' \code{check_model} Back transformation to original scale of
#'  natural logarithmic transformed parameters and calculating
#'  confidence interval
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
#' @author Martijn Heymans, 2020
#' @keywords internal
#'
#' @export
check_model <- function(data,
                        formula,
                        keep.predictors,
                        impvar,
                        p.crit,
                        method,
                        nimp,
                        direction,
                        model_type)
{

  form <-
    terms(formula)
  form_vars <-
    attr(form, "term.labels")
  if(is_empty(form_vars))
    stop("\n", "No predictors defined, model is empty")
  Outcome <-
    as.character(attr(form, "variables")[[2]])
  int.P <-
    form_vars[grepl(paste(c("[*]", ":"), collapse = "|"), form_vars)]
  int.P_temp <-
    unique(unlist(str_split(int.P, paste(c("[*]", ":"), collapse = "|"))))
  form_vars <-
    form_vars[!grepl(paste(c("[*]", ":"), collapse = "|"), form_vars)]
  form_vars <-
    unique(c(form_vars, int.P_temp))
  cat.P <-
    form_vars[grepl("factor", form_vars)]
  form_vars <-
    form_vars[!grepl("factor", form_vars)]
  s.P <-
    form_vars[grepl("rcs", form_vars)]
  nknots <-
    c(readr::parse_number(s.P))
  form_vars <-
    form_vars[!grepl("rcs", form_vars)]
  int.P <-
    gsub(":", "*", clean_P(int.P))
  cat.P <- clean_P(cat.P)
  s.P <- clean_P(s.P)
  P <- form_vars

  keep.P <-
    gsub(":", "*", keep.predictors)

  keep.P <-
    sapply(as.list(keep.P), clean_P)

  P.check <-
    c(P, cat.P, s.P)
  # Check data input
  if(is_empty(model_type))
    stop("model_type not defined, use 'binomial' or 'linear'")
  if(p.crit!=1){
    if(is_empty(direction))
      stop("Specify FW or BW for forward or backward predictor selection")
  }
  if(is.mids(data)){
    data <- data.frame(complete(data, action="long", include=FALSE))
    names(data)[names(data)==".imp"] <- impvar
  }
  if (!is.data.frame(data))
    stop("Data should be a data frame")
  data <- data.frame(as_tibble(data))
  data <- mutate_if(data, is.factor, ~ as.numeric(as.character(.x)))
  if(model_type=="binomial") {
  if(!all(data[Outcome]==1 | data[Outcome]==0))
    stop("Outcome should be a 0 - 1 variable")
  }
  if(model_type=="linear" & method=="D3")
    stop("Method D3 is not available for linear regression models")
  if ((nvar <- ncol(data)) < 2)
    stop("Data should contain at least two columns")
  if(is_empty(impvar))
    stop("Imputation variable is not defined")
  if(is_empty(method)) method="RR"
  if(all(!is_empty(cat.P) | !is_empty(s.P)) & method=="RR")
    stop("Categorical or spline variables in model,
         define selection method: D1, D2, D3, D4 or MPR")
  if (sort(unique(data[, impvar]))[1] == 0)
    stop("Original dataset should not be included")
  if(is_empty(nimp))
    stop("Number of imputed datasets is not defined, use nimp!")
  if (nimp < 2) {
    stop("\n", "Number of imputed datasets must be > 1", "\n\n")
  }
  if (p.crit > 1)
    stop("\n", "P-value criterium > 1", "\n")
  if (any(nknots<3))
    stop("\n", "Number of knots must be > 2", "\n")
  if (length(nknots) != length(s.P))
    stop("\n", "Number of knots not specified for every spline variable", "\n")
  if (!is_empty(cat.P)) {
    if(any(cat.P%in%P)){
      cat.P.double <- cat.P[cat.P%in%P]
      stop("\n", "Categorical variable(s) -", cat.P.double,
           "- also defined as Predictor", "\n\n")
    }
  }
  if (!is_empty(s.P)){
    if(any(s.P%in%P)){
      s.P.double <- s.P[s.P%in%P]
      stop("\n", "Do not include Spline variable(s) -", s.P.double,
           "- in predictors", "\n\n")
    }
  }
  if(any(duplicated(P))){
    stop("\n", "Predictor(s) - ", c(P[duplicated(P)]),
         " - defined more than once", "\n\n")
  }
  # Check if al variables are available in dataset
  if(any(!P.check %in% names(data))) {
    P.mis <- P.check[!P.check %in% names(data)]
    stop("\n", "Predictor(s) - ", P.mis,
         "- not available in dataset", "\n\n")
  }
  if(!is_empty(int.P)) {
    int.P.check <- lapply(int.P[grep("[*]", int.P)],
                          function(x) { unlist(strsplit(x, split="[*]")) })
    int.P.check <- unique(unlist(int.P.check))
    if(any(!int.P.check %in% P.check))
      stop("\n", "Not all interaction terms defined as
        Predictor or Categorical Predictor", "\n\n")
  }
  # First predictors, second categorical
  # predictors and last interactions
  P <-
    c(P, cat.P, s.P, int.P)
  if (is_empty(P))
    stop("\n", "No predictors to select, model is empty", "\n\n")

  if (!is_empty(keep.P)) {
    for(i in 1:length(keep.P)){
      if(grepl("[*]", keep.P[i])) {
        keep.P.spl <- unlist(strsplit(keep.P[i], split="[*]"))
        if(length(P[Reduce("&", lapply(keep.P.spl, grepl, P))])==0)
          stop("Interaction term in keep.predictors not defined
            as int.predictors, incorrect")
        keep.P[i] <- P[Reduce("&", lapply(keep.P.spl, grepl, P))]
      }
    }
  }

  if (!is_empty(cat.P)) {
    if(length(cat.P)==1){
      P <-
        gsub(cat.P,
             replacement=paste0("factor(", cat.P, ")"), P)
      if(!is_empty(keep.P)){
        keep.P <-
          gsub(cat.P,
               replacement=paste0("factor(", cat.P, ")"), keep.P)
      }
    } else {
      for(i in 1:length(cat.P)) {
        P <-
          gsub(cat.P[i],
               replacement=paste0("factor(", cat.P[i], ")"), P)
        if(!is_empty(keep.P)){
          keep.P <-
            gsub(cat.P[i],
                 replacement=paste0("factor(", cat.P[i], ")"), keep.P)
        }
      }
    }
  }
  if (!is_empty(s.P)) {
    if(length(s.P)==1){
      P <-
        gsub(s.P,
             replacement=paste0("rcs(", s.P, ",", nknots, ")"), P)
      if(!is_empty(keep.P)){
        keep.P <-
          gsub(s.P,
               replacement=paste0("rcs(", s.P, ",", nknots, ")"), keep.P)
      }
    } else {
      for(i in 1:length(s.P)) {
        P <- gsub(s.P[i],
                  replacement=paste0("rcs(", s.P[i], ",", nknots[i], ")"), P)
        if(!is_empty(keep.P)){
          keep.P <-
            gsub(s.P[i],
                 replacement=paste0("rcs(", s.P[i], ",", nknots[i], ")"), keep.P)
        }
      }
    }
  }
  levels.cat.P <- lapply(cat.P, function(x) {
    nr.levels.cat.P <- length(table(data[data[impvar] == 1, ][, x]))
    if (nr.levels.cat.P < 3) {
      stop("\n", "Categorical variable(s) only 2 levels,
        do not define as categorical", "\n\n")
    }
  })

  if(any(!keep.P %in% P))
    stop("\n", "Variables to keep not defined as Predictor", "\n\n")

  obj <- list(Outcome=Outcome, P=P, keep.P=keep.P)
  return(obj)
}
