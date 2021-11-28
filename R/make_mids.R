#' Converts a data.frame, tibble or list into a object of class
#'  \code{mice::mids}
#'
#' \code{make_mids} Converts data.frame, tibble or list into object of
#'  class \code{mice::mids}.
#'
#' @param data A Data frame, tibble or list containing the multiply imputed
#'  datasets. The original dataset that contains missing values must be
#'  excluded from the dataset. For data frames and tibbles, The imputed
#'  datasets must be distinguished by an imputation variable,
#'  specified under impvar, and starting by 1.
#' @param impvar A character vector. Name of the variable that distinguishes the
#'  imputed datasets.
#'
#' @return Object of class mids.
#'
#' @author Martijn Heymans, 2021
#'
#' @export
make_mids <- function(data,
                      impvar=NULL){

  if(any(class(data)=="data.frame")){
    if(is.null(impvar))
      stop("determine variable that distinguishes imputed datasets using 'impvar'")
    data <- data %>%
      group_split(get(impvar), .keep = TRUE)
  }
  if(!any(class(data)=="list"))
    stop("data is not of object data.frame, tibble or list")

  dat_imp1 <- data[[1]]

  n <- nrow(dat_imp1)
  nimp <- length(data)

  r_X <- matrix(NA, n, ncol(dat_imp1))

  for(k in seq_len(ncol(dat_imp1))){

    x <- data[[1]]
    imp_var <- unlist(sapply(data, function(x) x[, k]))
    imp_X <- matrix(imp_var, n, nimp)

    # make missing
    r_imp <- matrix(NA, n, nimp-1)
    for(i in 1:ncol(imp_X)-1){
      imp_comp <- imp_X[, i]
      for(j in 2:nimp) {
        r_imp[,i] <- imp_comp == imp_X[, j]
      }
    }
    r_X[, k] <- ifelse(apply(r_imp, 1, sum)==nimp-1, 0, 1)
  }
  dat_imp1[r_X==1] <- NA

  # impute
  imp_ini <- mice(dat_imp1, m=nimp, maxit=0, allow.na = TRUE)
  ini_imputed <- imp_ini$imp

  # replace missing data with imputed data
  for(j in 1:nimp){
    for(i in 1:ncol(dat_imp1)){
      repl_var <- unlist(data[[j]][, i])
      ini_imputed[[i]][, j]  <- repl_var[r_X[, i]==1]
    }
  }
  imp_ini$imp <- ini_imputed
  imp_ini$method[impvar] <- ""
  names_mis <- names(imp_ini$nmis[imp_ini$nmis!=0])
  names_mis <- names_mis[!names_mis%in%impvar]
  imp_ini$method[names_mis] <- "miceafter"
  return(imp_ini)
}
