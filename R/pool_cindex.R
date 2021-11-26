#' Calculates the pooled C-index
#'
#' \code{pool_cindex} Calculates the pooled C-index and Confidence interval.
#'
#' @param data An object of class 'raami' (repeated analysis after
#'  multiple imputation) or a 2 x m matrix with C-index
#'  values and standard errors. For the latter situation dfcom has to be
#'  provided.
#' @param conf.level conf.level Confidence level of the confidence intervals.
#' @param dfcom Number of completed-data analysis degrees of freedom.
#'
#' @details Rubin's Rules are used for pooling. The C-index values are log
#'  transformed before pooling and finally back transformed.
#'
#' @return The pooled c-index value and the confidence intervals.
#'
#' @author Martijn Heymans, 2021
#'
#' @examples
#'  imp_list <- lbpmilr %>%
#'   group_split(Impnr, .keep = FALSE)
#'
#'  imp_data <- make_mids(imp_list)
#'
#'  # Logistic regression
#'  data <- with.miceafter(data=imp_data,
#'   expr = cindex(glm(Chronic ~ Gender + Radiation, family="binomial")))
#'  res <- pool_cindex(data)
#'  res
#'
#'  # Cox regression
#'  imp_list <- lbpmicox %>%
#'   group_split(Impnr, .keep = FALSE)
#'
#'  imp_data <- make_mids(imp_list)
#'
#'  data <- with.miceafter(data=imp_data,
#'   expr = cindex(coxph(Surv(Time, Status) ~ Pain + Radiation)))
#'  res <- pool_cindex(data)
#'  res
#'
#' @export
pool_cindex <- function(data,
                        conf.level = 0.95,
                        dfcom=NULL)
{

  if(class(data)=='raami'){
    ra <- data.frame(do.call("rbind", data$statistics))
    colnames(ra) <- c("est", "se", "dfcom")
  } else {
    if (!all(!is.data.frame(data) | !is.matrix(data)))
      stop("Data should be a data frame or matrix")
    ra <- data
    if(is_empty(dfcom))
      stop("Include number of complete data degrees of freedom")
  }

  est_log <- pool_scalar_RR(est=ra[, 1], se=ra[, 2], logit_trans=TRUE,
                            conf.level=0.95, dfcom=ra[,3][1])
  est_orig <- inv_logit(est=est_log$pool_est,
                         se=est_log$pool_se,
                         crit.value = est_log$t)

  output <- matrix(est_orig, 1, 4)

  colnames(output) <- c("C-index", "Critical value",
                        c(paste(conf.level*100, "CI low"),
                          paste(conf.level*100, "CI high")))
  return(output)
}
