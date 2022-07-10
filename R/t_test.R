#' Calculates the one, two and paired sample t-test
#'
#' \code{t_test} Calculates the one, two and paired sample t-test.
#'
#' @param y numeric response variable.
#' @param x categorical variable with 2 groups.
#' @param formula A formula object to specify the model as normally
#'  used by glm.
#' @param data An objects of class \code{milist}, created by
#'  \code{df2milist}, \code{list2milist} or \code{mids2milist}.
#' @param paired a logical indicating whether you want
#'  a paired t-test (TRUE) or not (FALSE, default).
#' @param var_equal a logical, if TRUE equal variances are assumed, if FALSE
#'  (default) equal variances are not assumed and Welch correction
#'  is applied for the number of degrees of freedom. See detail.
#'
#' @details For all t-tests the dataset must be in long format
#'  (i.e. group data under each other). For the paired t-test x and y
#'  must have the same length. When variances between groups are
#'  unequal, the Welch df correction formula is used and eventually
#'  averaged across multiply imputed datasets in the \code{pool_t_test}
#'  function.
#'
#'@return An object containing the following objects are extracted:
#'  \itemize{
#'  \item  \code{mdiff} the mean difference.
#'  \item  \code{se} the standard error.
#'  \item  \code{dfcom} the complete data degrees of freedom.
#'}
#'
#' @author Martijn Heymans, 2022
#'
#' @seealso \code{\link{with.milist}}, \code{\link{pool_t_test}}
#'
#' @examples
#'
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=t_test(Pain ~ Gender))
#'
#' @export
t_test <- function(y,
                   x,
                   formula,
                   data,
                   paired=FALSE,
                   var_equal=TRUE) {

  call <- match.call()

  if(!inherits(y,"formula")){
    eval_prop <-
      eval(call[[1L]], parent.frame())
    df <-
      data.frame(y, x)
  } else {
    eval_prop <-
      eval(call[[2]], parent.frame())
    fit <-
      lm(eval_prop, y=TRUE, x=TRUE)
    nr_var <-
      attr(fit$terms, "term.labels")
    if(length(nr_var) > 1)
      stop("Include only one categorical variable")
    df <-
      fit$model
    names(df) <-
      c("y", "x")
  }
  gr <- sort(unique(df$x))
  if(length(gr)>2)
    stop("only two groups are allowed")
  dat1 <- subset(df, x==gr[1])
  dat2 <- subset(df, x==gr[2])
  y1 <- dat1$y
  y2 <- dat2$y
  n1 <- length(y1)
  n2 <- length(y2)
  # paired t-test
  if(paired==TRUE){
    if(!n1==n2)
      stop("groups must be of the same size")
    diff <- y1-y2
    mdiff <- mean(diff)
    sd_diff <- sd(diff)
    se <- sd_diff/sqrt(n1)
    dfcom <- n1-1
    # 2 sample t-test
  } else {
    mdiff <- mean(y1)-mean(y2)
    s1 <- sd(y1)
    s2 <- sd(y2)
    if(var_equal){
      sp <- sqrt((((n1-1)*s1^2) + ((n2-1)*s2^2))/(n1+n2-2))
      se <- sp * sqrt(1/n1 + 1/n2)
      dfcom <- n1+n2-2
      # Welch df correction
    } else {
      se <- sqrt((s1^2/n1) + (s2^2/n2))
      df_1 <- ((s1^2/n1) + (s2^2/n2))^2
      df_2 <- ((s1^2/n1)^2/(n1-1)) + ((s2^2/n2)^2/(n2-1))
      dfcom <- df_1/df_2
    }
  }
  output <- c(mdiff, se, dfcom)
  return(output)
}
