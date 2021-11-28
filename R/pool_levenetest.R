#' Calculates the pooled Levene test.
#'
#' \code{pool_levenetest} Calculates the pooled F-statistic
#'  of the Levenene test.
#'
#' @param object An object of class 'raami' (repeated analysis after
#'  multiple imputation) after using \code{with.miceafter}.
#' @param method A character vector to choose the pooling method,
#'  'D1' (default) or 'D2'.
#'
#' @return The (combined) F-statistic, p-value and degrees of freedom.
#'
#' @references Eekhout I, van de Wiel MA, Heymans MW. Methods for significance testing of categorical
#'   covariates in logistic regression models after multiple imputation: power and applicability
#'   analysis. BMC Med Res Methodol. 2017;17(1):129.
#' @references Enders CK (2010). Applied missing data analysis. New York: The Guilford Press.
#' @references Van Buuren S. (2018). Flexible Imputation of Missing Data. 2nd Edition. Chapman & Hall/CRC
#'   Interdisciplinary Statistics. Boca Raton.
#'
#' @author Martijn Heymans, 2021
#'
#' @seealso \code{\link{with.milist}}, \code{\link{levene_test}}
#'
#' @examples
#' imp_dat <- df2milist(lbpmilr, impvar="Impnr")
#' ra <- with(imp_dat, expr=levene_test(Pain ~ factor(Carrying)))
#' res <- pool_levenetest(ra, method="D1")
#' res
#'
#' @export
pool_levenetest <- function(object, method="D1"){

  if(all(class(object)!="mistats"))
    stop("object must be of class 'mistats'")
  if(!is.list(object$statistics))
    stop("object must be a list")

  m <- length(object$statistics)

  if(method=="D1"){
    ra_qhat <- do.call("rbind",
                       lapply(object$statistics,
                              function(x) x[[2]][-1]
                       ))

    vcov_u <- lapply(object$statistics,
                     function(x) x[[3]][-1, -1])

    u <- array(as.numeric(unlist(vcov_u)),
               dim=c(dim(vcov_u[[1]]), length(vcov_u)))

    # Average coefficient estimate
    qbar <- apply(ra_qhat, 2, mean)
    k <- length(qbar)

    # Within imputation covariance
    ubar <- apply(u, c(1, 2), mean)

    # Between imputation covariance
    e <-
      ra_qhat - matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
    b <-
      (t(e) %*% e)/(m - 1)
    # r Calculation according to package mitml
    r <-
      (1+m^(-1))*sum(diag(b%*%solve(ubar)))/k

    T.stable <- (1 + r)*ubar

    est <- t(qbar) %*% solve(T.stable) %*% qbar / k

    dfcom <- object$statistics[[1]]$dfcom

    t <- k*(m-1)
    if(!is.null(dfcom)){

      # small-sample df (Reiter, 2007: Eq. 1-2)
      a <- r*t/(t-2)
      vstar <- ( (dfcom+1) / (dfcom+3) ) * dfcom

      c0 <- 1 / (t-4)
      c1 <- vstar - 2 * (1+a)
      c2 <- vstar - 4 * (1+a)

      z <- 1 / c2 +
        c0 * (a^2 * c1 / ((1+a)^2 * c2)) +
        c0 * (8*a^2 * c1 / ((1+a) * c2^2) + 4*a^2 / ((1+a) * c2)) +
        c0 * (4*a^2 / (c2 * c1) + 16*a^2 * c1 / c2^3) +
        c0 * (8*a^2 / c2^2)

      v <- 4 + 1/z

    }else{

      if (t > 4){
        v <- 4 + (t-4) * (1 + (1 - 2*t^(-1)) * (r^(-1)))^2
      }else{
        v <- t * (1 + k^(-1)) * ((1 + r^(-1))^2) / 2
      }

    }
    output <- list(est = est, k = k, v = v, r = r)
    r <- output$r
    pval <- pf(output$est, k, output$v, lower.tail = FALSE)
    output <- matrix(c(est, k, v, pval, r), ncol = 5)
    colnames(output) <- c("F_value", "df1", "df2",
                          "P(>F)", "RIV")
  }
  if(method=="D2"){
    est <-
      sapply(object$statistics,
             function(x) x[[1]][1])
    output <-
      pool_D2(dw=est,
              v=object$statistics[[1]]$fstats[2])
  }
  class(output) <- "mipool"
  return(output)
}
