#' fastDyadRobust
#'
#' @description
#' Computes dyad-robust standard errors via multiway decomposition,
#' proposed by Aronow, Peter M., Cyrus Samii, and Valentina A. Assenova.
#' "Cluster-robust variance estimation for dyadic data." Political Analysis 23.4 (2015): 564-577.
#'
#' This package is based on \code{dyadRobust}.
#'
#' @useDynLib fastDyadRobust
#' @importFrom Rcpp sourceCpp
#'
#' @param fit The model object estimated by the \code{fixest} package.
#' @param dyad_cluster A data frame or matrix that specifies how to dyadically cluster the standard errors.
#' The dimension of `dyad_cluster` must be D x 2,
#' where D is the number of rows of the data frame that you have used to estimate `fit`.
fastDyadRobust <- function(fit, dyad_cluster) {

  # Check dyad_cluster
  if (ncol(dyad_cluster) != 2) {
    stop("dyad_cluster must have just two columns.")
  }

  if (nrow(dyad_cluster) != fit$nobs) {
    stop("The number of observations of the fitted model doesn't match that of dyad_cluster.")
  }

}
