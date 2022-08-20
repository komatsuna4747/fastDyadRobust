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
#'
#' @export
fastDyadRobust <- function(fit, dyad_cluster) {

  # Check dyad_cluster
  if (ncol(dyad_cluster) != 2) {
    stop("dyad_cluster must have just two columns.")
  }

  # Extract ids for dyad-robust clustering
  list_data <- clean_dyad_cluster(dyad_cluster)

  # Create dyad id
  dyad_id <-
    paste(list_data$dyad_cluster[, 1], list_data$dyad_cluster[, 2], sep = "-")

  # Prepare
  est_fun <- sandwich::estfun(fit)

  # Prepare meat and bread
  meat <- create_meat(est_fun, list_data$dyad_cluster, list_data$id)
  bread <- sandwich::bread(fit)

  #
  dyad_robust_vcov <-
    1/nrow(est_fun) * (bread %*% meat %*% bread) -
    sandwich::vcovCL(fit, type = "HC0", cluster = dyad_id, cadjust = FALSE) -
    (length(list_data$id) - 2) * sandwich::vcovHC(fit, "HC0")

  return(dyad_robust_vcov)
}
