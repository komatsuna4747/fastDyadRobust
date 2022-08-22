#' Compute dyadic-robust standard errors
#'
#' @description
#' Computes dyad-robust standard errors via multiway decomposition,
#' proposed by Aronow, Peter M., Cyrus Samii, and Valentina A. Assenova.
#' "Cluster-robust variance estimation for dyadic data."
#' Political Analysis 23.4 (2015): 564-577.
#'
#' This package is based on \code{dyadRobust}.
#'
#' @param fit The model object estimated by the \code{fixest} package.
#' @param dyad_cluster A data frame or matrix that specifies how to dyadically
#' cluster the standard errors.
#' The dimension of `dyad_cluster` must be D x 2,
#' where D is the number of rows of the data frame that you have used
#' to estimate `fit`.
#'
#' @export
fastDyadRobust <- function(fit, dyad_cluster, n_core = RcppParallel::defaultNumThreads() - 1) {

  # Check dyad_cluster
  if (ncol(dyad_cluster) != 2) {
    stop("dyad_cluster must have just two columns.")
  }

  est_fun <- sandwich::estfun(fit)

  if (nrow(dyad_cluster) != nrow(est_fun)) {
    stop("The number of rows of dyad_cluster doesn't match the number of observations of fit.")
  }

  # Extract and format ids for dyad-robust clustering
  list_data <- clean_dyad_cluster(dyad_cluster)

  # Create dyad id
  dyad_id <-
    paste(list_data$dyad_cluster[, 1], list_data$dyad_cluster[, 2], sep = "-")

  # Prepare meat and bread
  RcppParallel::setThreadOptions(n_core)
  meat <- create_meat(est_fun, list_data$dyad_cluster, list_data$id)
  bread <- sandwich::bread(fit)

  # Compute dyad-robust standard errors via multiway decomposition
  dyad_robust_vcov <-
    1 / nrow(est_fun) * (bread %*% meat %*% bread) -
    sandwich::vcovCL(fit, type = "HC0", cluster = dyad_id, cadjust = FALSE) -
    (length(list_data$id) - 2) * sandwich::vcovHC(fit, "HC0")

  return(dyad_robust_vcov)
}
