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
#' @param nthreads The number of threads. The default is to use all threads minus one.
#' @export
vcovDyadRobust <- function(fit, dyad_cluster, nthreads = RcppParallel::defaultNumThreads() - 1) {

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
  RcppParallel::setThreadOptions(nthreads)
  meat <- create_meat(est_fun, list_data$dyad_cluster, list_data$id)
  bread <- sandwich::bread(fit)

  # Compute dyad-robust standard errors via multiway decomposition (Aronow, Samii, and Assenova, 2015).
  # V_r = \sum_{i=1}^{N} V_{C, i} - V_D - (N - 2) * V_0

  # V_{C, i}: the usual asymptotically consistent cluster-robust variance estimator
  # (with no degressof-freedom adjustment) that clusters all dyads containing unit i
  # and assumes all other observations to be independent

  # V_D: the same cluster robust estimator but clustering all repeated dyad observations

  # V_0: the usual asymptotically consistent heteroskedasticity robust (HC) variance estimator
  # that assumes all observations (even within repeated dyad groupings) are independent.

  # N: the number of nodes

  # Step 1: Compute \sum_{i=1}^{N} V_{C, i}
  dyad_robust_vcov <- 1 / nrow(est_fun) * (bread %*% meat %*% bread)

  # Step 2: Compute V_D and subtract it from dyad_robust_vcov
  dyad_robust_vcov <- dyad_robust_vcov -
    sandwich::vcovCL(fit, cluster = dyad_id, type = "HC0", cadjust = FALSE)

  # Step 3: Compute (N - 2) * V_0 and subtract it from dyad_robust_vcov
  dyad_robust_vcov <- dyad_robust_vcov -
    (length(list_data$id) - 2) * sandwich::vcovCL(fit, cluster = 1:nrow(est_fun), type = "HC0", cadjust = FALSE)

  return(dyad_robust_vcov)
}
