#' @useDynLib fixestDyadRobust
#' @importFrom Rcpp sourceCpp
#'
#' @param fit The model object estimated by the \code{fixest} package.
#' @param dyad_cluster A data frame or matrix that specifies how to dyadically cluster the standard errors.
#' The dimension of `dyad_cluster` must be D x 2,
#' where D is the number of rows of the data frame that you have used to estimate `fit`.
fixestDyadRobust <- function(fit, dyad_cluster) {

  # Check fit
  if (class(fit) != "fixest") {
    stop("fixestDyadRobust() only supports fixest objects.")
  }

  if (fit$method != "feols") {
    stop("fixestDyadRobust() only supports feols models.")
  }

  # Check dyad_cluster
  if (ncol(dyad_cluster) != 2) {
    stop("dyad_cluster must have just two columns.")
  }

  if (nrow(dyad_cluster) != fit$nobs) {
    stop("The number of observations of the fitted model doesn't match that of dyad_cluster.")
  }

}
