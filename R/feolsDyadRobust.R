#' Dyadic-robust standard errors for `fixest::feols()`
#'
#' @description
#' A wrapper function for `fixest::feols()` to compute dyadic-robust standard errors.
#'
#' @inheritParams fixest::feols
#' @param cluster hoge
#' Tells how to dyadically cluster the standard-errors.
#' Assume we want to perform dyadic clustering over var1 and var2 contained in the data.frame.
#' Then you should provide `feolsDyadRobust()` with `cluster = c("var1", "var2")`.
#' @param ... Other parameters to be passed to `fixest::feols()`. See `?fixest::feols` for details.
#'
#' @return Returns a `fixest` object whose standard errors are computed by `vcovDyadRobust()`.
#'
#' @examples
#' \dontrun{
#' # Load a toy dataset
#' df <- fastDyadRobust::toyData
#'
#' # Run regression using fastDyadRobust::feolsDyadRobust()
#' # You can pass other arguments to fixest::feols() such as `weidhts`.
#' reg <-
#'   fastDyadRobust::feolsDyadRobust(
#'     fml = dy ~ dx1 + dx2,
#'     data = df,
#'     cluster = c("src", "dst")
#'   )
#'
#' # Check the result
#' summary(reg)
#' }
#'
#' @export
feolsDyadRobust <- function(fml, data, cluster, ...) {
  if (is.null(cluster)) {
    stop("The argument `cluster` must be specified.")
  }

  if (!is.character(cluster)) {
    stop("The argument `cluster` must be a character vector, like c('id1', 'id2')")
  }

  if (length(cluster) != 2) {
    stop("The argument `cluster` must be of length 2.")
  }

  # Fit the model
  reg <- fixest::feols(fml = fml, data = data, cluster = cluster, ...)

  # Extract ids to dyadically cluster the standard errors
  dyad_cluster <- data[cluster]

  # If some observations are removed:
  if (!is.null(reg$obs_selection$obsRemoved)) {
    dyad_cluster <- dyad_cluster[reg$obs_selection$obsRemoved, , drop = FALSE]
  }

  # Compute dyad-robust standard errors
  vcov_dyad_robust <- vcovDyadRobust(reg, dyad_cluster)

  # Replace the old standard errors with new ones.
  reg <- summary(reg, vcov = vcov_dyad_robust)
  attr(reg$se, "type") <- "Dyadic-robust"

  return(reg)
}
