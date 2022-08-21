#' Dyad-robust standard errors
#' @inheritParams fixest::feols
#' @param cluster hoge
#' Tells how to cluster the standard-errors.
#' Assume we want to perform dyadic clustering over var1 and var2 contained in the data.frame.
#' Then \code{cluster = c("var1", "var2")}
#' @param ... Other parameters to be passed to `fixest::feols()`.
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
  vcov_dyad_robust <- fastDyadRobust(reg, dyad_cluster)

  # Replace the old standard errors with new ones.
  reg <- summary(reg, vcov = vcov_dyad_robust)
  attr(reg$se, "type") <- "Dyadic-robust"

  return(reg)
}
