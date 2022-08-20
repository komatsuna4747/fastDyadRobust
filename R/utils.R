clean_dyad_cluster <- function(dyad_cluster) {
  dyad_cluster <- as.matrix(dyad_cluster)
  id <- unique(c(dyad_cluster[, 1], dyad_cluster[, 2]))

  if (!is.integer(id)) {
    id <- as.factor(id)
    dyad_cluster <- cbind(
      factor(dyad_cluster[, 1], levels = id),
      factor(dyad_cluster[, 2], levels = id)
    )
    id <- as.integer(id)
  }

  dyad_cluster[which(dyad_cluster[, 1] > dyad_cluster[, 2])] <-
    rev(dyad_cluster[which(dyad_cluster[, 1] < dyad_cluster[, 2])])

  return(list("id" = id, "dyad_cluster" = dyad_cluster))
}
