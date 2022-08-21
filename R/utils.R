clean_dyad_cluster <- function(dyad_cluster) {
  dyad_cluster <- as.matrix(dyad_cluster)
  id <- sort(unique(c(dyad_cluster[, 1], dyad_cluster[, 2])))

  if (!is.integer(id)) {
    id <- as.factor(id)
    dyad_cluster <- cbind(
      factor(dyad_cluster[, 1], levels = id),
      factor(dyad_cluster[, 2], levels = id)
    )
    id <- as.integer(id)
  }

  # Conditionally swapping so that col1 <= col2.
  i <- which(dyad_cluster[, 1] > dyad_cluster[, 2])

  v1 <- dyad_cluster[i, 1]
  v2 <- dyad_cluster[i, 2]

  dyad_cluster[i, 1] <- v2
  dyad_cluster[i, 2] <- v1

  return(list("id" = id, "dyad_cluster" = dyad_cluster))
}
