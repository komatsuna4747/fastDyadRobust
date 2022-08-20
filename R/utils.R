clean_data_cluster <- function(data_cluster) {
  id <- unique(c(data_cluster[, 1], data_cluster[, 2]))

  if (!is.integer(id)) {
    id <- as.factor(id)
    data_cluster <- cbind(
      factor(data_cluster[, 1], level = id),
      factor(data_cluster[, 2], level = id)
    )
    id <- as.integer(id)
  }

  data_cluster[which(data_cluster[, 1] > data_cluster[, 2])] <-
    rev(data_cluster[which(data_cluster[, 1] < data_cluster[, 2])])

  return(list("id" = id, "data_cluster" = data_cluster))
}
