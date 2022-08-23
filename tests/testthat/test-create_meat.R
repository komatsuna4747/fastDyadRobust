# Tests for cpp functions
set.seed(12345)

# Check if meat is correctly made.
dyad <- cbind(c(1, 1, 2), c(2, 3, 3))
est_fun <- cbind(rnorm(3), rnorm(3))
id <- c(1, 2, 3)

expected <- matrix(0, nrow = 2, ncol = 2)

for (i in id) {
  cluster_i <- dyad[, 1] == i | dyad[, 2] == i
  clusIndexUp <- cluster_i * (-99) + (1 - cluster_i) * 1:nrow(est_fun)
  uj_dt <- stats::aggregate(est_fun, by = list(cluster = clusIndexUp), FUN = sum)
  uj <- as.matrix(uj_dt[, 2:ncol(uj_dt)])
  expected <- expected + t(uj) %*% uj / nrow(est_fun)
}

actual <- create_meat(est_fun, dyad, id)

test_that("create_meat correctly makes meat", {
  expect_equal(actual, expected, ignore_attr = TRUE)
})
