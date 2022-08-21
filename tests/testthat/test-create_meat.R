# Tests for cpp functions

mat <- cbind(1:5, 6:10)
group <- c(1, 1, 1, 2, 3)
cluster_bool <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
expected <- aggregate(mat, by = list(cluster = group), FUN = sum)
expected <- as.matrix(expected[, 2:nrow(expected)])

actual <- sum_by_dyad_cluster(mat, cluster_bool)

test_that("sum_by_dyad_cluster works", {
  expect_equal(actual, expected, ignore_attr = TRUE)
})

# Check if meat is correctly made.
dyad <- cbind(c(1, 1, 2), c(2, 3, 3))
est_fun <- cbind(rnorm(3), rnorm(3))
id <- c(1, 2, 3)

expected <- matrix(0, nrow = 2, ncol = 2)

for (i in id) {
  cluster_i <- dyad[, 1] == i | dyad[, 2] == i
  uj <- sum_by_dyad_cluster(est_fun, cluster_i)
  expected <- expected + t(uj) %*% uj / nrow(est_fun)
}

actual <- create_meat(est_fun, dyad, id)

test_that("create_meat correctly makes meat", {
  expect_equal(actual, expected, ignore_attr = TRUE)
})
