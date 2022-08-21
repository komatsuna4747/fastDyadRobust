dyad_raw <- cbind(c(1, 1, 2), c(2, 3, 3))
dyad_processed <- clean_dyad_cluster(dyad_raw)
dyad_expected <- dyad_raw
id_expected <- c(1, 2, 3)

test_that("Cleaning dyad_cluster works.", {
  expect_equal(dyad_processed$dyad_cluster, dyad_expected)
  expect_equal(dyad_processed$id, id_expected)
})


dyad_raw <- cbind(c(2, 3, 3), c(1, 1, 2))
id <- c(1, 2, 3)
dyad_processed <- clean_dyad_cluster(dyad_raw)
dyad_expected <- cbind(c(1, 1, 2), c(2, 3, 3))
id_expected <- c(1, 2, 3)

test_that("Conditional swapping works", {
  expect_equal(dyad_processed$dyad_cluster, dyad_expected)
  expect_equal(dyad_processed$id, id_expected)
})


dyad_raw <- cbind(c("a", "a", "b"), c("b", "c", "c"))
dyad_processed <- clean_dyad_cluster(dyad_raw)
dyad_expected <- cbind(c(1, 1, 2), c(2, 3, 3))
id_expected <- c(1, 2, 3)

test_that("Handling character dyad_cluster works", {
  expect_equal(dyad_processed$dyad_cluster, dyad_expected)
  expect_equal(dyad_processed$id, id_expected)
})
