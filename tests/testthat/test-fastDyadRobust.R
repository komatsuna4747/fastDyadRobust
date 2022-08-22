set.seed(334)

for (is_repeated in c(FALSE, TRUE)) {
  df_test <- make_test_data(repeated = is_repeated)

  # Compute dyad-robust standard errors using fastDyadRobust
  dyad_cluster <- df_test[c("Var1", "Var2")]
  reg_dyad <- lm(y ~ x, df_test)
  vcov_fastDyadRobust <- fastDyadRobust(reg_dyad, dyad_cluster)

  # Compute dyad-robust standard errors using dyadRobust
  # testDyadRobust() is in R/testthat-helpers.R
  dyad_cluster_test <- dyad_cluster
  names(dyad_cluster_test) <- c("egoid", "alterid")
  dyad_cluster_test$dyadid <- paste(dyad_cluster_test$egoid, dyad_cluster_test$alterid, sep = "-")
  vcov_dyadRobust <- testDyadRobust(reg_dyad, dyad_cluster_test)

  test_that("Correctly computing dyad-robust standard errors works", {
    expect_equal(vcov_fastDyadRobust, vcov_dyadRobust, tolerance = 1e-12)
  })

  # Check errors
  test_that("Error messages", {
    expect_error(fastDyadRobust(reg_dyad, dyad_cluster[, 1, drop = FALSE]))
    expect_error(fastDyadRobust(reg_dyad, dyad_cluster[1:10, ]))
  })
}
