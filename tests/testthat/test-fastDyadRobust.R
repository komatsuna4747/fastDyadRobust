set.seed(334)

# Create a test dataset
df_test <- expand.grid(1:20, 1:20)
df_test <- df_test[df_test$Var1 < df_test$Var2, ]
df_test$x <- rnorm(nrow(df_test))
df_test$e <- rnorm(nrow(df_test))
df_test$y <- df_test$x + df_test$e

# Compute dyad-robust standard errors using fastDyadRobust
dyad_cluster <- df_test[c("Var1", "Var2")]
reg_dyad <- lm(y ~ x, df_test)
se_fastDyadRobust <- fastDyadRobust(reg_dyad, dyad_cluster)

# Compute dyad-robust standard errors using dyadRobust
# testDyadRobust() is in R/testthat-helpers.R
dyad_cluster_test <- dyad_cluster
names(dyad_cluster_test) <- c("egoid", "alterid")
dyad_cluster_test$dyadid <- paste(dyad_cluster_test$egoid, dyad_cluster_test$alterid, sep = "-")
se_dyadRobust <- testDyadRobust(reg_dyad, dyad_cluster_test)

test_that("Correctly computing dyad-robust standard errors works", {
  expect_equal(se_fastDyadRobust, se_dyadRobust, tolerance = 1e-14)
})

# Check errors
test_that("Error messages", {
  expect_error(fastDyadRobust(reg_dyad, dyad_cluster[, 1, drop = FALSE]))
  expect_error(fastDyadRobust(reg_dyad, dyad_cluster[1:10, ]))
})
