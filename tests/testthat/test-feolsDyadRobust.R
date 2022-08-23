set.seed(1878237564)

df_test <- make_test_data(repeated = FALSE)

# Run regression using feolsDyadRobust
reg_feols <- feolsDyadRobust(y ~ x | Var1 + Var2, data = df_test, cluster = c("Var1", "Var2"))
vcov(reg_feols)

test_that("Check if diagonal elements of vcov are all positive", {
  expect_true(diag(vcov(reg_feols)) > 0)
})

test_that("Error handling correctly works", {
  # No cluster argument
  expect_error(feolsDyadRobust(y ~ x | Var1 + Var2, data = df_test))

  # Inappropriate cluster argument
  expect_error(feolsDyadRobust(y ~ x | Var1 + Var2, data = df_test, cluster = ~ Var1 + Var2))
  expect_error(feolsDyadRobust(y ~ x | Var1 + Var2, data = df_test, cluster = "Var1"))
})
