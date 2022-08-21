set.seed(334)

# From jbisbee1/dyadRobust
# https://github.com/jbisbee1/dyadRobust
dyad_se_helper <- function(fit, dyad.mat, iUp, sw) {
  if (length(iUp) == 1) {
    clusUp <- as.numeric(iUp == dyad.mat$egoid) + as.numeric(iUp == dyad.mat$alterid)
    clusIndexUp <- clusUp * -99 + (1 - clusUp) * 1:nrow(dyad.mat)
  } else if (length(iUp) == nrow(sw)) {
    clusIndexUp <- iUp
  } else {
    stop("Dyad ID issue")
  }

  uj_dt <- aggregate(sw, by = list(cluster = clusIndexUp), FUN = sum)
  uj <- as.matrix(uj_dt[, 2:ncol(uj_dt)])
  rownames(uj) <- uj_dt$cluster
  meat <- crossprod(uj) / length(clusIndexUp)
  bread <- sandwich::bread(fit)
  gc()
  return((1 / nrow(sw) * (bread %*% meat %*% bread)))
}


testDyadRobust <- function(fit, dat) {
  dyad.mat <- dat[c("dyadid", "egoid", "alterid")]

  if (!is.null(fit$na.action)) {
    dyad.mat <- dyad.mat[-fit$na.action, ]
  }
  if (class(fit) == "felm" & "cX" %in% names(fit)) {
    xmat <- fit$cX
    xmat <- naresid(fit$na.action, xmat)
    if (any(alias <- is.na(coef(fit)))) xmat <- xmat[, !alias, drop = FALSE]
    wts <- weights(fit)
    if (is.null(wts)) wts <- 1
    res <- residuals(fit)
    sw <- as.vector(res) * wts * xmat
  } else {
    sw <- sandwich::estfun(x = fit)
  }

  index <- unique(c(dyad.mat$egoid, dyad.mat$alterid))

  dcrUp <- dyad_se_helper(fit, dyad.mat, iUp = index[1], sw)

  tmp <- vector(mode = "list", length = length(index) - 1)
  for (i in 2:length(index)) {
    tmp[[(i - 1)]] <- dyad_se_helper(fit, dyad.mat, index[i], sw)
  }

  for (i in 1:length(tmp)) {
    dcrUp <- dcrUp + tmp[[i]]
  }

  dcrUp2 <- dcrUp - dyad_se_helper(fit, dyad.mat, dyad.mat$dyadid, sw)
  Vhat <- dcrUp2 - (length(index) - 2) * sandwich::vcovHC(fit, type = "HC0")

  return(Vhat)
}

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
