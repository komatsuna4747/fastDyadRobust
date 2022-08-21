# From jbisbee1/dyadRobust
# https://github.com/jbisbee1/dyadRobust
dyad_meat_helper <- function(dyad.mat, iUp, sw) {
  if (length(iUp) == 1) {
    clusUp <- as.numeric(iUp == dyad.mat$egoid) + as.numeric(iUp == dyad.mat$alterid)
    clusIndexUp <- clusUp * (-99) + (1 - clusUp) * 1:nrow(dyad.mat)
  } else if (length(iUp) == nrow(sw)) {
    clusIndexUp <- iUp
  } else {
    stop("Dyad ID issue")
  }

  uj_dt <- aggregate(sw, by = list(cluster = clusIndexUp), FUN = sum)
  uj <- as.matrix(uj_dt[, 2:ncol(uj_dt)])
  rownames(uj) <- uj_dt$cluster
  meat <- crossprod(uj) / length(clusIndexUp)
  return((1 / nrow(sw)) * meat)
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
  bread <- sandwich::bread(fit)

  dcrUp <- dyad_meat_helper(dyad.mat, iUp = index[1], sw)

  tmp <- vector(mode = "list", length = length(index) - 1)
  for (i in 2:length(index)) {
    tmp[[(i - 1)]] <- dyad_meat_helper(dyad.mat, index[i], sw)
  }

  for (i in 1:length(tmp)) {
    dcrUp <- dcrUp + tmp[[i]]
  }

  dcrUp <- bread %*% dcrUp %*% bread

  dcrUp2 <- dcrUp - bread %*% dyad_meat_helper(dyad.mat, dyad.mat$dyadid, sw) %*% bread
  Vhat <- dcrUp2 - (length(index) - 2) * sandwich::vcovHC(fit, type = "HC0")

  return(Vhat)
}
