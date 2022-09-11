# fastDyadRobust

<!-- badges: start -->
[![R-CMD-check](https://github.com/komatsuna4747/fixestDyadRobust/workflows/R-CMD-check/badge.svg)](https://github.com/komatsuna4747/fixestDyadRobust/actions)
<!-- badges: end -->

The `fastDyadRobust` package computes cluster-robust standard errors for dyadic data as suggested by Aronow, Samii, and Assenova (2015).

This package is built on [`dyadRobust`](https://github.com/jbisbee1/dyadRobust), which also calculates dyadic cluster-robust standard errors. With the aid of `RcppArmadillo` and `RcppParallel`, `fastDyadRobust` performs dyadic cluster-robust standard error computation much faster than `dyadRobust` as shown below.

```
# For a dataset with 100 nodes. 

            test replications elapsed relative user.self sys.self user.child sys.child
1     dyadRobust           10  69.597  644.417     5.976    1.865       0.53     0.741
2 fastDyadRobust           10   0.108    1.000     0.263    0.055       0.00     0.000
```

This package provides `fastDyadRobust::feolsDyadRobust()`, a wrapper function that estimates a linear model and computes dyadic cluster-robust standard errors using `fixest::feols()`.

```r
library(fastDyadRobust)
data(toyData)

reg <- fastDyadRobust::feolsDyadRobust(dy ~ dx1 + dx2, toyData, cluster = c("src", "dst"))
reg$se
```

```
(Intercept)         dx1         dx2 
 0.20751883  0.09215473  0.07933073 
attr(,"type")
[1] "Dyadic cluster-robust"
```


## Installation

```r
devtools::install_github("komatsuna4747/fastDyadRobust")
```

## References
Aronow, P. M., Samii, C., & Assenova, V. A. (2015). Cluster–robust variance estimation for dyadic data. _Political Analysis_, _23_(4), 564-577.

