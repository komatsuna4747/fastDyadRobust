library(fastDyadRobust)
data("toyData")
df_pair <- toyData[c("src", "dst")]

# Install dyadRobust if not installed
if (!"dyadRobust" %in% installed.packages()[,"Package"]) {
  devtools::install_github("jbisbee1/dyadRobust")
}

library(dyadRobust)

reg <- lm(dy ~ dx1 + dx2, data = toyData)

benchmark <-
  rbenchmark::benchmark(
    dyadRobust = {
      vcov_dyadRobust <-
        dyadRobust::dyadRobust(
          fit = reg,
          dat = toyData,
          dyadid = "dyad_id",
          egoid = "src",
          alterid = "dst",
          parallel = TRUE
        )
    },
    fastDyadRobust = {
      vcov_fastDyadRobust <-
        fastDyadRobust::vcovDyadRobust(
          fit = reg,
          dyad_cluster = df_pair
        )
    },
    replications = 10
  )

max(vcov_dyadRobust$Vhat - vcov_fastDyadRobust)

benchmark
