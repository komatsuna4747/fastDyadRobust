library(magrittr)

simulate_data <- function(n_node) {
  node <-
    tibble::tibble(
      id = 1:n_node,
      x1 = rnorm(n_node),
      x2 = rnorm(n_node),
      a = rnorm(n_node)
    )

  dyad <-
    list(src = node$id, dst = node$id) %>%
    purrr::cross_df() %>%
    dplyr::filter(src < dst)

  df <-
    dyad %>%
    dplyr::left_join(node, by = c("src" = "id")) %>%
    dplyr::rename_at(c("x1", "x2", "a"), ~ paste0(., "_src")) %>%
    dplyr::left_join(node, by = c("dst" = "id")) %>%
    dplyr::rename_at(c("x1", "x2", "a"), ~ paste0(., "_dst")) %>%
    dplyr::mutate(
      dx1 = abs(x1_src - x1_dst),
      dx2 = abs(x2_src - x2_dst)
    ) %>%
    dplyr::mutate(
      de = rnorm(nrow(.)),
      dy = dx1 - 0.5 * dx2 + a_src + a_dst + de,
      dyad_id = paste(src, dst, sep = "-")
    )

  df %<>%
    dplyr::select(src, dst, dyad_id, dplyr::everything())

  return(df)
}

toyData <- withr::with_seed(334, simulate_data(100))
