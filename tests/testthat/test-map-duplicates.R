test_that("generic map plotters tolerate duplicated basin rows", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")

  sf_basins <- sf::st_sf(
    HYBAS_ID = c("1", "1", "2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 0.5, 0, 0.5, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0.5, 0, 1, 0, 1, 1, 0.5, 1, 0.5, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))
    ),
    crs = 4326
  )

  probs <- data.frame(
    HYBAS_ID = c("1", "1", "2"),
    p_below = c(0.2, 0.4, 0.7),
    p_normal = c(0.3, 0.2, 0.2),
    p_above = c(0.5, 0.4, 0.1),
    class_hat = c("above", "above", "below")
  )

  p_prob <- wass2s_plot_map(sf_basins, probs, basin_col = "HYBAS_ID")
  p_class <- wass2s_plot_map(sf_basins, probs, basin_col = "HYBAS_ID", type = "class")

  expect_s3_class(p_prob, "ggplot")
  expect_s3_class(p_class, "ggplot")
  expect_silent(ggplot2::ggplot_build(p_prob))
  expect_silent(ggplot2::ggplot_build(p_class))
})