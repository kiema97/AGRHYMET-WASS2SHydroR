test_that("skill map data joins run reports to basin geometries", {
  skip_if_not_installed("sf")

  report <- list(
    summary = tibble::tibble(
      id = c("1", "2"),
      approach = "ML",
      test_kge = c(0.2, 0.7),
      test_rmse = c(20, 10),
      overfit_flag = c(TRUE, FALSE)
    )
  )
  class(report) <- c("wass2s_run_report", "list")

  sf_basins <- sf::st_sf(
    HYBAS_ID = c("1", "2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))
    ),
    crs = 4326
  )

  dat <- .wass2s_skill_map_data(report, sf_basins, metrics = c("test_kge", "test_rmse"))

  expect_s3_class(dat, "sf")
  expect_equal(nrow(dat), 4L)
  expect_true(all(c("metric", "value", "score") %in% names(dat)))
  rmse <- dat[dat$metric == "test_rmse", ]
  expect_gt(rmse$score[rmse$HYBAS_ID == "2"], rmse$score[rmse$HYBAS_ID == "1"])
})

test_that("wass2s_plot_skill_maps returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")

  df <- tibble::tibble(
    id = c("1", "2"),
    test_kge = c(0.3, 0.6),
    test_rmse = c(30, 20)
  )

  sf_basins <- sf::st_sf(
    HYBAS_ID = c("1", "2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))
    ),
    crs = 4326
  )

  p <- wass2s_plot_skill_maps(df, sf_basins, metrics = c("test_kge", "test_rmse"))

  expect_s3_class(p, "ggplot")
})