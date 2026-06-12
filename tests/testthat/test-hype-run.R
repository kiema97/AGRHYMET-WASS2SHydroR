test_that("hype watchlog reports successful completed runs", {
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
  skip_if_not(file.exists(rscript))

  run_dir <- withr::local_tempdir()
  file.create(file.path(
    run_dir,
    c("info.txt", "Pobs.txt", "TMAXobs.txt", "TMINobs.txt", "Tobs.txt", "GeoClass.txt", "GeoData.txt")
  ))
  res <- wass2s_hype_run_watchlog(
    run_dir = run_dir,
    exe_path = rscript,
    args = c("-e", "file.create('hyss_done.log')"),
    clean_regex = character(),
    timeout_sec = 10,
    check_interval = 0.1,
    stable_duration = 0.1
  )

  expect_true(res$success)
  expect_identical(res$exit_status, 0L)
})
