# =============================================================================
# WASS2SHydroR - Scenario orchestration (multiple forcing models)
# =============================================================================

#' Run one model workflow: prepare OBS -> update info.txt resultdir -> run HYPE
#'
#' Typical usage: in a forcing directory containing `<model>_PRCP.txt`, `<model>_TMAX.txt`, ...
#' you call this function with `resultdir = model` and `prefix = paste0(model, "_")`.
#'
#' @param run_dir HYPE run directory (contains info.txt and static inputs).
#' @param forcing_dir Directory containing forcing files for the selected model.
#' @param resultdir Result directory name for this run (often the model ID).
#' @param exe_path Optional explicit path to HYPE executable. If NULL, searches for a single `.exe` in `run_dir`.
#' @param timeout_sec Timeout for the HYPE run.
#' @param args Command-line arguments passed to the executable (if supported).
#' @param env Named character vector of environment variables during the run.
#' @param expected_files Output files expected (relative to `run_dir`). If empty, defaults to `<resultdir>/timeCOUT.txt`.
#' @param clean_regex REGEX patterns to delete before run (applied under `run_dir`).
#' @param log_prefix Prefix for stdout/stderr logs.
#' @param log_regex REGEX for completion log detection (watchlog mode only).
#' @param check_interval Seconds between watch checks (watchlog mode only).
#' @param stable_duration Seconds of unchanged log size (watchlog mode only).
#' @param ... Additional arguments forwarded to `wass2s_hype_prepare_obs()` (e.g., obsid, date_from/date_to, round_digits).
#'
#' @return A one-row tibble with run metadata.
#' @export
wass2s_hype_run_one_model <- function(run_dir,
                                      forcing_dir,
                                      resultdir,
                                      exe_path = NULL,
                                      timeout_sec = 300,
                                      args = character(),
                                      env = character(),
                                      expected_files = character(),
                                      clean_regex = c("^hyss_.*\\.log$"),
                                      log_prefix = NULL,
                                      log_regex = "^hyss_.*\\.log$",
                                      check_interval = 10,
                                      stable_duration = 30,
                                      ...) {
  run_dir <- as.character(run_dir)
  forcing_dir <- as.character(forcing_dir)
  resultdir <- as.character(resultdir)

  # 1) Prepare OBS for this model (write into run_dir)
  wass2s_hype_prepare_obs(
    forcing_dir = forcing_dir,
    run_dir = run_dir,
    ...
  )

  # 2) Update info.txt to write outputs into ./<resultdir>/
  result_path <- wass2s_hype_update_resultdir(run_dir, resultdir)

  # 3) Default expected output file
  if (!length(expected_files)) {
    expected_files <- file.path(resultdir, "timeCOUT.txt")
  } else {
    # Ensure expected files are relative to run_dir (user may pass "timeCOUT.txt")
    expected_files <- as.character(expected_files)
  }

  # 4) Clean: by default, wipe hyss logs + anything under resultdir (txt/log)
  # Use REGEX; escape slashes properly
  clean_regex_final <- unique(c(
    clean_regex,
    glue::glue("^({stringr::str_replace_all(resultdir, '([\\\\.\\\\+\\\\*\\\\?\\\\^\\\\$\\\\(\\\\)\\\\[\\\\]\\\\{\\\\}\\\\|])', '\\\\\\\\1')})/.*\\\\.(txt|log)$")
  ))

  # 5) Run HYPE
  res <- wass2s_hype_run_watchlog(
    run_dir = run_dir,
    exe_path = exe_path,
    args = args,
    env = env,
    timeout_sec = timeout_sec,
    expected_files = expected_files,
    clean_regex = clean_regex_final,
    log_prefix = log_prefix,
    log_regex = log_regex,
    check_interval = check_interval,
    stable_duration = stable_duration
  )

  tibble::tibble(
    resultdir = resultdir,
    success = isTRUE(res$success),
    exit_status = res$exit_status,
    duration_sec = res$duration_sec,
    result_dir = as.character(result_path),
    stdout_log = res$stdout_log,
    stderr_log = res$stderr_log,
    n_outputs = if (!is.null(res$outputs)) nrow(res$outputs) else 0L
  )
}

#' Run the full workflow for all models discovered in a forcing directory
#'
#' This function discovers model IDs with `wass2s_hype_get_model_ids()` and runs each model by:
#'   - writing obs files into `run_dir` from `<model>_PRCP.txt` etc.
#'   - updating `info.txt` resultdir to `./<model>/`
#'   - executing HYPE (watchlog mode by default)
#'
#' @param run_dir HYPE run directory.
#' @param forcing_dir Directory containing `<model>_(PRCP|TMAX|TMIN|TMEAN).txt`.
#' @param exclude_models Character vector of model IDs to skip.
#' @param exe_path Optional explicit path to HYPE executable.
#' @param timeout_sec Timeout for each run.
#' @param ... Extra args forwarded to `wass2s_hype_run_one_model()` and `wass2s_hype_prepare_obs()`
#'   (e.g., obsid, date_from/date_to, round_digits, log_regex, etc.).
#'
#' @return Tibble with one row per model.
#' @export
wass2s_hype_run_all_models <- function(run_dir,
                                       forcing_dir,
                                       exclude_models = character(),
                                       exe_path = NULL,
                                       timeout_sec = 300,
                                       ...) {

  run_dir <- as.character(run_dir)
  forcing_dir <- as.character(forcing_dir)
  exclude_models <- as.character(exclude_models)

  mods <- wass2s_hype_get_model_ids(forcing_dir)
  mods <- setdiff(mods, exclude_models)

  if (!length(mods)) {
    rlang::abort("No model IDs discovered in forcing_dir after exclusions.")
  }

  purrr::map_dfr(mods, function(m) {
    wass2s_hype_run_one_model(
      run_dir = run_dir,
      forcing_dir = forcing_dir,
      resultdir = m,
      exe_path = exe_path,
      timeout_sec = timeout_sec,
      prefix = paste0(m, "_"),
      ...
    )
  })
}
