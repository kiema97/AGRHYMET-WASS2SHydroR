# =============================================================================
# WASS2SHydroR - HYPE execution helpers
# =============================================================================

#' @noRd
wass2s__hype_now_tag <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

#' @noRd
wass2s__is_windows <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "windows")
}

#' @noRd
wass2s__hype_find_exe <- function(run_dir, exe_path = NULL) {
  run_dir <- as.character(run_dir)

  if (!is.null(exe_path) && nzchar(exe_path)) {
    exe_path <- as.character(exe_path)
    if (!fs::file_exists(exe_path)) {
      rlang::abort(glue::glue("HYPE executable not found: {exe_path}"))
    }
    return(exe_path)
  }

  exe <- fs::dir_ls(run_dir, regexp = "\\.exe$", type = "file", fail = FALSE)
  if (length(exe) == 0) rlang::abort(glue::glue("No .exe found in run_dir: {run_dir}"))
  if (length(exe) > 1)  rlang::abort(glue::glue("More than one .exe found in run_dir: {run_dir}"))
  exe[[1]]
}

#' @noRd
wass2s__hype_check_dir <- function(run_dir,
                                   required = c(
                                     "info.txt",
                                     "Pobs.txt",
                                     "TMAXobs.txt",
                                     "TMINobs.txt",
                                     "Tobs.txt",
                                     "GeoClass.txt",
                                     "GeoData.txt"
                                   ),
                                   warn_only = FALSE) {
  run_dir <- as.character(run_dir)

  if (!fs::dir_exists(run_dir)) {
    rlang::abort(glue::glue("Run directory not found: {run_dir}"))
  }

  req <- fs::path(run_dir, required)
  ok <- fs::file_exists(req)

  if (!all(ok)) {
    missing <- required[!ok]
    msg <- glue::glue(
      "HYPE run directory is missing required files in {run_dir}: {paste(missing, collapse=', ')}"
    )
    if (isTRUE(warn_only)) warning(msg) else rlang::abort(msg)
  }

  invisible(TRUE)
}

#' @noRd
wass2s__hype_capture_paths <- function(run_dir, tag) {
  list(
    stdout = fs::path(run_dir, glue::glue("hype_stdout_{tag}.log")),
    stderr = fs::path(run_dir, glue::glue("hype_stderr_{tag}.log"))
  )
}

#' @noRd
wass2s__hype_delete_outputs <- function(run_dir, clean_regex = character()) {
  run_dir <- as.character(run_dir)
  if (!length(clean_regex)) return(invisible(NULL))

  to_del <- unlist(
    purrr::map(clean_regex, function(rx) {
      fs::dir_ls(run_dir, regexp = rx, type = "file", recurse = TRUE, fail = FALSE)
    }),
    use.names = FALSE
  )

  if (length(to_del)) fs::file_delete(to_del)
  invisible(NULL)
}

#' @noRd
wass2s__expected_outputs_info <- function(run_dir, expected_files) {
  expected_files <- as.character(expected_files)
  expected_files <- expected_files[nzchar(expected_files)]
  if (!length(expected_files)) {
    return(tibble::tibble(path = character(), size = numeric(), mtime = as.POSIXct(character())))
  }

  exp_abs <- fs::path(run_dir, expected_files)
  ok <- fs::file_exists(exp_abs)
  present <- exp_abs[ok]

  if (!length(present)) {
    return(tibble::tibble(path = character(), size = numeric(), mtime = as.POSIXct(character())))
  }

  finfo <- fs::file_info(present)
  tibble::tibble(
    path  = as.character(present),
    size  = finfo$size,
    mtime = as.POSIXct(finfo$modification_time)
  )
}

#' Run HYPE in a given directory (non-GUI mode)
#'
#' This function runs the HYPE executable from `run_dir` and captures stdout/stderr
#' into log files. Optionally, it deletes old outputs before running and validates
#' that expected outputs exist after the run.
#'
#' @param run_dir Directory containing HYPE inputs (info.txt, GeoData.txt, etc.).
#' @param exe_path Optional explicit path to the HYPE executable. If NULL, the function
#'   searches for exactly one `.exe` directly inside `run_dir`.
#' @param args Character vector of command-line arguments passed to HYPE (if supported).
#' @param env Named character vector of environment variables to set during the run.
#' @param timeout_sec Maximum time to wait in seconds (0 = no timeout).
#' @param expected_files Character vector of output file paths (relative to `run_dir`)
#'   that must exist after a successful run.
#' @param clean_regex Character vector of REGEX patterns used to delete files under
#'   `run_dir` before running (e.g., `c("^hyss_.*\\\\.log$", "results/.*\\\\.txt$")`).
#' @param log_prefix Prefix for stdout/stderr log files. Defaults to a timestamp tag.
#'
#' @return A list with:
#' \itemize{
#'   \item success (logical)
#'   \item exit_status (integer)
#'   \item duration_sec (double)
#'   \item run_dir (character)
#'   \item stdout_log (character)
#'   \item stderr_log (character)
#'   \item outputs (tibble)
#' }
#'
#' @export
wass2s_hype_run <- function(run_dir,
                            exe_path = NULL,
                            args = character(),
                            env = character(),
                            timeout_sec = 300,
                            expected_files = character(),
                            clean_regex = character(),
                            log_prefix = NULL) {

  run_dir <- as.character(run_dir)
  exe_path <- wass2s__hype_find_exe(run_dir, exe_path = exe_path)
  wass2s__hype_check_dir(run_dir)

  if (is.null(log_prefix) || !nzchar(log_prefix)) log_prefix <- wass2s__hype_now_tag()
  caps <- wass2s__hype_capture_paths(run_dir, log_prefix)

  wass2s__hype_delete_outputs(run_dir, clean_regex)

  start_time <- Sys.time()

  res <- withr::with_envvar(as.list(env), {
    withr::with_dir(run_dir, {
      processx::run(
        command = exe_path,
        args = args,
        stdout = caps$stdout,
        stderr = caps$stderr,
        windows_verbatim_args = TRUE,
        error_on_status = FALSE,
        timeout = if (timeout_sec > 0) timeout_sec else Inf
      )
    })
  })

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  outputs <- wass2s__expected_outputs_info(run_dir, expected_files)

  success <- identical(res$status, 0L) &&
    (length(expected_files) == 0 || all(fs::file_exists(fs::path(run_dir, expected_files))))

  list(
    success = success,
    exit_status = as.integer(res$status),
    duration_sec = duration,
    run_dir = run_dir,
    stdout_log = as.character(caps$stdout),
    stderr_log = as.character(caps$stderr),
    outputs = outputs
  )
}

#' Run HYPE in "watch-log" mode (Windows GUI executables)
#'
#' Some HYPE builds may open a final Yes/No dialog and keep the process alive.
#' This function starts HYPE as a background process, waits for a log file matching
#' `log_regex` to appear and become stable (file size unchanged for `stable_duration`),
#' then attempts to close the window (Windows) and falls back to kill if needed.
#'
#' @param run_dir Directory containing HYPE inputs.
#' @param exe_path Optional explicit path to the HYPE executable. If NULL, searches for
#'   exactly one `.exe` inside `run_dir`.
#' @param args Character vector of command-line arguments passed to HYPE (if supported).
#' @param env Named character vector of environment variables to set during the run.
#' @param timeout_sec Maximum time to wait in seconds (0 = no timeout).
#' @param expected_files Expected output files (relative to `run_dir`) to validate.
#' @param clean_regex REGEX patterns used to delete files under `run_dir` before running.
#' @param log_prefix Prefix for stdout/stderr logs (defaults to timestamp tag).
#' @param log_regex REGEX to detect the "completion" log file in `run_dir`
#'   (default: `"^hyss_.*\\\\.log$"`).
#' @param check_interval Seconds between log checks.
#' @param stable_duration Seconds of unchanged log size to consider the run finished.
#'
#' @return Same structure as `wass2s_hype_run()`, plus `watched_log` (character or NA).
#'
#' @export
wass2s_hype_run_watchlog <- function(run_dir,
                                     exe_path = NULL,
                                     args = character(),
                                     env = character(),
                                     timeout_sec = 150,
                                     expected_files = character(),
                                     clean_regex = c("^hyss_.*\\.log$"),
                                     log_prefix = NULL,
                                     log_regex = "^hyss_.*\\.log$",
                                     check_interval = 10,
                                     stable_duration = 30) {

  run_dir <- as.character(run_dir)
  exe_path <- wass2s__hype_find_exe(run_dir, exe_path = exe_path)
  wass2s__hype_check_dir(run_dir)

  if (is.null(log_prefix) || !nzchar(log_prefix)) log_prefix <- wass2s__hype_now_tag()
  caps <- wass2s__hype_capture_paths(run_dir, log_prefix)

  wass2s__hype_delete_outputs(run_dir, clean_regex)

  start_time <- Sys.time()
  deadline <- if (timeout_sec > 0) start_time + timeout_sec else as.POSIXct(Inf, origin = "1970-01-01")

  # Start process (supervised background process)
  p <- withr::with_envvar(as.list(env), {
    withr::with_dir(run_dir, {
      processx::process$new(
        command = exe_path,
        args = args,
        stdout = caps$stdout,
        stderr = caps$stderr,
        windows_verbatim_args = TRUE,
        supervise = TRUE
      )
    })
  })

  pid <- p$get_pid()

  # 1) Wait for a completion log to appear
  watched_log <- NA_character_

  repeat {
    if (Sys.time() > deadline) break
    logs <- fs::dir_ls(run_dir, regexp = log_regex, type = "file", fail = FALSE)
    if (length(logs)) {
      # pick the most recently modified log
      fi <- fs::file_info(logs)
      watched_log <- as.character(logs[which.max(fi$modification_time)])
      break
    }
    Sys.sleep(min(0.5, check_interval))
  }

  # 2) If log exists, monitor until stable size
  if (!is.na(watched_log)) {
    last_size <- NA_real_
    stable_for <- 0

    repeat {
      if (Sys.time() > deadline) break
      fi <- fs::file_info(watched_log)
      sz <- fi$size

      if (!is.na(sz) && !is.na(last_size) && identical(sz, last_size)) {
        stable_for <- stable_for + check_interval
        if (stable_for >= stable_duration) break
      } else {
        stable_for <- 0
        last_size <- sz
      }

      Sys.sleep(check_interval)
    }
  }

  # 3) Attempt graceful close on Windows (CloseMainWindow), else kill
  if (p$is_alive()) {
    if (wass2s__is_windows()) {
      ps_cmd <- paste0(
        "$p = Get-Process -Id ", pid, " -ErrorAction SilentlyContinue; ",
        "if ($p) { ",
        " if ($p.MainWindowHandle -ne 0) { $null = $p.CloseMainWindow(); Start-Sleep -Seconds 5; if (!$p.HasExited) { $p.Kill() } } ",
        " else { $p.Kill() }",
        "}"
      )
      try(
        suppressWarnings(system2("powershell", c("-NoProfile", "-Command", ps_cmd), stdout = FALSE, stderr = FALSE)),
        silent = TRUE
      )
    }
  }

  # 4) Ensure exit
  if (p$is_alive()) p$kill()

  # In practice, get_exit_status() may be NA if killed; handle gracefully
  exit_status <- suppressWarnings(p$get_exit_status())
  if (is.na(exit_status)) exit_status <- if (p$is_alive()) 1L else 0L

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  outputs <- wass2s__expected_outputs_info(run_dir, expected_files)

  success <- identical(as.integer(exit_status), 0L) &&
    (length(expected_files) == 0 || all(fs::file_exists(fs::path(run_dir, expected_files))))

  list(
    success = !success,
    exit_status = as.integer(exit_status),
    duration_sec = duration,
    run_dir = run_dir,
    stdout_log = as.character(caps$stdout),
    stderr_log = as.character(caps$stderr),
    outputs = outputs,
    watched_log = watched_log
  )
}

#' Run multiple HYPE scenarios (one directory each)
#'
#' @param run_dirs Character vector of scenario directories.
#' @param mode Execution mode: `"watchlog"` (default) or `"run"`.
#' @param ... Passed to `wass2s_hype_run_watchlog()` or `wass2s_hype_run()`.
#'
#' @return A tibble with one row per scenario.
#' @export
wass2s_hype_run_batch <- function(run_dirs, mode = c("watchlog", "run"), ...) {
  mode <- match.arg(mode)
  run_dirs <- as.character(run_dirs)
  if (!length(run_dirs)) rlang::abort("run_dirs must be a non-empty character vector.")

  purrr::map_dfr(run_dirs, function(rd) {
    rd <- as.character(rd)

    res <- tryCatch(
      if (identical(mode, "watchlog")) wass2s_hype_run_watchlog(run_dir = rd, ...)
      else wass2s_hype_run(run_dir = rd, ...),
      error = function(e) {
        list(
          success = FALSE,
          exit_status = NA_integer_,
          duration_sec = NA_real_,
          run_dir = rd,
          stdout_log = NA_character_,
          stderr_log = NA_character_,
          outputs = tibble::tibble()
        )
      }
    )

    tibble::tibble(
      run_dir = rd,
      success = isTRUE(res$success),
      exit_status = res$exit_status,
      duration_sec = res$duration_sec,
      stdout_log = res$stdout_log,
      stderr_log = res$stderr_log,
      n_outputs = if (!is.null(res$outputs)) nrow(res$outputs) else 0L
    )
  })
}
