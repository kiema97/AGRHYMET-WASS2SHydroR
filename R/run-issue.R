#' Safely write an output only when allowed or objectively better
#'
#' This helper implements a conservative operational update policy: existing
#' outputs are kept unless \code{update = TRUE} or the new summary is better
#' according to \code{wass2s_compare_forecast_versions()}.
#'
#' @param object Object to write.
#' @param path Destination path.
#' @param summary_new Named list or one-row data frame describing the new
#'   result.
#' @param summary_old Optional summary for the existing result. If \code{NULL}
#'   and the existing file is an RDS object with a \code{"wass2s_summary"}
#'   attribute, that attribute is used.
#' @param update Logical. If \code{TRUE}, allow replacing an existing file.
#' @param backup Logical. If \code{TRUE}, rename the previous file before
#'   writing the new one.
#' @param writer Function with signature \code{function(object, path)}. Defaults
#'   to \code{saveRDS()}.
#' @param compare_fun Function used to compare \code{summary_new} and
#'   \code{summary_old}.
#'
#' @return A list describing the write decision.
#'
#' @export
wass2s_safe_write_result <- function(object,
                                     path,
                                     summary_new = list(),
                                     summary_old = NULL,
                                     update = FALSE,
                                     backup = TRUE,
                                     writer = function(object, path) saveRDS(object, path),
                                     compare_fun = wass2s_compare_forecast_versions) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (!file.exists(path)) {
    attr(object, "wass2s_summary") <- summary_new
    writer(object, path)
    return(list(decision = "written", path = path, backup = NA_character_, comparison = NULL))
  }

  if (is.null(summary_old)) {
    old_obj <- tryCatch(readRDS(path), error = function(e) NULL)
    summary_old <- attr(old_obj, "wass2s_summary", exact = TRUE)
  }

  comparison <- NULL
  if (!is.null(summary_old) && length(summary_old) > 0L && length(summary_new) > 0L) {
    comparison <- compare_fun(summary_new, summary_old)
  }

  should_replace <- isTRUE(update)
  if (!should_replace && !is.null(comparison)) {
    should_replace <- identical(comparison$decision, "new")
  }

  if (!should_replace) {
    return(list(
      decision = "kept_existing",
      path = path,
      backup = NA_character_,
      comparison = comparison
    ))
  }

  backup_path <- NA_character_
  if (isTRUE(backup)) {
    stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
    backup_path <- paste0(path, ".backup-", stamp)
    moved <- file.rename(path, backup_path)
    if (!isTRUE(moved)) {
      stop("Could not create backup before replacing existing output: ", path, call. = FALSE)
    }
  }

  attr(object, "wass2s_summary") <- summary_new
  writer(object, path)
  list(decision = "replaced", path = path, backup = backup_path, comparison = comparison)
}

#' Run one operational WASS2S forecast issue
#'
#' Lightweight orchestrator for operational runs. It can audit the CDS archive,
#' build a repair plan, execute user-supplied workflow steps, and write a run
#' manifest. Heavy modelling steps are passed as functions so STAT, ML, HYDRO
#' and consolidation workflows can evolve independently.
#'
#' @param config Named list. Recognized entries include \code{run_id},
#'   \code{issue_date}, \code{target}, \code{approach}, \code{archive},
#'   \code{manifest_path}, and \code{stop_on_archive_errors}.
#' @param steps Named list of functions. Each function is called as
#'   \code{step(context)} and should return an object to be stored under its
#'   step name.
#' @param update Logical forwarded to safe writing policies through the run
#'   context.
#' @param dry_run Logical. If \code{TRUE}, audit and manifest are produced but
#'   workflow steps are not executed.
#' @param quiet Logical. Suppress progress messages.
#'
#' @return A list with class \code{"wass2s_issue_run"} containing context,
#'   audit, repair plan, step results and manifest.
#'
#' @export
wass2s_run_issue <- function(config,
                             steps = list(),
                             update = FALSE,
                             dry_run = FALSE,
                             quiet = TRUE) {
  if (!is.list(config)) stop("config must be a named list.", call. = FALSE)
  if (!is.list(steps)) stop("steps must be a named list of functions.", call. = FALSE)
  if (length(steps) && (is.null(names(steps)) || any(!nzchar(names(steps))))) {
    stop("steps must be a named list.", call. = FALSE)
  }

  warnings <- character()
  errors <- character()
  audit <- NULL
  repair_plan <- tibble::tibble()
  results <- list()

  context <- list(
    config = config,
    update = update,
    dry_run = dry_run,
    issue_date = config$issue_date %||% NULL,
    target = config$target %||% NULL
  )

  if (!is.null(config$archive)) {
    archive_args <- config$archive
    audit <- tryCatch(
      do.call(wass2s_validate_archive, archive_args),
      error = function(e) {
        errors <<- c(errors, paste0("archive audit failed: ", e$message))
        NULL
      }
    )
    if (!is.null(audit)) {
      repair_plan <- wass2s_repair_archive_plan(audit)
      context$archive_audit <- audit
      context$repair_plan <- repair_plan

      if (nrow(repair_plan) > 0L) {
        msg <- paste0(nrow(repair_plan), " archive chunks require repair.")
        warnings <- c(warnings, msg)
        if (!quiet) message(msg)
        if (isTRUE(config$stop_on_archive_errors)) {
          errors <- c(errors, "Stopped because archive repair is required.")
        }
      }
    }
  }

  if (!length(errors) && !isTRUE(dry_run) && length(steps)) {
    for (nm in names(steps)) {
      step_fun <- steps[[nm]]
      if (!is.function(step_fun)) {
        errors <- c(errors, paste0("step '", nm, "' is not a function."))
        next
      }
      if (!quiet) message("Running step: ", nm)
      step_res <- tryCatch(
        step_fun(context),
        error = function(e) {
          errors <<- c(errors, paste0("step '", nm, "' failed: ", e$message))
          NULL
        }
      )
      results[[nm]] <- step_res
      context$results <- results
      if (length(errors)) break
    }
  }

  manifest <- wass2s_run_manifest(
    run_id = config$run_id %||% NULL,
    approach = config$approach %||% names(steps),
    issue_date = config$issue_date %||% NULL,
    target = config$target %||% NULL,
    config = config,
    inputs = list(archive = config$archive %||% NULL),
    outputs = list(results = names(results)),
    scores = config$scores %||% NULL,
    warnings = warnings,
    errors = errors,
    extra = list(
      dry_run = dry_run,
      update = update,
      repair_plan_n = nrow(repair_plan)
    )
  )

  if (!is.null(config$manifest_path)) {
    wass2s_write_manifest(manifest, config$manifest_path, overwrite = TRUE)
  }

  structure(
    list(
      context = context,
      audit = audit,
      repair_plan = repair_plan,
      results = results,
      manifest = manifest,
      warnings = warnings,
      errors = errors,
      ok = length(errors) == 0L
    ),
    class = c("wass2s_issue_run", "list")
  )
}
