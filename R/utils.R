#' Null-coalescing operator
#'
#' Retourne `y` si `x` est `NULL`, sinon `x`.
#'
#' @name %||%
#' @usage x %||% y
#' @param x,y objets
#' @return x si non-NULL, sinon y
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Pipe operator
#' @name %>%
#' @rdname pipe
#' @noRd
#' @keywords internal
#' @importFrom magrittr %>%
magrittr::`%>%`

#' Check availability of suggested packages (internal)
#'
#' This helper function verifies whether suggested packages are installed,
#' without forcing hard dependencies. It is mainly used to enable or skip
#' certain features in package workflows depending on whether optional
#' backend packages are available.
#'
#' @param pkg Character vector. Name(s) of the package(s) to check.
#' @param feature Optional character. Description of the feature or functionality
#'   that depends on these packages (used only in messages).
#' @param strict Logical. If `TRUE`, the function raises an error when any
#'   required package is missing. If `FALSE` (default), returns `FALSE` and
#'   emits an informative message for missing packages.
#' @param version Optional character vector. Minimum required version for
#'   each package. Recycled if length 1.
#' @param attach Logical. If `TRUE`, attempts to attach the package to the
#'   search path when available. Defaults to `FALSE`.
#'
#' @return Logical (`TRUE` if all packages are available and meet version
#'   requirements, `FALSE` otherwise). In `strict = TRUE` mode, an error
#'   is thrown instead of returning `FALSE`.
#'
#' @details
#' This function is intended for internal use to manage "soft dependencies"
#' declared in the `Suggests` field. It provides flexible checking for
#' multiple packages with optional version requirements and attachment.
#'
#' When `attach = TRUE`, the function will attempt to load the package
#' using `library(pkg, character.only = TRUE)` for each available package.
#' This can be useful when you need the package attached for its functionality.
#'
#' @examples
#' \dontrun{
#' # Check for single package
#' engine_is_available("xgboost", feature = "boosted trees")
#'
#' # Check for multiple packages with version requirements
#' engine_is_available(
#'   c("ggplot2", "dplyr"),
#'   version = c("3.4.0", "1.1.0"),
#'   feature = "advanced plotting"
#' )
#' }
#'
#' @keywords internal
engine_is_available <- function(pkg, feature = NULL, strict = FALSE,
                                version = NULL, attach = FALSE) {

  # Input validation
  if (!is.character(pkg) || length(pkg) == 0) {
    rlang::abort("'pkg' must be a non-empty character vector.")
  }

  if (!is.logical(strict) || length(strict) != 1) {
    rlang::abort("'strict' must be a single logical value.")
  }

  if (!is.logical(attach) || length(attach) != 1) {
    rlang::abort("'attach' must be a single logical value.")
  }

  # Recycle version if needed
  if (!is.null(version)) {
    if (!is.character(version)) {
      rlang::abort("'version' must be a character vector.")
    }
    if (length(version) == 1) {
      version <- rep(version, length(pkg))
    }
    if (length(version) != length(pkg)) {
      rlang::abort("'version' must be length 1 or same length as 'pkg'.")
    }
  }

  # Check package availability and versions
  available <- logical(length(pkg))
  missing_pkgs <- character()
  version_issues <- character()

  for (i in seq_along(pkg)) {
    pkg_name <- pkg[i]
    req_version <- if (!is.null(version)) version[i] else NULL

    # Check if package is installed
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg_name)
      available[i] <- FALSE
      next
    }

    # Check version if required
    if (!is.null(req_version)) {
      installed_version <- utils::packageVersion(pkg_name)
      if (installed_version < req_version) {
        version_issues <- c(
          version_issues,
          sprintf("%s (installed: %s, required: %s)",
                  pkg_name, installed_version, req_version)
        )
        available[i] <- FALSE
        next
      }
    }

    available[i] <- TRUE

    # Attach package if requested and available
    if (attach && available[i]) {
      tryCatch(
        {
          library(pkg_name, character.only = TRUE, quietly = TRUE)
        },
        error = function(e) {
          rlang::warn(sprintf("Failed to attach package '%s': %s", pkg_name, e$message))
        }
      )
    }
  }

  all_available <- all(available)

  if (!all_available) {
    # Construct informative message
    msg_parts <- character()

    if (!is.null(feature)) {
      msg_parts <- c(msg_parts, sprintf("Feature '%s' unavailable:", feature))
    } else {
      msg_parts <- c(msg_parts, "Skipping:")
    }

    if (length(missing_pkgs) > 0) {
      msg_parts <- c(msg_parts,
                     sprintf("packages not installed: %s %s",
                             paste(missing_pkgs, collapse = ", "), ".\nTry to install missing package by using install.packages(), then retry."))
    }

    if (length(version_issues) > 0) {
      msg_parts <- c(msg_parts,
                     sprintf("version issues: %s",
                             paste(version_issues, collapse = "; ")))
    }

    msg <- paste(msg_parts, collapse = " ")

    if (strict) {
      rlang::abort(
        message = msg,
        class = "softdep_missing",
        package = pkg[!available],
        feature = feature,
        missing_packages = missing_pkgs,
        version_issues = if (length(version_issues) > 0) version_issues else NULL
      )
    } else {
      rlang::inform(msg)
    }
  }

  return(all_available)
}


.need <- function(pkg) if (!requireNamespace(pkg, quietly = TRUE))
  stop(sprintf("Package '%s' is required for this feature. Install it.", pkg), call. = FALSE)






# R/internal_missing_handling.R

#' Validate and impute numeric columns with missing-value guard
#'
#' @description
#' Internal utility to (i) check the fraction of missing values per column,
#' (ii) stop if it exceeds a user-defined threshold, and (iii) impute remaining
#' missing values using a simple strategy (median by default). Optionally, it can
#' enforce that the resulting column has non-zero variance.
#'
#' @param df A data.frame or tibble.
#' @param cols Character vector of column names to check/impute (must be numeric).
#' @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#'   values per column before stopping (default \code{0.20} = 20\%).
#' @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#'   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#'   standard deviation after imputation (default \code{TRUE}).
#'
#' @return The input \code{df} with the specified columns possibly imputed in place.
#'
#' @details
#' - The function stops with an informative error when:
#'   (a) a target column is missing in \code{df};
#'   (b) the fraction of missing values in a column exceeds \code{max_na_frac};
#'   (c) all values are \code{NA} (imputation is undefined);
#'   (d) \code{require_variance = TRUE} and the post-imputation standard deviation is zero.
#' - Only numeric columns are supported.
#' @keywords internal
#' @noRd
.sanitize_numeric_columns <- function(df,
                                      cols,
                                      max_na_frac = 0.30,
                                      impute = c("median", "mean", "none"),
                                      require_variance = TRUE) {
  impute <- match.arg(impute)

  if (!is.data.frame(df)) {
    stop("`.sanitize_numeric_columns()`: `df` must be a data.frame/tibble.", call. = FALSE)
  }
  if (!is.character(cols) || length(cols) < 1L) {
    stop("`.sanitize_numeric_columns()`: `cols` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(max_na_frac) || length(max_na_frac) != 1L || max_na_frac < 0 || max_na_frac > 1) {
    stop("`.sanitize_numeric_columns()`: `max_na_frac` must be in [0, 1].", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols)) {
    stop("`.sanitize_numeric_columns()`: columns not found: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  for (col in cols) {
    x <- df[[col]]

    if (!is.numeric(x)) {
      stop("`.sanitize_numeric_columns()`: column '", col, "' must be numeric.", call. = FALSE)
    }

    n_total <- length(x)
    n_na    <- sum(is.na(x))
    frac_na <- n_na / n_total

    # Guard 1: too many missing values
    if (frac_na > max_na_frac) {
      stop(sprintf("Too many missing values in '%s' (%.1f%% > %.1f%% allowed).",
                   col, 100 * frac_na, 100 * max_na_frac), call. = FALSE)
    }

    # Guard 2: all NA (cannot impute)
    if (n_na == n_total) {
      stop(sprintf("All values in '%s' are NA; imputation is not possible.", col), call. = FALSE)
    }

    # Optional imputation
    if (n_na > 0 && impute != "none") {
      fill <- switch(
        impute,
        median = stats::median(x, na.rm = TRUE),
        mean   = base::mean(x, na.rm = TRUE)
      )
      if (!is.finite(fill)) {
        stop(sprintf("Computed %s for '%s' is not finite; cannot impute.",
                     impute, col), call. = FALSE)
      }
      x[is.na(x)] <- fill
      df[[col]] <- x
    }

    # Optional variance requirement
    if (isTRUE(require_variance)) {
      s <- stats::sd(df[[col]], na.rm = TRUE)
      if (!is.finite(s) || s == 0) {
        stop(sprintf("Column '%s' has zero standard deviation after checks/imputation.", col),
             call. = FALSE)
      }
    }
  }

  df
}


