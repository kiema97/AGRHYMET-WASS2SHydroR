# Compact SST predictor RDS files by removing repeated basin/model data.
#
# Usage:
#   Rscript scripts/compact_sst_presass2026.R
#
# Output format:
#   list(
#     predictors = list(model = data.frame(YYYY, sst_eof_*)),
#     q = data.frame(HYBAS_ID, YYYY, Q),
#     pca = list(model = list(pca_model, pca_columns, pca_explained_variance,
#                             dropped_correlated_predictors)),
#     metadata = list(...)
#   )

input_file <- "D:/CCR_AOS/ACTIVITES/AGRHYMET/2026/DCEM/PRESASS/PRESASS2026/data/predictors_v3/SST/SST_WAS_PRESASS2026.rds"
output_file <- "D:/CCR_AOS/ACTIVITES/AGRHYMET/2026/DCEM/PRESASS/PRESASS2026/data/predictors_v3/SST/SST_WAS_PRESASS2026_compact.rds"

sst_eof_columns <- function(x) {
  grep("^sst_eof_", names(x), value = TRUE)
}

copy_selected_attributes <- function(x, names) {
  out <- vector("list", length(names))
  names(out) <- names
  for (nm in names) {
    out[[nm]] <- attr(x, nm, exact = TRUE)
  }
  out
}

strip_nonessential_attributes <- function(x) {
  attributes(x) <- attributes(data.frame(x, check.names = FALSE))
  x
}

compact_sst_structure <- function(x, strict = TRUE) {
  if (!is.list(x) || is.null(names(x)) || length(x) == 0L) {
    stop("`x` must be a named list of basins.", call. = FALSE)
  }

  basin_ids <- names(x)
  model_names <- names(x[[1L]])
  if (is.null(model_names) || length(model_names) == 0L) {
    stop("Each basin must contain a named list of models.", call. = FALSE)
  }

  same_model_set <- vapply(x, function(z) identical(names(z), model_names), logical(1))
  if (!all(same_model_set)) {
    bad <- basin_ids[which(!same_model_set)[1L]]
    stop("Model names/order differ for basin: ", bad, call. = FALSE)
  }

  attr_names <- c(
    "pca_model",
    "pca_columns",
    "pca_explained_variance",
    "dropped_correlated_predictors"
  )

  predictors <- setNames(vector("list", length(model_names)), model_names)
  pca <- setNames(vector("list", length(model_names)), model_names)

  for (model in model_names) {
    first_df <- x[[1L]][[model]]
    eof_cols <- sst_eof_columns(first_df)
    if (length(eof_cols) == 0L) {
      stop("No `sst_eof_*` columns found for model: ", model, call. = FALSE)
    }

    pred_ref <- strip_nonessential_attributes(first_df[c("YYYY", eof_cols)])
    predictors[[model]] <- pred_ref
    pca[[model]] <- copy_selected_attributes(first_df, attr_names)

    if (strict) {
      for (basin in basin_ids[-1L]) {
        df <- x[[basin]][[model]]
        pred <- strip_nonessential_attributes(df[c("YYYY", eof_cols)])
        if (!identical(pred, pred_ref)) {
          stop(
            "SST predictors are not identical across basins for model `",
            model, "`. First differing basin: ", basin,
            call. = FALSE
          )
        }

        pca_here <- copy_selected_attributes(df, attr_names)
        if (!identical(pca_here, pca[[model]])) {
          stop(
            "PCA attributes are not identical across basins for model `",
            model, "`. First differing basin: ", basin,
            call. = FALSE
          )
        }
      }
    }
  }

  q_list <- setNames(vector("list", length(basin_ids)), basin_ids)
  for (basin in basin_ids) {
    first_model <- model_names[[1L]]
    q_ref <- strip_nonessential_attributes(x[[basin]][[first_model]][c("HYBAS_ID", "YYYY", "Q")])
    q_ref$HYBAS_ID <- as.character(q_ref$HYBAS_ID)

    if (strict) {
      for (model in model_names[-1L]) {
        q_here <- strip_nonessential_attributes(x[[basin]][[model]][c("HYBAS_ID", "YYYY", "Q")])
        q_here$HYBAS_ID <- as.character(q_here$HYBAS_ID)
        if (!identical(q_here, q_ref)) {
          stop(
            "`HYBAS_ID`, `YYYY`, or `Q` differ across models for basin `",
            basin, "`. First differing model: ", model,
            call. = FALSE
          )
        }
      }
    }

    q_list[[basin]] <- q_ref
  }

  q <- do.call(rbind, unname(q_list))
  rownames(q) <- NULL

  structure(
    list(
      predictors = predictors,
      q = q,
      pca = pca,
      metadata = list(
        format = "sst_compact_v1",
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
        source_file = normalizePath(input_file, winslash = "/", mustWork = FALSE),
        n_basins = length(basin_ids),
        basins = basin_ids,
        n_models = length(model_names),
        models = model_names,
        years = sort(unique(q$YYYY))
      )
    ),
    class = c("sst_compact_v1", "list")
  )
}

sst_compact_get <- function(sst, hybas_id, model, attach_pca = TRUE) {
  if (!inherits(sst, "sst_compact_v1")) {
    stop("`sst` must be an object with class `sst_compact_v1`.", call. = FALSE)
  }
  if (!model %in% names(sst$predictors)) {
    stop("Unknown model: ", model, call. = FALSE)
  }

  q <- sst$q[as.character(sst$q$HYBAS_ID) == as.character(hybas_id), , drop = FALSE]
  if (nrow(q) == 0L) {
    stop("Unknown HYBAS_ID: ", hybas_id, call. = FALSE)
  }

  pred <- sst$predictors[[model]]
  out <- merge(q, pred, by = "YYYY", all.x = TRUE, sort = FALSE)
  eof_cols <- sst_eof_columns(out)
  out <- out[order(out$YYYY), c("HYBAS_ID", "YYYY", "Q", eof_cols)]
  rownames(out) <- NULL

  if (attach_pca) {
    for (nm in names(sst$pca[[model]])) {
      attr(out, nm) <- sst$pca[[model]][[nm]]
    }
  }

  out
}

expand_sst_compact <- function(sst, attach_pca = TRUE) {
  if (!inherits(sst, "sst_compact_v1")) {
    stop("`sst` must be an object with class `sst_compact_v1`.", call. = FALSE)
  }

  basin_ids <- unique(as.character(sst$q$HYBAS_ID))
  model_names <- names(sst$predictors)

  setNames(
    lapply(basin_ids, function(basin) {
      setNames(
        lapply(model_names, function(model) {
          sst_compact_get(sst, basin, model, attach_pca = attach_pca)
        }),
        model_names
      )
    }),
    basin_ids
  )
}

validate_compact_sst <- function(original, compact, n_basins = 5L) {
  basin_ids <- head(names(original), n_basins)
  model_names <- names(original[[1L]])

  for (basin in basin_ids) {
    for (model in model_names) {
      old <- original[[basin]][[model]]
      new <- sst_compact_get(compact, basin, model, attach_pca = TRUE)

      if (!identical(strip_nonessential_attributes(old), strip_nonessential_attributes(new))) {
        stop(
          "Validation failed for basin `", basin, "` and model `", model, "`.",
          call. = FALSE
        )
      }

      for (nm in names(compact$pca[[model]])) {
        if (!identical(attr(old, nm, exact = TRUE), attr(new, nm, exact = TRUE))) {
          stop(
            "Attribute validation failed for `", nm, "`, basin `",
            basin, "`, model `", model, "`.",
            call. = FALSE
          )
        }
      }
    }
  }

  TRUE
}

message("Reading: ", input_file)
sst_old <- readRDS(input_file)

message("Compacting SST structure...")
sst_new <- compact_sst_structure(sst_old, strict = TRUE)

message("Validating compact structure on sample basins...")
validate_compact_sst(sst_old, sst_new, n_basins = 5L)

message("Saving compact file: ", output_file)
saveRDS(sst_new, output_file, compress = "xz")

old_size <- file.info(input_file)$size / 1024^3
new_size <- file.info(output_file)$size / 1024^3

message("Done.")
message("Original size: ", round(old_size, 3), " GB")
message("Compact size:  ", round(new_size, 3), " GB")
message("Reduction:     ", round(100 * (1 - new_size / old_size), 1), "%")
