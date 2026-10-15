# Forecasting pipeline.
#
# Analog of tabpfn-time-series `TabPFNTSPipeline`: owns context slicing,
# featurization, per-series backend fitting, and forecast assembly. The
# per-series loop plays the role of their `ParallelWorker` (sequential in
# v0.1; series are independent so parallelism parallelizes trivially later).

#' Construct a forecasting pipeline
#'
#' @param backend A backend from [ts_backend()] or [custom_backend()].
#' @param features A list of feature generators (see [default_features()]).
#' @param max_context_length Max trailing context rows per series
#'   (analog of `max_context_length`; sliced before featurization).
#' @param quantiles Default quantile levels for [predict_df()].
#' @return A pipeline object of class `panettone_pipeline`.
#' @export
panettone_pipeline <- function(backend,
                               features = default_features(),
                               max_context_length = 512L,
                               quantiles = c(0.1, 0.5, 0.9)) {
  max_context_length <- as.integer(max_context_length)
  if (length(max_context_length) != 1L || !is.finite(max_context_length) ||
      max_context_length < 1L) {
    stop("`max_context_length` must be a single positive integer.", call. = FALSE)
  }
  structure(
    list(
      backend = backend, features = features,
      max_context_length = max_context_length,
      quantiles = quantiles
    ),
    class = "panettone_pipeline"
  )
}

#' Forecast with a pipeline (analog of `TabPFNTSPipeline.predict_df()`)
#'
#' @param object A pipeline from [panettone_pipeline()].
#' @param context_df Historical data (`timestamp`, `target`, optional
#'   `item_id`, optional known-future covariates).
#' @param prediction_length Steps to forecast per series (mutually exclusive
#'   with `future_df`; simple path when no future covariates exist).
#' @param future_df Future timestamps (+ known covariates); `target` must be
#'   absent or all `NA`.
#' @param quantiles Quantile levels, or `NULL` for point forecasts only.
#' @param ... Reserved.
#' @return A data frame with `item_id`, `timestamp`, `.pred`, and one
#'   `q<level>` column per quantile.
#' @export
predict_df <- function(object, ...) {
  UseMethod("predict_df")
}

#' @export
predict_df.panettone_pipeline <- function(object, context_df, prediction_length = NULL,
                                          future_df = NULL, quantiles = object$quantiles,
                                          ...) {
  if (is.null(prediction_length) == is.null(future_df)) {
    stop("Provide exactly one of `prediction_length` or `future_df`.", call. = FALSE)
  }
  context <- as_ts_frame(context_df)
  if (!is.null(prediction_length)) {
    future <- make_future_frame(context, prediction_length)
  } else {
    future <- as_ts_frame(future_df)
    if ("target" %in% names(future_df)) {
      if (!all(is.na(future$target))) {
        stop("`future_df` targets must be absent or all NA.", call. = FALSE)
      }
    } else {
      future$target <- NA_real_
    }
  }

  parts <- forecast_horizon(context, future)
  context <- parts$context
  future <- parts$future

  if (!is.null(quantiles) && !supports_quantiles(object$backend)) {
    stop("This backend has no native quantile support; use `quantiles = NULL`.", call. = FALSE)
  }

  ids <- unique(context$item_id)
  max_ctx <- suppressWarnings(as.integer(object$max_context_length))
  if (length(max_ctx) != 1L || is.na(max_ctx) || max_ctx < 1L) {
    stop("`max_context_length` must be a single positive integer.", call. = FALSE)
  }
  preds <- lapply(ids, function(id) {
    ctx <- context[context$item_id == id, , drop = FALSE]
    fut <- future[future$item_id == id, , drop = FALSE]
    if (nrow(ctx) > max_ctx) {
      # Guarded: this branch runs only when nrow(ctx) > max_ctx >= 1, so
      # the start index is >= 1. seq.int() avoids `a:b` pitfalls.
      ctx <- ctx[seq.int(nrow(ctx) - max_ctx + 1L, nrow(ctx)), , drop = FALSE]
    }
    feats <- build_features(ctx, fut, object$features)
    feature_cols <- setdiff(names(feats$train), core_cols())
    fit <- fit_backend(
      object$backend,
      feats$train[, feature_cols, drop = FALSE], feats$train$target
    )
    out <- predict_backend(
      fit, feats$test[, feature_cols, drop = FALSE], quantiles = quantiles
    )
    cbind(
      data.frame(item_id = id, timestamp = fut$timestamp),
      out
    )
  })
  res <- do.call(rbind, preds)
  rownames(res) <- NULL
  res
}
