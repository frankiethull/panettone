# Time-series frame handling.
#
# Analog of tabpfn-time-series `TimeSeriesDataFrame` + `generate_test_X` +
# the `_preprocess_*` helpers in `pipeline.py`: normalize user input to a
# canonical (item_id, timestamp, target, covariates) layout and build the
# future frame for a requested horizon.

core_cols <- function() c("item_id", "timestamp", "target")

#' Normalize user input to a canonical time-series frame
#'
#' @param df A data frame with a timestamp and a numeric target column.
#' @param timestamp Name of the timestamp column (anything sortable:
#'   `POSIXct`, `Date`, or a numeric index).
#' @param target Name of the numeric target column.
#' @param item_id Name of the series-id column, or `NULL` for a single series
#'   (a dummy id is added, as in `TabPFNTSPipeline.predict_df()`).
#' @return A data frame sorted by `(item_id, timestamp)` with canonical
#'   `item_id`/`timestamp`/`target` names; remaining columns are covariates.
#' @export
as_ts_frame <- function(df, timestamp = "timestamp", target = "target",
                        item_id = NULL) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  if (!(timestamp %in% names(df))) stop("Timestamp column not found.", call. = FALSE)
  if (!(target %in% names(df))) stop("Target column not found.", call. = FALSE)
  if (!is.numeric(df[[target]])) stop("Target column must be numeric.", call. = FALSE)

  out <- df
  names(out)[names(out) == timestamp] <- "timestamp"
  names(out)[names(out) == target] <- "target"
  if (is.null(item_id)) {
    out$item_id <- 0
  } else {
    if (!(item_id %in% names(out))) stop("Item-id column not found.", call. = FALSE)
    names(out)[names(out) == item_id] <- "item_id"
  }
  out <- out[order(out$item_id, out$timestamp), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Covariates shared by context and future frames
#'
#' Mirrors `_preprocess_covariates()`: only covariates present in *both*
#' frames are usable as known-future regressors. Past-only or future-only
#' columns are dropped (with a warning when columns are lost).
#'
#' @param context A canonical frame from [as_ts_frame()].
#' @param future A canonical frame with `target = NA`.
#' @return A list with `context`, `future`, and `covariates`.
#' @export
forecast_horizon <- function(context, future) {
  cc <- setdiff(names(context), core_cols())
  fc <- setdiff(names(future), core_cols())
  keep <- intersect(cc, fc)
  dropped <- union(setdiff(cc, fc), setdiff(fc, cc))
  if (length(dropped)) {
    warning(
      "Dropping non-shared columns (not usable as known-future covariates): ",
      paste(dropped, collapse = ", "),
      call. = FALSE
    )
  }
  context <- context[, c(core_cols(), keep), drop = FALSE]
  future <- future[, c(core_cols(), keep), drop = FALSE]
  list(context = context, future = future, covariates = keep)
}

#' Build a future frame from a context frame and a horizon
#'
#' Analog of `generate_test_X()`: infers the sampling interval per series
#' (median step of the timestamp) and extends it `prediction_length` steps.
#' Future targets are `NA` (asserted downstream, as in
#' `FeatureTransformer`).
#'
#' @param context A canonical frame from [as_ts_frame()].
#' @param prediction_length Number of steps to forecast per series.
#' @return A canonical frame with `target = NA_real_`.
#' @export
make_future_frame <- function(context, prediction_length) {
  if (length(prediction_length) != 1L || prediction_length < 1L) {
    stop("`prediction_length` must be a positive integer.", call. = FALSE)
  }
  prediction_length <- as.integer(prediction_length)
  ids <- unique(context$item_id)
  parts <- lapply(ids, function(id) {
    sub <- context[context$item_id == id, , drop = FALSE]
    last_ts <- sub$timestamp[nrow(sub)]
    step <- median(diff(as.numeric(sub$timestamp)))
    if (!is.finite(step) || step <= 0) step <- 1
    if (inherits(sub$timestamp, "POSIXct")) {
      ts <- last_ts + seq_len(prediction_length) * step
    } else if (inherits(sub$timestamp, "Date")) {
      ts <- last_ts + seq_len(prediction_length) * round(step / 86400)
    } else {
      ts <- last_ts + seq_len(prediction_length) * step
    }
    data.frame(item_id = id, timestamp = ts, target = NA_real_)
  })
  future <- do.call(rbind, parts)
  rownames(future) <- NULL
  # Preserve timestamp class of the context.
  future
}
