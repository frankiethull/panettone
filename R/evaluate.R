# Rolling-origin evaluation and baselines (Phase-1 harness).

#' Mean absolute error
#' @param actual,forecast Numeric vectors of equal length.
#' @return Single numeric.
#' @export
mae <- function(actual, forecast) mean(abs(actual - forecast))

#' Root mean squared error
#' @param actual,forecast Numeric vectors of equal length.
#' @return Single numeric.
#' @export
rmse <- function(actual, forecast) sqrt(mean((actual - forecast)^2))

#' Rolling-origin evaluation of a pipeline
#'
#' For each origin `o` (a predictable position in `y`), fits the pipeline on
#' `y[seq_len(o - 1)]` and scores the 1-step-ahead forecast. All backend
#' state (including Fiorino shifts) is re-fit per origin from history only.
#'
#' NOTE: one engine per process (see `R/backend.R`). For brulee backends
#' this runs in the current process; run one `Rscript` per engine and
#' combine the returned frames.
#'
#' @param y Numeric vector (the full series).
#' @param origins Integer positions to forecast (each `> lookback_min`).
#' @param pipeline A pipeline from [panettone_pipeline()].
#' @param timestamp Optional timestamp vector parallel to `y`.
#' @return A data frame with `origin`, `actual`, `.pred`, `abserr`.
#' @export
rolling_eval <- function(y, origins, pipeline, timestamp = seq_along(y)) {
  stopifnot(is.numeric(y), length(y) == length(timestamp))
  out <- lapply(origins, function(o) {
    ctx_df <- data.frame(timestamp = timestamp[seq_len(o - 1)], target = y[seq_len(o - 1)])
    fc <- predict_df(pipeline, ctx_df, prediction_length = 1L, quantiles = NULL)
    data.frame(origin = o, actual = y[o], .pred = fc$.pred,
               abserr = abs(y[o] - fc$.pred))
  })
  do.call(rbind, out)
}
