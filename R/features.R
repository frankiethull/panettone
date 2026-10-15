# Temporal feature engine.
#
# Analog of tabpfn-time-series `features/`: generators are factory
# functions returning `function(df)` closures applied to the row-bound
# (context, future) frame, then split again in [build_features()] —
# the same concat -> generate -> split protocol as
# `FeatureTransformer.transform()`.
#
# Differences from the reference (deliberate, per benchmark evidence):
# * No gluonts dependency: calendar components come from base R.
# * `feat_lags()` is an opt-in extension. The reference uses no lags;
#   our day-2 benchmark showed lags help AR-like dynamics but hurt clean
#   seasonality, so strategies stay independently configurable.

# Generators may be called directly on frames without an `item_id` column;
# treat those as single-series, consistent with as_ts_frame()'s dummy id.
# Without this, ave(..., df$item_id) with `df$item_id == NULL` errors in
# split.default ("group length is 0 but data length > 0").
ensure_item_id <- function(df) {
  if (is.null(df$item_id)) df$item_id <- 0
  df
}

#' Running-index feature (analog of `RunningIndexFeature`)
#'
#' Per-series timestep counter. Gives the backend a trend carrier even
#' when timestamps are a plain numeric index.
#'
#' @return A generator closure.
#' @export
feat_running_index <- function() {
  function(df) {
    df <- ensure_item_id(df)
    df <- df[order(df$item_id, df$timestamp), , drop = FALSE]
    df$running_index <- ave(seq_len(nrow(df)), df$item_id, FUN = seq_along) - 1L
    df
  }
}

#' Calendar / seasonal features (analog of `CalendarFeature`)
#'
#' * `POSIXct`/`Date` timestamps: raw components plus sin/cos pairs at
#'   their natural periods (hour/24, weekday/7, month/12, yearday/365.25).
#' * Numeric timestamps: Fourier pairs at `period` (plus a 2nd harmonic),
#'   mirroring the seasonal encoding the reference auto-detects.
#'
#' @param period Dominant seasonal period for numeric timestamps
#'   (e.g. `12` for monthly data with annual seasonality). Required when
#'   timestamps are numeric; ignored for datetime stamps.
#' @return A generator closure.
#' @export
feat_calendar <- function(period = NULL) {
  function(df) {
    df <- ensure_item_id(df)
    ts <- df$timestamp
    if (inherits(ts, "POSIXct") || inherits(ts, "Date")) {
      lt <- as.POSIXlt(ts)
      hour <- lt$hour
      wday <- lt$wday
      mon <- lt$mon
      yday <- lt$yday
      df$cal_hour_sin <- sin(2 * pi * hour / 24)
      df$cal_hour_cos <- cos(2 * pi * hour / 24)
      df$cal_wday_sin <- sin(2 * pi * wday / 7)
      df$cal_wday_cos <- cos(2 * pi * wday / 7)
      df$cal_mon_sin <- sin(2 * pi * mon / 12)
      df$cal_mon_cos <- cos(2 * pi * mon / 12)
      df$cal_yday_sin <- sin(2 * pi * yday / 365.25)
      df$cal_yday_cos <- cos(2 * pi * yday / 365.25)
    } else {
      if (is.null(period)) {
        warning(
          "Numeric timestamps with `period = NULL`: `feat_calendar()` is a no-op. ",
          "Pass e.g. `feat_calendar(period = 12)`.",
          call. = FALSE
        )
        return(df)
      }
      # Per-series position 1..n_k (not the raw timestamp value): robust to
      # gaps/irregular steps while preserving seasonal phase within series.
      pos <- ave(seq_len(nrow(df)), df$item_id, FUN = seq_along)
      for (h in c(1, 2)) {
        df[[paste0("seas", h, "_sin")]] <- sin(2 * pi * h * pos / period)
        df[[paste0("seas", h, "_cos")]] <- cos(2 * pi * h * pos / period)
      }
    }
    df
  }
}

#' Lag features (extension beyond the reference implementation)
#'
#' Adds `lag_1..lag_lookback` of the target within each series. Lags for
#' the 1-step-ahead future row are read from the trailing context, so lag
#' strategies support `prediction_length = 1` (direct) only — deeper
#' horizons need recursive forecasting (Phase 2), because `lag_1` for step
#' 2+ would require unknowable future targets. The reference implementation
#' avoids lags entirely for this reason. Never scale or impute here:
#' backends standardize internally (TabICL/TabPFN) or on train-only
#' stats (Fiorino).
#'
#' @param lookback Number of lagged observations per row.
#' @return A generator closure.
#' @export
feat_lags <- function(lookback) {
  lookback <- as.integer(lookback)
  stopifnot(length(lookback) == 1L, lookback >= 1L)
  function(df) {
    df <- ensure_item_id(df)
    df <- df[order(df$item_id, df$timestamp), , drop = FALSE]
    for (k in seq_len(lookback)) {
      df[[paste0("lag_", k)]] <- ave(
        df$target, df$item_id,
        FUN = function(v) c(rep(NA_real_, k), utils::head(v, -k))
      )
    }
    df
  }
}

#' Reference-style default feature set
#'
#' Analog of `TABPFN_TS_DEFAULT_FEATURES` (running index + calendar /
#' seasonal), without lags.
#'
#' @param period Passed to [feat_calendar()] for numeric timestamps.
#' @return A list of generator closures.
#' @export
default_features <- function(period = NULL) {
  list(feat_running_index(), feat_calendar(period = period))
}

#' Apply feature generators to a (context, future) pair
#'
#' Mirrors `FeatureTransformer.transform()`: row-bind with a train tag,
#' run each generator over the combined frame (so future rows see trailing
#' context for lags), split back, and assert the train/test target
#' contract (no `NA` in train targets, all `NA` in test targets). Feature
#' columns containing `NA` in *future* rows are an error — they would be
#' unknowable at forecast time (e.g. `lookback < horizon`).
#'
#' @param context Canonical context frame (targets present).
#' @param future Canonical future frame (targets all `NA`).
#' @param generators List of generator closures.
#' @return A list with `train` and `test` data frames.
#' @export
build_features <- function(context, future, generators) {
  if (!all(is.na(future$target))) {
    stop("Future frame must have all-NA targets.", call. = FALSE)
  }
  if (any(is.na(context$target))) {
    stop("Context frame must have no NA targets.", call. = FALSE)
  }
  train_plain <- context
  train_plain$.is_train <- TRUE
  test_plain <- future
  test_plain$.is_train <- FALSE
  # Align columns (future may lack covariates dropped by forecast_horizon()).
  all_cols <- union(names(train_plain), names(test_plain))
  for (cn in setdiff(all_cols, names(train_plain))) train_plain[[cn]] <- NA
  for (cn in setdiff(all_cols, names(test_plain))) test_plain[[cn]] <- NA
  combined <- rbind(train_plain, test_plain)

  for (gen in generators) combined <- gen(combined)

  train <- combined[combined$.is_train, , drop = FALSE]
  test <- combined[!combined$.is_train, , drop = FALSE]
  train$.is_train <- NULL
  test$.is_train <- NULL

  if (any(is.na(train$target))) stop("NA targets leaked into training rows.", call. = FALSE)
  if (!all(is.na(test$target))) stop("Non-NA targets in future rows.", call. = FALSE)

  feature_cols <- setdiff(names(combined), c(core_cols(), ".is_train"))
  na_future <- vapply(
    test[, feature_cols, drop = FALSE], function(col) any(is.na(col)), logical(1)
  )
  if (any(na_future)) {
    stop(
      "Unknowable future features (NA in future rows): ",
      paste(names(na_future)[na_future], collapse = ", "),
      ". Lag features support 1-step-ahead direct forecasts only",
      " (recursive multi-step is Phase 2); drop the generator otherwise.",
      call. = FALSE
    )
  }
  list(train = train, test = test)
}
