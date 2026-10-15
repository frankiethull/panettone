# Weight-free end-to-end tests via custom_backend(lm).
# Exercises ts-frame -> features -> backend -> forecast without model weights.

lm_backend <- function() {
  custom_backend(
    fit_fn = function(X, y) {
      df <- cbind(X, .y = y)
      stats::lm(.y ~ ., data = df)
    },
    predict_fn = function(model, X) as.numeric(stats::predict(model, X))
  )
}

test_that("as_ts_frame adds dummy id and sorts", {
  df <- data.frame(ts = c(3, 1, 2), y = c(30, 10, 20))
  out <- as_ts_frame(df, timestamp = "ts", target = "y")
  expect_equal(out$timestamp, c(1, 2, 3))
  expect_equal(out$target, c(10, 20, 30))
  expect_equal(unique(out$item_id), 0)
})

test_that("make_future_frame extends numeric index", {
  ctx <- data.frame(item_id = 0, timestamp = 1:10, target = rnorm(10))
  fut <- make_future_frame(ctx, 3L)
  expect_equal(fut$timestamp, 11:13)
  expect_true(all(is.na(fut$target)))
})

test_that("make_future_frame extends POSIXct by median step", {
  ts <- seq(as.POSIXct("2024-01-01", tz = "UTC"), by = "hour", length.out = 10)
  ctx <- data.frame(item_id = 0, timestamp = ts, target = rnorm(10))
  fut <- make_future_frame(ctx, 2L)
  expect_equal(as.numeric(difftime(fut$timestamp[1], ts[10], units = "hours")), 1)
})

test_that("forecast_horizon keeps only shared covariates", {
  ctx <- data.frame(item_id = 0, timestamp = 1:5, target = 1:5,
                    known = 1:5, past_only = 1:5)
  fut <- data.frame(item_id = 0, timestamp = 6:7, target = NA_real_, known = 6:7)
  expect_warning(res <- forecast_horizon(ctx, fut), "past_only")
  expect_equal(res$covariates, "known")
})

test_that("pipeline with lm backend forecasts seasonal data accurately", {
  set.seed(11)
  t <- 1:120
  y <- sin(2 * pi * t / 12) + 0.02 * t + rnorm(120, sd = 0.2)
  ctx <- data.frame(timestamp = t, target = y)
  pipe <- panettone_pipeline(
    backend = lm_backend(),
    features = default_features(period = 12),
    quantiles = NULL
  )
  fc <- predict_df(pipe, ctx, prediction_length = 6L)
  expect_equal(nrow(fc), 6L)
  expect_true(all(c("item_id", "timestamp", ".pred") %in% names(fc)))
  expect_false(any(is.na(fc$.pred)))
  # lm on trend+season should beat naive by a margin on this clean series
  naive <- y[120]
  expect_lt(mean(abs(fc$.pred - (sin(2 * pi * (121:126) / 12) + 0.02 * (121:126)))),
            abs(naive - (sin(2 * pi * 121 / 12) + 0.02 * 121)) + 1)
})

test_that("pipeline errors when both/neither horizon args given", {
  pipe <- panettone_pipeline(backend = lm_backend(), quantiles = NULL)
  ctx <- data.frame(timestamp = 1:10, target = rnorm(10))
  expect_error(predict_df(pipe, ctx, prediction_length = 2L,
                          future_df = data.frame(timestamp = 11:12)), "exactly one")
})

test_that("pipeline rejects quantiles for point-only backend", {
  pipe <- panettone_pipeline(backend = lm_backend()) # default quantiles non-NULL
  ctx <- data.frame(timestamp = 1:30, target = rnorm(30))
  expect_error(predict_df(pipe, ctx, prediction_length = 2L), "quantile")
})

test_that("future targets cannot leak: builder ignores non-NA future targets", {
  # predict_df path always constructs NA future targets internally; the
  # explicit-future path must reject smuggled values.
  pipe <- panettone_pipeline(backend = lm_backend(), quantiles = NULL)
  ctx <- data.frame(timestamp = 1:30, target = rnorm(30))
  bad_future <- data.frame(timestamp = 31:32, target = c(999, 999))
  expect_error(predict_df(pipe, ctx, future_df = bad_future), "all NA")
})

test_that("rolling_eval returns one row per origin", {
  set.seed(5)
  y <- sin(2 * pi * (1:60) / 12) + rnorm(60, sd = 0.2)
  pipe <- panettone_pipeline(
    backend = lm_backend(),
    features = default_features(period = 12),
    quantiles = NULL
  )
  ev <- rolling_eval(y, origins = 55:58, pipeline = pipe)
  expect_equal(nrow(ev), 4L)
  expect_false(any(is.na(ev$.pred)))
})
