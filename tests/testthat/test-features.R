# Weight-free tests: feature engine correctness + leakage guards.
# (No brulee/fiorino needed.)

test_that("feat_running_index counts per series from 0", {
  df <- data.frame(
    item_id = c(0, 0, 0, 1, 1),
    timestamp = c(1, 2, 3, 1, 2),
    target = c(1, 2, 3, 4, 5)
  )
  out <- feat_running_index()(df)
  expect_equal(out$running_index, c(0, 1, 2, 0, 1))
})

test_that("feat_calendar gives Fourier pairs for numeric index", {
  df <- data.frame(item_id = 0, timestamp = 1:24, target = rnorm(24))
  out <- feat_calendar(period = 12)(df)
  expect_true(all(c("seas1_sin", "seas1_cos", "seas2_sin", "seas2_cos") %in% names(out)))
  # one full period apart => identical seasonal encoding
  expect_equal(out$seas1_sin[1], out$seas1_sin[13], tolerance = 1e-8)
})

test_that("feat_calendar is a no-op with warning for numeric index without period", {
  df <- data.frame(item_id = 0, timestamp = 1:10, target = rnorm(10))
  expect_warning(out <- feat_calendar()(df), "no-op")
  expect_false("seas1_sin" %in% names(out))
})

test_that("feat_calendar extracts datetime components", {
  ts <- seq(as.POSIXct("2024-01-01", tz = "UTC"), by = "hour", length.out = 30)
  df <- data.frame(item_id = 0, timestamp = ts, target = rnorm(30))
  out <- feat_calendar()(df)
  expect_true(all(grepl("^cal_", names(out)[grepl("^cal_", names(out))])))
  expect_equal(out$cal_hour_sin[1], sin(2 * pi * 0 / 24), tolerance = 1e-8)
})

test_that("feat_lags aligns lag_1 to y[t-1]", {
  y <- 1:10
  df <- data.frame(item_id = 0, timestamp = 1:10, target = y)
  out <- feat_lags(3)(df)
  expect_equal(out$lag_1[4:10], y[3:9])
  expect_equal(out$lag_3[4:10], y[1:7])
  expect_true(all(is.na(out$lag_1[1]), is.na(out$lag_2[1:2]), is.na(out$lag_3[1:3])))
})

test_that("build_features splits with train/test target contract", {
  ctx <- data.frame(item_id = 0, timestamp = 1:20, target = rnorm(20))
  fut <- data.frame(item_id = 0, timestamp = 21:24, target = NA_real_)
  res <- build_features(ctx, fut, default_features(period = 12))
  expect_equal(nrow(res$train), 20)
  expect_equal(nrow(res$test), 4)
  expect_false(any(is.na(res$train$target)))
  expect_true(all(is.na(res$test$target)))
})

test_that("build_features errors on unknowable future lags", {
  ctx <- data.frame(item_id = 0, timestamp = 1:20, target = rnorm(20))
  fut <- data.frame(item_id = 0, timestamp = 21:24, target = NA_real_)
  # lookback 2 < horizon 4: future rows 3-4 need unknowable targets
  expect_error(
    build_features(ctx, fut, list(feat_running_index(), feat_lags(2))),
    "Unknowable future features"
  )
})

test_that("build_features lags read the 1-step future row from trailing context", {
  ctx <- data.frame(item_id = 0, timestamp = 1:20, target = 1:20)
  fut <- data.frame(item_id = 0, timestamp = 21, target = NA_real_)
  res <- build_features(ctx, fut, list(feat_lags(4)))
  expect_equal(res$test$lag_1, 20)
  expect_equal(res$test$lag_4, 17)
})

test_that("lag strategies reject multi-step horizons (direct only)", {
  ctx <- data.frame(item_id = 0, timestamp = 1:20, target = 1:20)
  fut <- data.frame(item_id = 0, timestamp = 21:22, target = NA_real_)
  # step-2 lag_1 would need the unknowable y[21]
  expect_error(
    build_features(ctx, fut, list(feat_lags(4))),
    "1-step-ahead"
  )
})
