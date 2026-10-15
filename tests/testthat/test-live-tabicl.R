# Live backend tests — skipped unless PANETTONE_LIVE=1.
# Each engine runs in its own process (R-torch vs Python-torch conflict);
# this file covers whichever engine the current process loaded. Run e.g.:
#   PANETTONE_LIVE=1 Rscript -e 'devtools::test(filter = "live-tabicl")'

test_that("live: tabicl backend fits a lag frame (needs weights)", {
  skip_if_not(Sys.getenv("PANETTONE_LIVE") == "1", "Set PANETTONE_LIVE=1 for live tests.")
  skip_if_not(requireNamespace("brulee", quietly = TRUE), "brulee not installed.")
  skip_if_not(brulee::tab_icl_weights_available(task = "regression"), "TabICL weights missing.")

  set.seed(42)
  t <- seq_len(60)
  y <- sin(2 * pi * t / 12) + rnorm(60, sd = 0.3)
  lag_df <- function(y, k) {
    X <- as.data.frame(sapply(seq_len(k), function(j) y[(k + 1 - j):(length(y) - j)]))
    names(X) <- paste0("lag_", seq_len(k))
    X[, rev(names(X)), drop = FALSE]
  }
  X <- lag_df(y, 8)
  yy <- y[9:60]
  fit <- fit_backend(ts_backend("tabicl", num_estimators = 1L), X[1:40, ], yy[1:40])
  p <- predict_backend(fit, X[41:52, ])
  expect_equal(nrow(p), 12L)
  expect_true(all(is.finite(p$.pred)))
  q <- predict_backend(fit, X[41:43, ], quantiles = c(0.1, 0.9))
  expect_true(all(c(".pred", "q0.1", "q0.9") %in% names(q)))
})
