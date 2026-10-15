# Backend adapter contract.
#
# Analog of tabpfn-time-series `TimeSeriesPredictor.from_tabpfn_family()` /
# `from_point_prediction_regressor()`: the feature engine never talks to a
# model directly. Each backend translates a prepared (X_train, y_train)
# pair into its native fit/predict interface and returns point forecasts
# plus *optional* quantiles.
#
# R-torch backends (tabicl, tabpfn via brulee) and the Python-torch backend
# (fiorino via reticulate) share one libtorch in the session, so the two
# builds must match. R torch bundles libtorch 2.8.0: pin the reticulate
# environment with
#   pip install "torch==2.8.0" --index-url https://download.pytorch.org/whl/cpu
# and all three engines run in one process (verified both load orders).
# With mismatched versions (e.g. default torch 2.14 alongside R's bundle)
# the second runtime fails with undefined libtorch symbols.

#' Construct a backend adapter
#'
#' @param engine One of `"tabicl"`, `"tabpfn"`, `"fiorino"`.
#' @param ... Engine config passed through to the native constructor:
#'   * `tabicl`: `num_estimators`, `normalization`, ... (see
#'     [brulee::brulee_tab_icl()]).
#'   * `tabpfn`: `version`, `num_estimators`, ... (see
#'     [brulee::brulee_tab_pfn()]).
#'   * `fiorino`: `device`, `seed`, `max_rows` (see
#'     `fiorino::fiorino_regressor()`).
#' @return A backend object (class `panettone_backend_<engine>`).
#' @export
ts_backend <- function(engine = c("tabicl", "tabpfn", "fiorino"), ...) {
  engine <- match.arg(engine)
  structure(
    list(engine = engine, config = list(...)),
    class = c(paste0("panettone_backend_", engine), "panettone_backend")
  )
}

#' Construct a custom backend from plain functions
#'
#' First-class generic hook (analog of `from_point_prediction_regressor`):
#' any tabular regressor works without touching package code. This is also
#' how the test suite exercises the full pipeline without model weights.
#'
#' @param fit_fn `function(X, y)` returning an opaque model object.
#' @param predict_fn `function(model, X)` returning a numeric vector of
#'   point forecasts (or, when `supports_quantiles` is `TRUE`, a data frame
#'   with `.pred` plus one column per requested level).
#' @param supports_quantiles Logical: does `predict_fn` honour quantiles?
#' @return A backend object.
#' @export
custom_backend <- function(fit_fn, predict_fn, supports_quantiles = FALSE) {
  stopifnot(is.function(fit_fn), is.function(predict_fn))
  structure(
    list(
      engine = "custom", fit_fn = fit_fn, predict_fn = predict_fn,
      supports_quantiles = isTRUE(supports_quantiles)
    ),
    class = c("panettone_backend_custom", "panettone_backend")
  )
}

#' Does this backend natively return quantiles?
#'
#' TabICL/TabPFN expose quantile heads; Fiorino and plain regressors are
#' point-only (intervals then need a residual/conformal layer, Phase 2).
#' @param backend A backend object.
#' @return Logical.
#' @export
supports_quantiles <- function(backend) {
  UseMethod("supports_quantiles")
}

#' @export
supports_quantiles.panettone_backend <- function(backend) {
  isTRUE(backend$engine %in% c("tabicl", "tabpfn"))
}

#' @export
supports_quantiles.panettone_backend_custom <- function(backend) {
  isTRUE(backend$supports_quantiles)
}

#' Fit a backend on a prepared feature table
#'
#' @param backend A backend object.
#' @param X A data frame of features.
#' @param y A numeric target vector.
#' @return A fitted object of class `panettone_fit`.
#' @export
fit_backend <- function(backend, X, y) {
  UseMethod("fit_backend")
}

panettone_fit <- function(backend, model, extra = list()) {
  structure(
    c(list(backend = backend, model = model), extra),
    class = "panettone_fit"
  )
}

#' @export
fit_backend.panettone_backend_tabicl <- function(backend, X, y) {
  if (!requireNamespace("brulee", quietly = TRUE)) {
    stop("Backend 'tabicl' needs the suggested package 'brulee'.", call. = FALSE)
  }
  args <- backend$config
  if (is.null(args$num_estimators)) args$num_estimators <- 1L
  model <- do.call(brulee::brulee_tab_icl, c(list(x = X, y = y), args))
  panettone_fit(backend, model)
}

#' @export
fit_backend.panettone_backend_tabpfn <- function(backend, X, y) {
  if (!requireNamespace("brulee", quietly = TRUE)) {
    stop("Backend 'tabpfn' needs the suggested package 'brulee'.", call. = FALSE)
  }
  args <- backend$config
  if (is.null(args$version)) args$version <- "v3.5"
  if (is.null(args$num_estimators)) args$num_estimators <- 4L
  model <- do.call(brulee::brulee_tab_pfn, c(list(x = X, y = y), args))
  panettone_fit(backend, model)
}

#' @export
fit_backend.panettone_backend_fiorino <- function(backend, X, y) {
  if (!requireNamespace("fiorino", quietly = TRUE)) {
    stop("Backend 'fiorino' needs the suggested package 'fiorino'.", call. = FALSE)
  }
  if (!is.numeric(y) || length(y) == 0L) {
    stop("Fiorino backend needs a non-empty numeric `y`.", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("Fiorino backend needs finite targets (no NA/NaN/Inf).", call. = FALSE)
  }
  # Bucket head models log1p(y): targets must be non-negative. Shift is fit
  # on train only and inverted in predict_backend() via `pred - offset`
  # (fit sees `y + offset`, so subtracting restores the original scale).
  # Leakage-free: offset derives from the training targets of this fit only.
  ymin <- min(y)
  offset <- if (ymin < 0.5) 0.5 - ymin else 0
  args <- backend$config
  if (is.null(args$device)) args$device <- "cpu"
  # Each fit owns an independent estimator cloned from a cached prototype.
  # fio_fit() mutates the Python object in place, so sharing one object
  # across fits would silently overwrite earlier fits (wrong results when
  # holding >1 fit; races under parallelism). The prototype itself is
  # never fitted.
  reg <- fiorino_estimator(args)
  reg <- fiorino::fio_fit(reg, X, y + offset)
  panettone_fit(backend, reg, extra = list(offset = offset))
}

#' @export
fit_backend.panettone_backend_custom <- function(backend, X, y) {
  panettone_fit(backend, backend$fit_fn(X, y))
}

#' Predict with a fitted backend
#'
#' @param fit A fitted object from [fit_backend()].
#' @param X A data frame of future features (same columns as fit time).
#' @param quantiles Numeric vector of quantile levels in (0, 1), or `NULL`
#'   for point forecasts only. Backends without native quantile support
#'   error on non-`NULL` quantiles — request intervals only where
#'   [supports_quantiles()] is `TRUE`.
#' @return A data frame with `.pred` plus one `q<level>` column per
#'   requested quantile.
#' @export
predict_backend <- function(fit, X, quantiles = NULL) {
  UseMethod("predict_backend")
}

#' @export
predict_backend.panettone_fit <- function(fit, X, quantiles = NULL) {
  backend <- fit$backend
  engine <- backend$engine
  if (!is.null(quantiles) && !supports_quantiles(backend)) {
    stop(
      sprintf("Backend '%s' has no native quantile support.", engine),
      call. = FALSE
    )
  }
  if (engine == "tabicl") {
    out <- data.frame(.pred = as.numeric(stats::predict(fit$model, X)$.pred))
    if (!is.null(quantiles)) {
      qmat <- brulee_quantiles(fit$model, X, quantiles)
      for (j in seq_along(quantiles)) out[[paste0("q", quantiles[j])]] <- qmat[, j]
    }
    return(out)
  }
  if (engine == "tabpfn") {
    out <- data.frame(.pred = as.numeric(stats::predict(fit$model, X)$.pred))
    if (!is.null(quantiles)) {
      qmat <- brulee_quantiles(fit$model, X, quantiles)
      for (j in seq_along(quantiles)) out[[paste0("q", quantiles[j])]] <- qmat[, j]
    }
    return(out)
  }
  if (engine == "fiorino") {
    offset <- fit$offset
    if (is.null(offset)) offset <- 0
    pred <- as.numeric(fiorino::fio_predict(fit$model, X)) - offset
    return(data.frame(.pred = pred))
  }
  # custom
  pred <- backend$predict_fn(fit$model, X)
  if (isTRUE(backend$supports_quantiles)) {
    if (!is.null(quantiles)) {
      if (!is.data.frame(pred) || !all(c(".pred", paste0("q", quantiles)) %in% names(pred))) {
        stop("Custom quantile backend must return `.pred` + `q<level>` columns.", call. = FALSE)
      }
      return(pred)
    }
    return(data.frame(.pred = pred$.pred))
  }
  data.frame(.pred = as.numeric(pred))
}

# Session cache of *unfitted* Fiorino prototypes, keyed by config.
# Each fit clones the prototype (never fits it in place) so fitted objects
# are independent. See fit_backend.panettone_backend_fiorino().
.fiorino_estimators <- new.env(parent = emptyenv())

fiorino_prototype <- function(args) {
  key <- fiorino_config_key(args)
  if (!exists(key, envir = .fiorino_estimators)) {
    assign(
      key, do.call(fiorino::fiorino_regressor, args),
      envir = .fiorino_estimators
    )
  }
  get(key, envir = .fiorino_estimators)
}

fiorino_config_key <- function(args) {
  if (length(args) == 0L) return("<default>")
  nm <- sort(names(args))
  paste(
    vapply(nm, function(n) paste0(n, "=", paste(args[[n]], collapse = ",")),
           character(1)),
    collapse = ";"
  )
}

# Fresh independent estimator per fit: clone the cached prototype's Python
# object (fast: no checkpoint reload) so fio_fit()'s in-place mutation
# cannot leak across fits. Falls back to a fresh constructor (slow but
# correct) if cloning is unavailable.
fiorino_estimator <- function(args) {
  proto <- fiorino_prototype(args)
  py_clone <- tryCatch(
    reticulate::import("sklearn.base")$clone(proto$py),
    error = function(e) tryCatch(
      reticulate::import("copy")$deepcopy(proto$py),
      error = function(e2) NULL
    )
  )
  if (!is.null(py_clone)) {
    out <- proto
    out$py <- py_clone
    return(out)
  }
  do.call(fiorino::fiorino_regressor, args)
}

# Extract the packed hardhat::quantile_pred column from one brulee
# predict(type = "quantile") call into a plain matrix (rows = new obs).
brulee_quantiles <- function(model, X, quantiles) {
  res <- stats::predict(model, X, type = "quantile", quantile_levels = quantiles)
  mat <- vctrs_field(res$.pred_quantile, "quantile_values")
  mat <- as.matrix(mat)
  if (!all(dim(mat) == c(nrow(X), length(quantiles)))) {
    stop("Unexpected quantile matrix shape from brulee backend.", call. = FALSE)
  }
  mat
}

# `vctrs::field()` without a hard dependency (vctrs ships with hardhat/brulee;
# fall back informatively if absent).
vctrs_field <- function(x, name) {
  if (requireNamespace("vctrs", quietly = TRUE)) return(vctrs::field(x, name))
  attr(x, name)
}
