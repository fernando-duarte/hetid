fv_test_boundary <- function() {
  readRDS(test_path("fixtures", "fitted-volatility-boundary-oracle.rds"))
}

read_fitted_volatility_endpoint_oracle <- function() {
  paths <- test_path("fixtures", paste0("fitted-volatility-endpoint-oracle.part-", 1:2, ".rds"))
  expected <- c(
    "f888b98e67e2e4389a5ed2bf0252b894eeaf0c151b71aec2a840be59aa38461d",
    "d6da4ab1e5614a04c0282a77d4ad7e433c9c3b5966b83e1697ee3e6fd68622ee"
  )
  stopifnot(identical(unname(tools::sha256sum(paths)), expected))
  bytes <- do.call(c, lapply(paths, function(path) readBin(path, "raw", n = file.info(path)$size)))
  con <- gzcon(rawConnection(bytes, "rb"))
  on.exit(close(con))
  readRDS(con)
}

fv_test_control <- function() {
  control <- log_variance_search_control()
  control$search$grid_n <- 7L
  control$search$grid_floor <- 3L
  control$search$primary_starts_per_side <- 1L
  control$search$audit_starts_per_side <- 1L
  control$search$primary_grid_cap <- 30L
  control$search$coverage_grid_cap <- 30L
  control$search$primary_fit_budget <- 1000L
  control$search$coverage_fit_budget <- 1000L
  control$search$sensitivity_fit_budget <- 1000L
  control
}

fv_test_source <- function(fail = FALSE, cold = FALSE, jacobian = NULL) {
  labels <- c(LOG_VARIANCE_INTERCEPT_LABEL, "pc1", "pc2")
  loading <- matrix(c(0, 1, 2), 3L, 1L, dimnames = list(labels, "news"))
  calls <- new.env(parent = emptyenv())
  calls$fits <- list()
  calls$jacobians <- list()
  map <- list(
    metadata = list(
      estimator = "ppml", target_functional = "theta_var", sample_id = "sample-a",
      spec_id = "fixture-v1", smoothness = "smooth", response_scale = "variance"
    ),
    coef_labels = labels, theta_labels = "news",
    fit_at_b = function(b, start = NULL, phase = NULL) {
      calls$fits[[length(calls$fits) + 1L]] <- list(b = b, start = start, phase = phase)
      if (fail || b < 0) {
        return(lv_set_fit_result(rep(NA_real_, 3L), "domain_failure", FALSE))
      }
      value <- c(100, b, 2 * b)
      if (cold && identical(phase, "cold_start")) value <- value + 0.1
      lv_set_fit_result(stats::setNames(value, labels), "ok", TRUE,
        objective = 2, score_norm = 0, convergence_code = 0L,
        warm_start = c(100, b, 2 * b), diagnostics = list(retained = TRUE)
      )
    },
    jacobian_at_b = function(b, fit = NULL) {
      calls$jacobians[[length(calls$jacobians) + 1L]] <- fit
      if (is.null(jacobian)) loading else jacobian()
    },
    precheck = function(quadratic, theta_table) list(unresolved = "source-precheck")
  )
  list(map = map, calls = calls)
}

fv_test_sets <- function() {
  starts <- seq(as.Date("2020-01-01"), by = "month", length.out = 13L)
  dates <- starts[-1L] - 1L
  sample <- prepare_log_variance_search(
    rep(c(-2, -1, 1, 2), 3L), cbind(news = rep(c(-1, 1), 6L)),
    cbind(pc1 = sin(seq_along(dates)), pc2 = cos(seq_along(dates))), dates, dates
  )
  source <- fv_test_source()
  source$map$metadata$sample_id <- sample$sample_id
  source$map$precheck <- NULL
  result <- list(schema = data.frame(), diagnostics = list(n_raw_feasible = 3L))
  sets <- list(
    key = "ppml", estimator = source$map, sample = sample,
    seed = c(news = 0), grid_cap = 7L, cache = new.env(parent = emptyenv()),
    results = stats::setNames(list(result), profile_tau_key(0.05)), taus = 0.05,
    request = list(point = c(news = 0))
  )
  list(sets = sets, calls = source$calls)
}

fv_test_system <- function(radius = 0.1, labels = "news") {
  d <- length(labels)
  list(
    quadratic = list(A_i = list(diag(d)), b_i = list(rep(0, d)), c_i = -radius^2),
    table = data.frame(
      coef = labels, status = "bounded",
      outer_lower = -radius, outer_upper = radius
    )
  )
}

fv_test_rehash <- function(sets) {
  sets$sample$sample_id <- NULL
  sets$sample$sample_id <- lv_set_hash(sets$sample)
  sets$estimator$metadata$sample_id <- sets$sample$sample_id
  sets
}

fv_test_replay <- function(history) {
  dates <- history$sample$response_qtr
  input <- history$input
  sample <- prepare_log_variance_search(
    input$sample$w1, input$sample$w2, input$raw, dates, dates,
    ols_residuals = input$sample$ols_residuals, response_ids = dates
  )
  ppml <- profile_log_variance_map(sample, history$quadratics, history$theta_tables,
    history$taus, "ppml",
    point = c(0, 0), control = history$control
  )
  sets <- if (history$method == "ppml") {
    ppml
  } else {
    profile_log_variance_map(
      sample, history$quadratics, history$theta_tables, history$taus, "harvey",
      point = c(0, 0), ppml = ppml, control = history$control
    )
  }
  path <- profile_log_variance_path(sets, history$quadratics, history$theta_tables,
    history$taus,
    control = history$control
  )
  envelopes <- lv_set_fixed_rng(stats::setNames(lapply(history$taus, function(tau) {
    key <- profile_tau_key(tau)
    profile_fitted_volatility(sets, history$quadratics[[key]], history$theta_tables[[key]],
      tau,
      control = history$control
    )
  }), profile_tau_key(history$taus)))
  list(sets = sets, path = path, envelopes = envelopes)
}

fv_test_path_setup <- function() {
  t <- seq_len(40L)
  x <- matrix(sin(t), ncol = 1L, dimnames = list(NULL, "x"))
  z <- seq(-1, 1, length.out = length(t))
  y2 <- cbind(news = x[, 1] + (1 + z) * cos(2 * t))
  fit <- compute_tau0_system(0.5 + 0.2 * x[, 1] + 0.7 * y2[, 1] + sin(3 * t), y2, x, z)
  starts <- seq(as.Date("2018-01-01"), by = "month", length.out = 41L)
  dates <- starts[-1L] - 1L
  setup <- fv_test_sets()
  setup$sets$sample <- prepare_log_variance_search(
    fit$w1, fit$w2,
    cbind(pc1 = sin(t[1:12]), pc2 = cos(t[1:12])), dates, dates[1:12]
  )
  setup$sets$estimator$metadata$sample_id <- setup$sets$sample$sample_id
  setup$sets$request$point <- setup$sets$seed <- fit$point$theta
  setup$fit <- fit
  setup
}
