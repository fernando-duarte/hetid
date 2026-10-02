# Covariance delegation preserves the paper's controls, axes and dated SE columns.
source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "inference", "standard_error_estimators.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "standard_errors.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "standard_errors.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "standard_errors.R"
))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

x <- cbind("(Intercept)" = 1, v = c(-2, -1, 0, 1, 2, 3))
y <- c(0, 2, 1, 3, 1, 4)
coef <- c(0.2, -0.1)
for (est in c("ppml", "harvey")) {
  vcov_fn <- get(paste0("logvar_", est, "_vcov"))
  check(paste(est, "delegates every matrix and its labels"), {
    identical(
      vcov_fn(coef, y, x, 2L),
      hetid::compute_log_variance_vcov_at_coef(coef, y, x, est, 2L, 1e-10)
    )
  })
  check(paste(est, "honors the paper conditioning control"), {
    tight <- vcov_fn(coef, y, x, 2L, rcond_tol = 1)
    all(vapply(tight, function(m) all(is.na(m)), logical(1)))
  })
  bad <- function(expr) inherits(tryCatch(expr, error = identity), "hetid_error")
  check(paste(est, "rejects fractional lags"), {
    bad(vcov_fn(coef, y, x, 0.5))
  })
  check(paste(est, "rejects permuted coefficient names"), {
    bad(vcov_fn(stats::setNames(coef, rev(colnames(x))), y, x, 2L))
  })
}

# Deliberately drop and reorder source rows; the paper reconstructs by qtr keys.
mean_eq <- list(
  qtr = rev(as.Date(c(
    "2001-03-31", "2001-06-30", "2001-09-30", "2001-12-31",
    "2002-03-31", "2002-06-30", "2002-09-30", "2002-12-31"
  ))),
  ols_fit = stats::lm(c(1, 3, 2, 5, 3, 8, 4, 6) ~ seq_len(8)),
  theta_table = data.frame(point = 0.25)
)
rows <- c(8, 6, 4, 2)
inputs <- list(
  qtr = mean_eq$qtr[rows], pcr = matrix(c(-1, 0, 1, 2),
    ncol = 1,
    dimnames = list(NULL, "v")
  ), w1 = c(2, 1, 4, 3), w2 = matrix(c(1, 2, 1, 2), ncol = 1)
)
x_cols <- logvar_design_matrix(inputs$pcr)
tab <- data.frame(coef = colnames(x_cols), reference = coef, point = coef)
for (est in c("ppml", "harvey")) {
  vcov_fn <- get(paste0("logvar_", est, "_vcov"))
  columns_fn <- get(paste0("logvar_", est, "_se_columns"))
  result <- list(
    table = tab,
    estimator = list(metadata = list(fit_control = list(rcond_tol = 1e-10)))
  )
  got <- columns_fn(result, inputs, mean_eq, 2L)
  expected_ref <- logvar_se_frame(vcov_fn(
    coef,
    stats::residuals(mean_eq$ols_fit)[rows]^2, x_cols, 2L
  ), tab$coef)
  expected_point <- logvar_se_frame(vcov_fn(
    coef,
    drop(inputs$w1 - inputs$w2 %*% 0.25)^2, x_cols, 2L
  ), tab$coef)
  check(paste(est, "preserves dated reference and point response reconstruction"), {
    identical(got, list(reference = expected_ref, point = expected_point, hac_lags = 2L))
  })
  result$estimator$metadata$fit_control$rcond_tol <- 1
  strict <- columns_fn(result, inputs, mean_eq, 2L)
  check(paste(est, "uses frozen metadata control for both columns"), {
    all(is.na(as.matrix(strict$reference[-1]))) && all(is.na(as.matrix(strict$point[-1])))
  })
  result$estimator$metadata$fit_control$rcond_tol <- 1e-10
  bad_controls <- list(missing = NULL, text = "bad", negative = -1, missing_value = NA_real_)
  for (label in names(bad_controls)) {
    broken <- result
    broken$estimator$metadata$fit_control$rcond_tol <- bad_controls[[label]]
    check(paste(est, "rejects", label, "frozen conditioning control"), {
      inherits(tryCatch(columns_fn(broken, inputs, mean_eq, 2L), error = identity), "error")
    })
  }
  missing <- mean_eq
  missing$theta_table$point <- NA_real_
  unavailable <- columns_fn(result, inputs, missing, 2L)
  check(paste(est, "keeps unavailable points distinct from reference estimates"), {
    identical(unavailable$reference, got$reference) &&
      identical(unavailable$point$coef, tab$coef) &&
      all(is.na(as.matrix(unavailable$point[-1])))
  })
}
# The log projections delegate both columns to hetid at the reference and point
# news vectors, keyed by the package's variant names
set.seed(5)
lp_n <- 40L
lp_z <- cbind(1, stats::rnorm(lp_n))
lp_news <- cbind(n1 = stats::rnorm(lp_n), n2 = stats::rnorm(lp_n))
lp_w1 <- drop(stats::lm.fit(lp_z, drop(lp_news %*% c(0.4, -0.2)) +
  stats::rnorm(lp_n))$residuals)
lp_w2 <- stats::lm.fit(lp_z, lp_news)$residuals
colnames(lp_w2) <- colnames(lp_news)
lp_prep <- hetid::prepare_log_projection(
  lp_w1, lp_w2, cbind(l.pc1 = stats::rnorm(lp_n)), seq_len(lp_n), seq_len(lp_n)
)
lp_labels <- rownames(lp_prep$projection)
lp_ctx <- list(b_ref = c(0.3, -0.1), b_point = c(0.4, -0.2), point_feasible = TRUE)
for (method in c("log_plus", "log_fuller")) {
  lp_mapped <- list(
    estimator = list(
      coef_labels = lp_labels,
      metadata = list(fit_control = list(method = method))
    ),
    multiplier = 1, point = stats::setNames(c(0.1, 0.2), lp_labels)
  )
  lp_frame <- function(b) {
    logvar_se_frame(
      hetid::compute_log_projection_vcov(lp_prep, b, method, 1, 2L), lp_labels
    )
  }
  lp_got <- logvar_log_projection_se_columns(lp_mapped, lp_prep, lp_ctx, 2L)
  check(paste(method, "delegates reference and point frames to hetid"), {
    identical(lp_got$reference, lp_frame(lp_ctx$b_ref)) &&
      identical(lp_got$point, lp_frame(lp_ctx$b_point)) &&
      identical(lp_got$reference$coef, lp_labels) &&
      identical(
        names(hetid::compute_log_projection_vcov(lp_prep, lp_ctx$b_ref, method)),
        LOGVAR_LOG_PROJECTION_SE_TYPES
      )
  })
  lp_off <- logvar_log_projection_se_columns(
    lp_mapped, lp_prep, utils::modifyList(lp_ctx, list(point_feasible = FALSE)), 2L
  )
  check(paste(method, "reports an infeasible point as an all-NA frame"), {
    identical(lp_off$reference, lp_got$reference) &&
      identical(lp_off$point$coef, lp_labels) &&
      all(is.na(as.matrix(lp_off$point[-1])))
  })
}
.test$finish()
