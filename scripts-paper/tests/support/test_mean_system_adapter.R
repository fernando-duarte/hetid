# Independent characterization of the paper mean-system adapter. Run from root:
# Rscript scripts-paper/tests/support/test_mean_system_adapter.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "quadratic_evaluation.R"))
paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "identified_set_bootstrap.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# Preserve the original joint-lm and stacked-QR recipe independently of the
# adapter and the package's public fit/point solvers.
mean_system_reference <- function(dat, spec, tol) {
  fit1 <- stats::lm(stats::reformulate(spec$x_cols, spec$y1_col), data = dat)
  beta1r <- stats::coef(fit1)
  w1 <- stats::residuals(fit1)
  if (spec$impose_null) {
    w2 <- as.matrix(dat[spec$y2_cols])
    beta2r <- matrix(
      0, length(spec$y2_cols), length(beta1r),
      dimnames = list(spec$y2_cols, names(beta1r))
    )
  } else {
    fit2 <- stats::lm(as.matrix(dat[spec$y2_cols]) ~ ., data = dat[spec$x_cols])
    w2 <- stats::residuals(fit2)
    beta2r <- t(stats::coef(fit2))
  }
  z <- dat[[spec$z_col]] - mean(dat[[spec$z_col]])
  moments <- hetid::compute_identification_moments(
    w1, w2, matrix(z, ncol = 1, dimnames = list(NULL, spec$z_col))
  )
  built <- build_pipeline_quadratic_system(spec$gamma, rep(0, ncol(w2)), moments)
  qmat <- do.call(rbind, built$components$Q_i)
  lvec <- built$components$L_i
  point <- NULL
  if (nrow(qmat) >= ncol(qmat) && qr(qmat, tol = tol)$rank == ncol(qmat)) {
    theta <- qr.solve(qmat, lvec, tol = tol)
    if (all(is.finite(theta)) &&
      max(abs(qmat %*% theta - lvec)) <= tol * max(1, max(abs(lvec)))) {
      point <- list(theta = as.numeric(theta), cond = kappa(qmat))
    }
  }
  list(
    beta1r = beta1r, w1 = w1, beta2r = beta2r, w2 = w2, z = z,
    moments = moments, point0 = point, tau0_quadratic = built$quadratic
  )
}

mean_system_equal <- function(reference, candidate) {
  if (!identical(attributes(reference), attributes(candidate)) ||
    !identical(typeof(reference), typeof(candidate)) ||
    length(reference) != length(candidate)) {
    return(FALSE)
  }
  if (is.list(reference)) {
    return(all(vapply(seq_along(reference), function(i) {
      mean_system_equal(reference[[i]], candidate[[i]])
    }, logical(1))))
  }
  if (!is.numeric(reference)) {
    return(identical(reference, candidate))
  }
  finite <- is.finite(reference)
  identical(finite, is.finite(candidate)) &&
    identical(reference[!finite], candidate[!finite]) &&
    isTRUE(all.equal(reference[finite], candidate[finite], tolerance = 1e-12))
}

set.seed(20260924L)
adapter_n <- 150L
adapter_z <- rnorm(adapter_n)
adapter_x <- rnorm(adapter_n)
adapter_x2 <- rnorm(adapter_n)
adapter_news <- exp(0.4 * adapter_z) * matrix(rnorm(adapter_n * 2L), adapter_n)
adapter_data <- data.frame(
  date = as.Date("1980-01-01") + seq_len(adapter_n),
  y1 = 0.3 + adapter_x + drop(adapter_news %*% c(0.4, -0.2)) + rnorm(adapter_n),
  x = adapter_x, x2 = adapter_x2,
  news_a = adapter_news[, 1L] + 0.3 * adapter_x,
  news_b = adapter_news[, 2L] - 0.2 * adapter_x2, z = adapter_z
)
adapter_spec <- list(
  y1_col = "y1", x_cols = c("x", "x2"),
  y2_cols = c("news_a", "news_b"), z_col = "z",
  gamma = matrix(1, 1L, 2L), impose_null = TRUE
)
adapter_custom <- adapter_data
rownames(adapter_custom) <- paste0("observation_", seq_len(adapter_n))
adapter_rows <- rep(seq_len(adapter_n / 2L), each = 2L)
adapter_frames <- list(
  default = adapter_data, custom = adapter_custom,
  repeated = adapter_custom[adapter_rows, ], tibble = tibble::as_tibble(adapter_data)
)
adapter_tol <- PAPER_QUADRATIC_CONTROL$point_identification_tolerance
for (null in c(TRUE, FALSE)) {
  for (frame_name in names(adapter_frames)) {
    for (x_cols in list("x", c("x", "x2"))) {
      for (scale in c(1e-6, 1, 1e6)) {
        dat <- adapter_frames[[frame_name]]
        spec <- adapter_spec
        spec$impose_null <- null
        spec$x_cols <- x_cols
        spec$y2_cols <- rev(spec$y2_cols)
        spec$gamma <- matrix(c(0.7, 1.3), 1L, dimnames = list("z", spec$y2_cols))
        dat[c(spec$y1_col, spec$y2_cols)] <- dat[c(spec$y1_col, spec$y2_cols)] * scale
        check(
          sprintf(
            "recipe parity: null=%s, %s, p=%d, scale=%g",
            null, frame_name, length(x_cols), scale
          ),
          mean_system_equal(
            mean_system_reference(dat, spec, adapter_tol), estimate_set_id_system(dat, spec)
          )
        )
      }
    }
  }
}

paper_source_once(paper_path("tests", "support", "mean_system_adapter_checks.R"))
.test$finish()
