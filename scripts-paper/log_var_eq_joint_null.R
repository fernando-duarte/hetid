# Driver for the joint-null theta_R = 0 distance diagnostic: the guarded
# pipeline orchestration over the mean equation's warm-refined display-tau news
# sets. Sourced by run_all.R after log_var_eq_harvey_sets.R and before the
# panels table; the offline suite sources it too. The print and serialization
# helpers live in log_var_eq_joint_null_report.R, sourced here so they are
# defined whether or not the pipeline objects exist. The frozen module
# interfaces logvar_joint_null_projection/_scales/_at_tau are consumed, never
# reimplemented. Run via run_all.R.

source("scripts-paper/log_var_eq_joint_null_report.R")

# Guarded pipeline orchestration: it runs only when the upstream benchmark
# objects are present, so sourcing this file in the offline suite defines the
# report helpers without touching the pipeline. It consumes log_var_eq$inputs
# (including the qtr-aligned eps_ref seam), the warm-refined display boxes
# mean_eq_bounds_tau, and set_id_mean_eq for the quadratic systems, and owns the
# single projection, the joins, the prints, and the writes.
if (exists("log_var_eq") && exists("set_id_mean_eq") &&
  exists("mean_eq_bounds_tau")) {
  jn_inputs <- log_var_eq$inputs
  stopifnot(
    !is.null(jn_inputs$eps_ref),
    length(jn_inputs$eps_ref) == length(jn_inputs$w1),
    identical(log_var_eq$sample_id, log_var_eq$sample_contract$sample_id)
  )
  jn_proj <- logvar_joint_null_projection(
    jn_inputs$pcr, jn_inputs$qtr, log_var_eq$sample_id
  )
  jn_scales <- logvar_joint_null_scales(jn_inputs$pcr)
  jn_d_inv2 <- jn_scales$d_inv2
  jn_w1 <- jn_inputs$w1
  jn_w2 <- jn_inputs$w2
  jn_eps_ref <- jn_inputs$eps_ref
  jn_qs_fn <- function(tau) {
    tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  }
  # scan at the benchmark grid resolution (at_tau reads this option; left unset
  # it defaults to a coarse 13-grid), and fill the driver-owned per-row fields
  # (sample_id, nearest crossing qtr) the sample-agnostic at_tau leaves NA
  jn_grid_opt <- options(logvar_joint_null_grid_n = log_var_eq$grid_n)
  jn_fill <- function(row) {
    row$sample_id <- log_var_eq$sample_id
    b <- c(row$b1, row$b2, row$b3)
    if (all(is.finite(b))) {
      e <- drop(jn_w1 - jn_w2 %*% b)
      row$nearest_crossing_qtr <- format(jn_inputs$qtr[which.min(abs(e))], "%Y Q%q")
    }
    row
  }
  # the carried nesting floor's q_reported must be the DIRECT objective q at the
  # arg-min, bit-identical to the scan's re-evaluation; 0.5 * scaled_l2^2 would
  # round-trip through sqrt and miss the exact monotonicity check by one ULP
  jn_qrep <- function(b) {
    logvar_joint_null_objective(b, jn_w1, jn_w2, jn_proj, jn_d_inv2)$q
  }

  # tau = 0 singleton: evaluate the closed-form Lewbel point through a
  # degenerate box, prepended as the first row (single-point evaluation, so no
  # independent starts and no perturbation set)
  jn_b_point <- set_id_mean_eq$theta_table$point
  jn_box0 <- list(
    coef = colnames(jn_w2), set_lower = jn_b_point, set_upper = jn_b_point
  )
  jn_row0 <- jn_fill(logvar_joint_null_at_tau(
    0, 0, jn_box0, jn_w1, jn_w2, jn_proj, jn_d_inv2, jn_eps_ref, jn_qs_fn(0)
  ))
  jn_row0$tau_order <- 1L
  jn_rows <- list(jn_row0)

  # display taus, carrying each attained arg-min forward as a nesting floor;
  # q_reported = the exact attained q makes the monotonicity check exact. Seed
  # the floor with the tau = 0 Lewbel point so the first display tau cannot
  # report a larger distance (the monotonicity integration check).
  jn_taus <- set_id_mean_eq$tau_display
  jn_prior <- if (is.finite(jn_row0$scaled_l2)) {
    list(structure(jn_b_point, q_reported = jn_qrep(jn_b_point)))
  } else {
    list()
  }
  for (jn_idx in seq_along(jn_taus)) {
    jn_tau <- jn_taus[[jn_idx]]
    jn_key <- sprintf("%.17g", jn_tau)
    jn_box <- mean_eq_bounds_tau[[jn_key]]
    stopifnot(!is.null(jn_box))
    jn_row <- jn_fill(logvar_joint_null_at_tau(
      jn_tau, jn_tau, jn_box, jn_w1, jn_w2, jn_proj, jn_d_inv2, jn_eps_ref,
      jn_qs_fn(jn_tau), jn_prior
    ))
    jn_row$tau_order <- jn_idx + 1L
    jn_rows[[length(jn_rows) + 1L]] <- jn_row
    jn_win <- c(jn_row$b1, jn_row$b2, jn_row$b3)
    attr(jn_win, "q_reported") <- jn_qrep(jn_win)
    jn_prior <- c(jn_prior, list(jn_win))
  }

  # provenance stamp for the deferred inference round; a read-only git call,
  # fully captured so it never reaches the console regression
  jn_commit <- tryCatch(
    suppressWarnings(
      system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) NA_character_
  )
  if (length(jn_commit) != 1L || !nzchar(jn_commit)) jn_commit <- NA_character_
  log_var_eq_joint_null <- list(
    schema_version = "1.0.0", estimator = "logols",
    diagnostic = "joint_null_theta_r", sample_id = log_var_eq$sample_id,
    inference_status = "deferred", benchmark_commit = jn_commit,
    root_tol = 1e-6,
    scales = list(
      d = jn_scales$d, d_inv2 = jn_scales$d_inv2,
      names = colnames(jn_inputs$pcr)
    ),
    rows = jn_rows
  )

  # delete any stale artifacts, then require both to be regenerated this run
  jn_out <- if (exists("out_dir")) out_dir else "scripts-paper/output"
  jn_csv <- file.path(jn_out, "log_var_eq_joint_null.csv")
  jn_rds <- file.path(jn_out, "log_var_eq_joint_null.rds")
  unlink(c(jn_csv, jn_rds))
  write_logvar_joint_null_artifacts(log_var_eq_joint_null, jn_csv, jn_rds)

  cat("[BEGIN LOGVAR JOINT NULL]\n")
  print_logvar_joint_null(jn_rows, jn_scales)
  cat("[END LOGVAR JOINT NULL]\n")

  options(jn_grid_opt)
  rm(
    jn_inputs, jn_proj, jn_scales, jn_d_inv2, jn_w1, jn_w2, jn_eps_ref,
    jn_qs_fn, jn_grid_opt, jn_fill, jn_qrep, jn_b_point, jn_box0, jn_row0, jn_rows,
    jn_taus, jn_prior, jn_idx, jn_tau, jn_key, jn_box, jn_row, jn_win,
    jn_commit, jn_out, jn_csv, jn_rds
  )
}
