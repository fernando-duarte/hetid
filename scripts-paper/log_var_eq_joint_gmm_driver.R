# Guarded pipeline orchestration for the joint moment-compatibility driver (Plan
# 4, logvar-joint-gmm). Definitions plus one exists()-guarded block: sourcing
# this file in the offline suite defines the row builders without touching the
# pipeline, because log_var_eq / set_id_mean_eq are absent there. Under run_all
# the driver validates the tracked decision record, asserts the sample identity,
# aligns the instrument by qtr, revalidates joint_input_id, and -- with both
# substantive gates declined -- runs Stage A graph replication only, writing the
# always-present status manifest and per-block skip reasons and printing the
# language-guarded comparison note. It joins, validates, writes, prints, and
# cleans up; the pure joint_gmm_spec core owns the arithmetic.

# Finite off-crossing b at which the software-invariance check runs: the tau = 0
# Lewbel point, augmented by a small feasible grid over the baseline display set
# when its box is finite. A set unbounded above tau* leaves only the point.
.jg_replication_points <- function(mean_eq) {
  pt <- as.numeric(mean_eq$theta_table$point)
  pts <- if (length(pt) && all(is.finite(pt))) list(pt) else list()
  base_tab <- mean_eq$set_tables[[1L]]$theta
  lower <- as.numeric(base_tab$set_lower)
  upper <- as.numeric(base_tab$set_upper)
  if (all(is.finite(c(lower, upper))) && all(upper >= lower)) {
    qs <- tau_quadratic_system(mean_eq$gamma, mean_eq$tau_display[1L], mean_eq$moments)
    grid <- logvar_feasible_grid(qs, lower, upper, 3L)
    for (i in seq_len(min(nrow(grid), 8L))) pts[[length(pts) + 1L]] <- grid[i, ]
  }
  if (!length(pts)) stop("log_var_eq_joint_gmm: no finite feasible replication point")
  do.call(rbind, pts)
}

# The always-written graph-replication status manifest row: the software-
# invariance verdict, its fresh residual and crossing distance, and the shared
# provenance/control fields the artifact schema pins.
.jg_manifest_row <- function(spec, repl, n_obs) {
  ct <- spec$controls
  list(
    schema_version = "2.0.0", diagnostic = "joint_gmm", sample_id = spec$sample_id,
    joint_input_id = spec$joint_input_id, decision_spec_id = spec$decision_spec_id,
    spec_id = spec$spec_id, inference_status = "deferred",
    moment_block = "graph_replication", numerical_status = repl$replication_status,
    search_status = "not_applicable", coverage_status = "not_applicable",
    fresh_moment_residual = repl$max_moment_residual, min_abs_eps = repl$min_abs_eps,
    n_obs = as.integer(n_obs), param_xtol_rel = ct[["param_xtol_rel"]],
    objective_tol = ct[["objective_tol"]], constraint_tol = ct[["constraint_tol"]],
    root_tol = ct[["root_tol"]]
  )
}

# A per-block skip row carrying the declined-gate reason and the block's pinned
# representation dimensions, so the artifact records why each substantive block
# did not run.
.jg_skip_row <- function(block, spec, reason) {
  ct <- spec$controls
  c(
    list(
      schema_version = "2.0.0", diagnostic = "joint_gmm", sample_id = spec$sample_id,
      joint_input_id = spec$joint_input_id, decision_spec_id = spec$decision_spec_id,
      spec_id = spec$spec_id, inference_status = "deferred", moment_block = block,
      numerical_status = "skipped", coverage_status = "not_applicable",
      skip_reason = reason, param_xtol_rel = ct[["param_xtol_rel"]],
      objective_tol = ct[["objective_tol"]], constraint_tol = ct[["constraint_tol"]],
      root_tol = ct[["root_tol"]]
    ),
    logvar_joint_gmm_dim_fields(block)
  )
}

if (exists("log_var_eq") && exists("set_id_mean_eq")) {
  jg_dec <- logvar_joint_decision_validate(logvar_joint_gmm_decision)
  # This round the driver implements only the no-answer default (Stage A at a_L);
  # a ratified empirical block, Stage C tolerance, or a_P/both target has no wired
  # runner, so refuse it loudly rather than silently omit, stub, or fall back to a_L.
  if (!logvar_joint_gmm_default_config_only(jg_dec)) {
    logvar_joint_decision_stop(
      "unwired_joint_branch",
      "the driver implements only the no-answer default this round"
    )
  }
  jg_sid <- log_var_eq$sample_id
  stopifnot(
    is.character(jg_sid), length(jg_sid) == 1L, nzchar(jg_sid),
    identical(jg_sid, log_var_eq$sample_contract$sample_id)
  )
  jg_in <- log_var_eq$inputs
  jg_qtr <- jg_in$qtr
  jg_w1 <- jg_in$w1
  jg_w2 <- jg_in$w2
  jg_pcr <- jg_in$pcr
  jg_x <- cbind("(Intercept)" = 1, jg_pcr)
  logvar_joint_check_design(jg_x)
  jg_proj <- logvar_projection(jg_pcr)
  # align the instrument to the log-variance qtr order (never by row position)
  jg_z <- logvar_joint_align_inputs(jg_qtr, set_id_mean_eq$qtr, set_id_mean_eq$z)
  jg_systems <- lapply(set_id_mean_eq$tau_display, function(tau) {
    tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  })
  jg_iid <- logvar_joint_input_id(
    jg_sid, jg_qtr, jg_z, set_id_mean_eq$gamma,
    set_id_mean_eq$tau_display, jg_systems
  )
  jg_spec <- logvar_joint_gmm_spec(jg_dec, jg_sid, jg_iid)
  jg_blocks <- logvar_joint_gmm_enabled_blocks(jg_dec)
  # declined gate: Stage A graph replication only (the dossier minimum product)
  jg_repl <- logvar_joint_gmm_graph_replication(
    .jg_replication_points(set_id_mean_eq), jg_w1, jg_w2, jg_proj, jg_x,
    jg_spec$controls[["root_tol"]]
  )
  jg_rows <- list(.jg_manifest_row(jg_spec, jg_repl, length(jg_w1)))
  if (!("z" %in% jg_blocks)) {
    jg_rows[[length(jg_rows) + 1L]] <- .jg_skip_row("z", jg_spec, "gate_declined")
  }
  if (!("log_ppml" %in% jg_blocks)) {
    jg_rows[[length(jg_rows) + 1L]] <- .jg_skip_row("log_ppml", jg_spec, "gate_declined")
  }
  # gated Stage C stays disabled without a ratified delta and prints its reason
  jg_projection <- logvar_joint_project_set(
    list(block = "none", n_slope = ncol(jg_w2)), jg_dec$moment_delta
  )
  log_var_eq_joint_gmm <- list(
    schema_version = "2.0.0", diagnostic = "joint_gmm", sample_id = jg_sid,
    joint_input_id = jg_iid, decision_spec_id = jg_dec$decision_spec_id,
    spec_id = jg_spec$spec_id, inference_status = "deferred", decision = jg_dec,
    spec = jg_spec, graph_replication = jg_repl, projection = jg_projection,
    rows = jg_rows
  )
  jg_out <- if (exists("out_dir")) out_dir else "scripts-paper/output"
  jg_csv <- file.path(jg_out, "log_var_eq_joint_gmm.csv")
  jg_rds <- file.path(jg_out, "log_var_eq_joint_gmm.rds")
  unlink(c(jg_csv, jg_rds))
  write_joint_gmm_artifacts(log_var_eq_joint_gmm, jg_csv, jg_rds)

  cat("[BEGIN LOGVAR JOINT GMM]\n")
  cat(build_joint_gmm_comparison(list(moment_block = "none")), sep = "\n")
  cat(sprintf(
    "\n  graph replication: %s (max scaled moment residual %.3g over %d of %d points)\n",
    jg_repl$replication_status, jg_repl$max_moment_residual, jg_repl$n_finite,
    jg_repl$n_points
  ))
  cat(sprintf(
    "  z block: skipped (%s); log/PPML block: skipped (%s)\n",
    "gate_declined", "gate_declined"
  ))
  cat(sprintf("  %s\n", jg_projection$reason))
  cat("[END LOGVAR JOINT GMM]\n")

  rm(
    jg_dec, jg_sid, jg_in, jg_qtr, jg_w1, jg_w2, jg_pcr, jg_x, jg_proj, jg_z,
    jg_systems, jg_iid, jg_spec, jg_blocks, jg_repl, jg_rows, jg_projection,
    jg_out, jg_csv, jg_rds
  )
}
