# Gate dispatch, the pure joint_gmm_spec core, and the graph-replication
# check for the joint moment-compatibility layer (joint-GMM, logvar-joint-gmm).
# Definitions only. The enabled-block and ratified-projection predicates read the validated
# decision record; the spec core stamps a canonical spec_id over the sample and
# joint identities, the decision spec_id, the module policy versions, the frozen
# scales, the pinned tolerance controls, the search budgets, and the enabled
# blocks; the graph-replication core proves the stacked just-identified moment
# layer reproduces the benchmark two-step map at feasible b -- a software-
# invariance check, not new identification. The artifact and projection modules
# and the guarded pipeline driver are sourced here. Sourced by
# tests/diagnostics/joint_gmm/test_joint_gmm.R and, at pipeline time, by run_pipeline.R.

source(paper_path("log_variance", "diagnostics", "joint_gmm", "artifact_schema.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "projection.R"))

# Enabled moment blocks from the decision record: the z block when enable_z, the
# log/PPML block when enable_log_ppml, both when both, and none when neither (the
# no-answer default, which runs only the graph-replication check).
logvar_joint_gmm_enabled_blocks <- function(decision) {
  blocks <- character(0)
  if (isTRUE(decision$enable_z)) blocks <- c(blocks, "z")
  if (isTRUE(decision$enable_log_ppml)) blocks <- c(blocks, "log_ppml")
  blocks
}

# ratified-projection is enabled only with an explicit request and a nonempty ratified delta
# grid; a request without a delta stays disabled (a tolerance is never a region).
logvar_joint_gmm_stage_c_enabled <- function(decision) {
  isTRUE(decision$stage_c_requested) && length(decision$moment_delta) > 0L
}

# TRUE only for the no-answer-default scientific configuration the driver actually
# implements this round: no empirical block, no ratified-projection tolerance, a_L intercept.
# Any other schema-valid choice has no wired driver runner, so the driver refuses
# it rather than silently omitting a block, stubbing a projection, or using a_L.
logvar_joint_gmm_default_config_only <- function(decision) {
  !isTRUE(decision$enable_z) && !isTRUE(decision$enable_log_ppml) &&
    !isTRUE(decision$stage_c_requested) &&
    length(decision$moment_delta) == 0L &&
    identical(decision$intercept_target, "a_L")
}

# Canonical 17-significant-digit serialization for the spec-ID numeric parts, so
# the stamp is representation-stable across runs (mirrors the decision spec-ID).
.jg_fmt17 <- function(x) {
  formatC(as.numeric(x), digits = 17, format = "fg", flag = "#")
}

# Canonical spec_id over the sample/joint identities, the decision spec_id, the
# module policy versions, the frozen moment/coordinate scales, the tolerance
# controls, the search budgets, and the enabled blocks. Written with the pinned
# RDS version and hashed with base tools, so any substantive change re-stamps it.
logvar_joint_gmm_spec_id <- function(parts) {
  payload <- list(
    sample_id = as.character(parts$sample_id),
    joint_input_id = as.character(parts$joint_input_id),
    decision_spec_id = as.character(parts$decision_spec_id),
    basis_version = as.character(parts$basis_version),
    profile_version = as.character(parts$profile_version),
    candidate_version = as.character(parts$candidate_version),
    enabled_blocks = as.character(parts$enabled_blocks),
    moment_scales = .jg_fmt17(parts$moment_scales),
    coord_scales = .jg_fmt17(parts$coord_scales),
    controls = .jg_fmt17(parts$controls),
    budgets = .jg_fmt17(parts$budgets),
    moment_delta = .jg_fmt17(sort(unique(as.numeric(parts$moment_delta))))
  )
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(payload, tmp, version = 3)
  unname(tools::md5sum(tmp))
}

# The pure joint_gmm_spec core: assemble the deterministic specification the
# driver, artifacts, and deferred inference round all share. The tolerance
# controls and search budgets are read from logvar_joint_gmm_constants (the single
# source in the epigraph module) rather than re-pinned; the scales are frozen
# upstream and passed in (empty on the declined-gate path, which runs no search).
logvar_joint_gmm_spec <- function(decision, sample_id, joint_input_id,
                                  coord_scales = numeric(0),
                                  moment_scales = numeric(0)) {
  k <- logvar_joint_gmm_constants
  controls <- c(
    param_xtol_rel = k$param_xtol_rel, objective_tol = k$objective_tol,
    constraint_tol = k$constraint_tol, root_tol = k$root_tol
  )
  budgets <- c(
    grid_n = k$grid_n, grid_floor = k$grid_floor, gmm_grid_cap = k$gmm_grid_cap,
    candidate_eval_cap = k$candidate_eval_cap,
    pattern_start_cap = k$pattern_start_cap,
    per_solve_maxeval = k$per_solve_maxeval, box_h1 = k$box_half_widths[1L],
    box_h2 = k$box_half_widths[2L], box_h3 = k$box_half_widths[3L]
  )
  spec <- list(
    sample_id = sample_id, joint_input_id = joint_input_id,
    decision_spec_id = decision$decision_spec_id,
    basis_version = "basis-v1", profile_version = "profile-v1",
    candidate_version = "candidate-v1",
    enabled_blocks = logvar_joint_gmm_enabled_blocks(decision),
    moment_scales = moment_scales, coord_scales = coord_scales,
    controls = controls, budgets = budgets, moment_delta = decision$moment_delta,
    combine_blocks = FALSE,
    stage_c_enabled = logvar_joint_gmm_stage_c_enabled(decision)
  )
  spec$spec_id <- logvar_joint_gmm_spec_id(spec)
  spec
}

# Graph replication: at every feasible off-crossing b, the just-identified
# log-variance moment X'{log(e^2) - X (a_L, beta)} / n vanishes at the projected
# (a_L, beta) = theta_hat(b), so the stacked moment layer reproduces the benchmark
# two-step map. attained only when every finite point clears root_tol.
logvar_joint_gmm_graph_replication <- function(b_points, w1, w2, proj, x_mat,
                                               root_tol = logvar_joint_gmm_constants$root_tol) {
  b_points <- as.matrix(b_points)
  max_resid <- 0
  n_finite <- 0L
  min_abs_eps <- Inf
  for (i in seq_len(nrow(b_points))) {
    b <- b_points[i, ]
    e <- drop(w1 - w2 %*% b)
    min_abs_eps <- min(min_abs_eps, min(abs(e)))
    theta <- logvar_theta_hat(b, w1, w2, proj)
    if (any(!is.finite(theta))) next
    resid <- max(abs(logvar_moment_log(b, theta[1L], theta[-1L], w1, w2, x_mat)))
    if (is.finite(resid)) {
      n_finite <- n_finite + 1L
      max_resid <- max(max_resid, resid)
    }
  }
  list(
    replication_status = if (n_finite > 0L && max_resid < root_tol) {
      "attained"
    } else {
      "unreliable"
    },
    max_moment_residual = max_resid, n_points = nrow(b_points),
    n_finite = n_finite,
    min_abs_eps = if (is.finite(min_abs_eps)) min_abs_eps else NA_real_
  )
}

# The guarded pipeline driver (joins, validation, artifact writes, prints).
source(paper_path("log_variance", "diagnostics", "joint_gmm", "pipeline_driver.R"))
