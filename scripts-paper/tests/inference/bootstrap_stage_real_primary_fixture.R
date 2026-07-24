# Shared real-callback fixture for fixed-index unified-stage checks.

bsr_reference_est <- estimate_set_id_system(lbd_dat, lbd_spec)
bsr_system_fields <- c(
  "gamma", "y1_col", "x_cols", "y2_cols", "z_col", "impose_null"
)
bsr_mean_spec <- c(
  list(
    coefs = c(
      names(bsr_reference_est$beta1r),
      rownames(bsr_reference_est$beta2r)
    ),
    taus = lbd_spec$taus[-1L],
    tau_grid = seq(0, max(lbd_spec$taus), by = 0.05),
    tau_star_iterations =
      PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bootstrap_bisection_iterations
  ),
  lbd_spec[bsr_system_fields]
)
bsr_logvar_spec <- logvar_set_boot_compat_spec(lbd_spec)
bsr_logvar_spec$response_scale <- 1
bsr_logvar_spec$logols_coef <- stats::setNames(
  rep(0, length(bsr_logvar_spec$coefs)),
  bsr_logvar_spec$coefs
)
bsr_logvar_spec$normal_log_square_gap <- LOGVAR_NORMAL_LOG_SQUARE_GAP
bsr_logvar_spec$se_types <- stats::setNames(
  rep("hac", length(bsr_logvar_spec$estimator_ids)),
  bsr_logvar_spec$estimator_ids
)
bsr_stage_spec <- list(
  frame = list(data = lbd_dat, key_col = "qtr", sample_size = nrow(lbd_dat)),
  system = lbd_spec[bsr_system_fields],
  tau = list(display = lbd_spec$taus[-1L], union = lbd_spec$taus),
  mean = list(
    coefs = bsr_mean_spec$coefs,
    tau_star_grid = bsr_mean_spec$tau_grid,
    tau_star_iterations = bsr_mean_spec$tau_star_iterations
  ),
  log_variance = bsr_logvar_spec[BOOTSTRAP_STAGE_FIELDS$log_variance]
)
bsr_eval_specs <- bootstrap_stage_eval_specs(bsr_stage_spec)
bsr_collect_specs <- list(
  mean = list(
    coefs = bsr_stage_spec$mean$coefs,
    taus = bsr_stage_spec$tau$display
  ),
  log_variance = list(
    coefs = bsr_stage_spec$log_variance$coefs,
    taus = bsr_stage_spec$tau$union,
    estimator_ids = bsr_stage_spec$log_variance$estimator_ids
  )
)
bsr_logvar_spec <- c(
  bsr_stage_spec$log_variance,
  bsr_stage_spec$system,
  list(
    taus = bsr_stage_spec$tau$union,
    key_col = bsr_stage_spec$frame$key_col,
    builders = bsr_eval_specs$log_variance$builders
  )
)
bsr_family <- paper_mbb_index_family(
  2L,
  nrow(lbd_dat),
  paper_mbb_block_len(nrow(lbd_dat)),
  20260723L,
  paper_mbb_protocol()$family_names[["primary"]]
)
bsr_primary_callback <- function(index, draw_id) {
  bootstrap_stage_primary_indexed_draw(index, draw_id, bsr_stage_spec)
}
bsr_sensitivity_family <- paper_mbb_index_family(
  2L,
  nrow(lbd_dat),
  bootstrap_stage_sensitivity_block_length(
    paper_mbb_block_len(nrow(lbd_dat))
  ),
  20260723L,
  paper_mbb_protocol()$family_names[["sensitivity"]]
)
bsr_sensitivity_callback <- function(index, draw_id) {
  bootstrap_stage_volatility_indexed_draw(index, draw_id, bsr_stage_spec)
}
