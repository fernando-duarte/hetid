# Single-maturity sweep, nested-conditioning, and structural recovery for the
# results companion. Shows (a) each maturity alone is well-conditioned and bounded
# -- isolating the difficulty as JOINT collinearity -- and (b) what the data pin
# down: the structural coefficients beta1(theta) = beta1R - (beta2R)'theta at the
# VFCI tau = 0 point (a single theta) versus the PC loadings beta over the honest
# VFCI set (unbounded). With common conditioning, beta2R is FULL-WIDTH and carries
# nonzero lag columns, so the lag coefficients psi are point-VALUED only at the
# fixed theta below; over the identified set Theta they are set-valued (not
# point-identified). Deterministic (seed 123).

source(here::here("scripts/utils/common_settings.R"))
set.seed(SEED)
out_dir <- file.path(OUTPUT_TEMP_DIR, "results_companion")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Single-maturity (true I = 1) sweep: each bond alone, fixed VFCI weight ---
single_row <- function(m) {
  inp <- load_identification_inputs(maturities = m)
  rs <- compute_identification_residuals(inp$data, maturities = m)
  mom <- compute_identification_moments(rs$w1, rs$w2, rs$pcs_aligned)
  g <- get_baseline_gamma(
    "vfci",
    n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, n_components = 1
  )
  pt <- solve_point_identification(build_pipeline_quadratic_system(g, 0, mom)$components)
  b05 <- solve_all_profile_bounds(
    build_pipeline_quadratic_system(g, BASELINE_TAU, mom)$quadratic
  )
  sd1 <- stats::sd(rs$w1)
  sd2 <- stats::sd(rs$w2[, 1])
  data.frame(
    maturity_m = m, point_tau0 = pt$theta, cond = pt$cond,
    lo_tau05 = b05$lower, hi_tau05 = b05$upper, width_tau05 = b05$width,
    bounded = b05$bounded_lower && b05$bounded_upper,
    std_effect_tau0 = pt$theta * sd2 / sd1, stringsAsFactors = FALSE
  )
}
single <- do.call(rbind, lapply(c(24L, 60L, 108L), single_row))
cat("=== single-maturity (I = 1) sweep, fixed VFCI weight ===\n")
print(single, digits = 5, row.names = FALSE)
cat("\n")

# --- Nested conditioning: cond(Q) as maturities are added (collinearity proof) ---
nest_cond <- function(mats) {
  inp <- load_identification_inputs(maturities = mats)
  rs <- compute_identification_residuals(inp$data, maturities = mats)
  mom <- compute_identification_moments(rs$w1, rs$w2, rs$pcs_aligned)
  g <- get_baseline_gamma(
    "vfci",
    n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, n_components = length(mats)
  )
  pt <- solve_point_identification(build_pipeline_quadratic_system(g, rep(0, length(mats)), mom)$components)
  if (is.null(pt)) NA_real_ else pt$cond
}
nest <- data.frame(
  maturities = c("24", "24-60", "24-60-108"),
  cond = c(nest_cond(24L), nest_cond(c(24L, 60L)), nest_cond(c(24L, 60L, 108L)))
)
cat("=== nested conditioning cond(Q) ===\n")
print(nest, digits = 6, row.names = FALSE)
cat("\n")

# --- Structural recovery (I = 3): beta1(theta) at the VFCI tau = 0 point (a
# single theta, so psi is point-VALUED here) versus beta over the VFCI set ---
inputs <- load_identification_inputs()
resid <- compute_identification_residuals(inputs$data)
moments <- compute_identification_moments(resid$w1, resid$w2, resid$pcs_aligned)
i_dim <- attr(moments, "n_components")
beta1r <- resid$w1_result$coefficients
beta2r <- resid$w2_coefficients
# Classify the common-design columns by NAME (not by width): the spec conditions
# BOTH Y1 and Y2 on the same X_t = (1, PC, H lags of Y1), so beta2R is FULL-WIDTH
# and column-matched to beta1R. PCs are the only instruments (^pc[0-9]+$); the
# l.y1* columns are the predetermined conditioning lags.
design_cols <- classify_common_design_cols(names(beta1r))
pc_cols <- design_cols$pc_cols
lag_cols <- design_cols$lag_cols
n_pcs <- design_cols$n_pcs
n_lag <- design_cols$n_lag
# beta2R is already full-width and column-matched to beta1R; no zero-padding is
# needed and the exact recovery identity carries every column (constant, PCs, AND
# lags) through directly. Fail loudly if a legacy PC-only beta2R is ever passed --
# do NOT silently re-pad, which would falsely point-identify psi.
assert_beta_columns_matched(beta1r, beta2r)
gamma_vfci <- get_baseline_gamma(
  "vfci",
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, n_components = i_dim
)
pt_vfci <- solve_point_identification(
  build_pipeline_quadratic_system(gamma_vfci, rep(0, i_dim), moments)$components
)
beta1_point <- recover_structural_coefficients(beta1r, beta2r, pt_vfci$theta)
cat("=== structural recovery at the VFCI tau = 0 point ===\n")
print(round(beta1_point, 4))
cat(
  "(the last", n_lag, "entries are psi at this single theta; psi is",
  "set-valued over Theta)\n\n"
)

# beta (intercept + PC loadings) over the VFCI tau = 0.05 SET: unbounded?
# Sweep only the constant and the named PC columns (lags are excluded here, since
# the psi rows are reported as a point at the fixed theta above).
qs05 <- build_pipeline_quadratic_system(gamma_vfci, rep(BASELINE_TAU, i_dim), moments)
p_lab <- names(beta1r)
intercept_col <- design_cols$intercept_col
beta_set <- lapply(c(intercept_col, pc_cols), function(p) {
  cp <- as.numeric(beta2r[, p])
  fmn <- solve_linear_functional_bound(qs05$quadratic, cp, "min")
  fmx <- solve_linear_functional_bound(qs05$quadratic, cp, "max")
  data.frame(
    coef = p_lab[p], lo = beta1r[p] - fmx$bound, hi = beta1r[p] - fmn$bound,
    bounded = isTRUE(fmn$bounded) && isTRUE(fmx$bounded), stringsAsFactors = FALSE
  )
})
beta_set <- do.call(rbind, beta_set)
cat("=== beta (intercept + PC loadings) over the VFCI tau = 0.05 set ===\n")
print(beta_set, digits = 4, row.names = FALSE)
cat("\n")

saveRDS(
  list(
    single = single, nest = nest, beta1_point = beta1_point,
    beta_set = beta_set, n_lag = n_lag, n_pcs = n_pcs
  ),
  file.path(out_dir, "single_maturity_and_recovery.rds")
)
cat("saved:", file.path(out_dir, "single_maturity_and_recovery.rds"), "\n")
