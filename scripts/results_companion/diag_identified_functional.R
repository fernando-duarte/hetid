# Identified-functional and weak-identification geometry for the results companion.
# Determines which linear combinations c'theta of the SDF-news loadings are
# identified over the fixed-VFCI set, and quantifies the near-rank-one geometry of
# Q that drives the result. Deterministic (seed 123); recomputes from data.rds.

source(here::here("scripts/utils/common_settings.R"))
set.seed(SEED)
out_dir <- file.path(OUTPUT_TEMP_DIR, "results_companion")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Fresh residuals + moments (the default I = 3 system, n_pcs = 4) ---
inputs <- load_identification_inputs()
lookup <- inputs$lookup
resid <- compute_identification_residuals(inputs$data)
pcs <- resid$pcs_aligned
moments <- compute_identification_moments(resid$w1, resid$w2, pcs)
i_dim <- attr(moments, "n_components")
n_obs <- resid$n_obs
sd_w1 <- stats::sd(resid$w1)
sd_w2 <- apply(resid$w2, 2, stats::sd)

cat("=== sample ===\n")
cat(
  "n_obs =", n_obs, " i_dim =", i_dim, " maturities =",
  paste(lookup$bond_maturity, collapse = "/"), "months\n"
)
cat("sd(W1) =", signif(sd_w1, 4), " sd(W2) =", paste(signif(sd_w2, 4), collapse = ", "), "\n\n")

# --- Cross-maturity correlation of the SDF-news residuals W2 ---
w2_cor <- stats::cor(resid$w2)
cat("=== cross-maturity W2 correlation ===\n")
print(round(w2_cor, 5))
cat(
  "pairwise: 2y-5y =", round(w2_cor[1, 2], 5),
  " 2y-9y =", round(w2_cor[1, 3], 5), " 5y-9y =", round(w2_cor[2, 3], 5), "\n\n"
)

# --- VFCI gamma and quadratic systems at three slacks ---
gamma_vfci <- get_baseline_gamma("vfci", n_pcs = 4, n_components = i_dim)
qs0 <- build_pipeline_quadratic_system(gamma_vfci, rep(0, i_dim), moments)
qs02 <- build_pipeline_quadratic_system(gamma_vfci, rep(0.02, i_dim), moments)
qs05 <- build_pipeline_quadratic_system(gamma_vfci, rep(0.05, i_dim), moments)

# --- Q, L, SVD, condition number, point solve (verify against saved artifact) ---
qmat <- do.call(rbind, qs0$components$Q_i)
lvec <- qs0$components$L_i
sv <- svd(qmat)
pt_vfci <- solve_point_identification(qs0$components)
cat("=== Q geometry (VFCI, tau = 0) ===\n")
cat("singular values:", paste(signif(sv$d, 4), collapse = ", "), "\n")
cat(
  "cond(Q) = kappa =", signif(pt_vfci$cond, 8),
  " ratio d1/dI =", signif(sv$d[1] / sv$d[i_dim], 8), "\n"
)
cat("point solve theta (VFCI):", paste(signif(pt_vfci$theta, 6), collapse = ", "), "\n")
std_pt <- pt_vfci$theta * sd_w2 / sd_w1
cat("standardized (theta_i * sd(W2_i)/sd(W1)):", paste(signif(std_pt, 4), collapse = ", "), "\n\n")

# --- Per-coordinate profile bounds at tau = 0.05 (expect 2y bounded, 5y/9y not) ---
b05 <- solve_all_profile_bounds(qs05$quadratic)
cat("=== per-coordinate bounds, VFCI, tau = 0.05 ===\n")
print(b05[, c("component", "lower", "upper", "width", "bounded_lower", "bounded_upper")])
cat("\n")

# --- Identified functionals: bounds of c'theta over the set for candidate c ---
news_pc <- stats::prcomp(resid$w2, center = TRUE, scale. = FALSE)
dirs <- list(
  e_2y = c(1, 0, 0), e_5y = c(0, 1, 0), e_9y = c(0, 0, 1),
  level = rep(1, i_dim) / sqrt(i_dim),
  Q_sv1 = sv$v[, 1], Q_sv2 = sv$v[, 2], Q_sv3 = sv$v[, i_dim],
  news_pc1 = as.numeric(news_pc$rotation[, 1])
)
ctheta_at <- function(qs, tau_label) {
  rows <- lapply(names(dirs), function(nm) {
    cc <- dirs[[nm]]
    fmin <- solve_linear_functional_bound(qs$quadratic, cc, "min")
    fmax <- solve_linear_functional_bound(qs$quadratic, cc, "max")
    data.frame(
      tau = tau_label, direction = nm,
      point_tau0 = sum(cc * pt_vfci$theta),
      lo = fmin$bound, hi = fmax$bound,
      width = fmax$bound - fmin$bound,
      bounded = isTRUE(fmin$bounded) && isTRUE(fmax$bounded),
      valid = isTRUE(fmin$valid) && isTRUE(fmax$valid),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
ct <- rbind(ctheta_at(qs05, 0.05), ctheta_at(qs02, 0.02))
cat("=== identified functionals c'theta over the VFCI set ===\n")
cat("(news_pc1 rotation[,1] =", paste(signif(dirs$news_pc1, 3), collapse = ", "), ")\n")
cat("(Q_sv1 =", paste(signif(dirs$Q_sv1, 3), collapse = ", "), ")\n")
print(ct, digits = 5, row.names = FALSE)
cat("\n")

# --- tau = 0 point-solve swing across fixed weightings (VFCI vs reduced-form) ---
beta2r <- resid$w2_coefficients
gamma_rf <- build_reduced_form_gamma(beta2r)
pt_rf <- solve_point_identification(
  build_pipeline_quadratic_system(gamma_rf, rep(0, i_dim), moments)$components
)
cat("=== tau = 0 point-solve swing across fixed gamma ===\n")
cat(
  "VFCI gamma:        ", paste(signif(pt_vfci$theta, 6), collapse = ", "),
  " cond =", signif(pt_vfci$cond, 6), "\n"
)
cat(
  "reduced-form gamma:", paste(signif(pt_rf$theta, 6), collapse = ", "),
  " cond =", signif(pt_rf$cond, 6), "\n\n"
)

saveRDS(
  list(
    n_obs = n_obs, i_dim = i_dim, w2_cor = w2_cor,
    sv = sv, cond = pt_vfci$cond, theta_vfci = pt_vfci$theta,
    theta_rf = pt_rf$theta, std_pt = std_pt, bounds_tau05 = b05,
    ctheta = ct, sd_w1 = sd_w1, sd_w2 = sd_w2
  ),
  file.path(out_dir, "identified_functional.rds")
)
cat("saved:", file.path(out_dir, "identified_functional.rds"), "\n")
