# Create Consumption-Equation Structural-Coefficient Table
# Publication table of the first-stage coefficients of the consumption-growth
# equation Y1 = bar.beta + sum_n beta_n PC_n + sum_m theta_m SDFnews_m + eps1,
# recovered via beta1(theta) = beta1R - (beta2R)' theta (the exported package
# function recover_structural_coefficients). Two columns:
#   * Point identification (tau = 0): theta from the reduced-form gamma
#     (Y2-on-PC first-stage slopes); beta1(theta) is a single vector.
#   * Set identification (tau = BASELINE_TAU): the optimized gamma; each
#     coefficient is an EXACT interval over the identified set Theta, the beta1
#     rows obtained by optimizing the linear functional beta1_k(theta) over Theta
#     (solve_linear_functional_bound), the theta rows reused from stage 05.
# beta2R (= beta20, the Y2 equation) is point-identified and NOT tabulated.

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/latex_simple_table.R"))

cli_h1("Creating Consumption-Equation Structural-Coefficient Table")

# Reduced-form gamma (maturities mode): the Y2-on-PC slope block of beta2R,
# transposed to the J x I (instruments x components) layout build_quadratic_system
# expects. Drops the intercept column.
build_reduced_form_gamma <- function(beta2r) {
  gamma <- t(beta2r[, -1, drop = FALSE])
  stopifnot(is.matrix(gamma), all(is.finite(gamma)))
  attr(gamma, "method") <- "reduced_form_maturities"
  gamma
}

interval_cell <- function(lo, hi, valid_lo, valid_hi) {
  paste0(
    "[", format_bound(lo, valid_lo, TABLE_DIGITS), ", ",
    format_bound(hi, valid_hi, TABLE_DIGITS), "]"
  )
}

# Render a positive scalar as pdfLaTeX math scientific notation, m x 10^e.
latex_sci <- function(x, digits = 2) {
  e <- floor(log10(abs(x)))
  m <- x / 10^e
  paste0(formatC(m, format = "f", digits = digits), "\\times 10^{", e, "}")
}

# --- Canonical moments (stage 04) + optimized gamma (stage 05) ---
baseline <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_baseline",
  "baseline_identification_results.rds"
))
optimized <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_optimized",
  "optimized_identification_results.rds"
))
moments <- baseline$moments
lookup <- baseline$lookup
gamma_opt <- optimized$gamma_optimized
i_dim <- attr(moments, "n_components")
# tau_fixed is the per-component slack vector used by stage 05; require it to be
# the baseline slack on every component so the reused optimized_bounds and the
# recomputed beta1 intervals share the same tau.
tau_set <- optimized$tau_fixed
stopifnot(
  length(tau_set) == i_dim,
  all(abs(tau_set - BASELINE_TAU) < 1e-12)
)
tau_disp <- tau_set[1]

# --- Recompute residuals to extract beta1R / beta2R, asserting they are the
# same residuals that built the canonical moments (so beta1(theta) is consistent
# with the identified set Theta). ---
data <- readRDS(DATA_RDS_PATH)
if (is.list(data) && !is.data.frame(data)) {
  data <- as.data.frame(data)
}
resid <- compute_identification_residuals(data, mode = "maturities")
stopifnot(
  isTRUE(all.equal(unname(resid$w1), unname(baseline$residuals$w1))),
  isTRUE(all.equal(unname(unclass(resid$w2)), unname(unclass(baseline$residuals$w2))))
)
beta1r <- resid$w1_result$coefficients
beta2r <- resid$w2_coefficients
stopifnot(
  length(beta1r) == ncol(beta2r),
  nrow(beta2r) == i_dim,
  identical(names(beta1r), colnames(beta2r))
)
n_pcs <- length(beta1r) - 1L

# === POINT IDENTIFICATION (tau = 0, reduced-form gamma) ===
gamma_rf <- build_reduced_form_gamma(beta2r)
qs0 <- build_quadratic_system(gamma_rf, rep(0, i_dim), moments)
pt0 <- solve_point_identification(qs0$components)

cond_max <- 1e12
point_unreliable <- is.null(pt0) || !is.finite(pt0$cond) || pt0$cond > cond_max
if (point_unreliable) {
  reason <- if (is.null(pt0)) {
    "$Q\\theta = L$ is rank-deficient or inconsistent"
  } else {
    paste0(
      "the linear system is near-singular ($\\kappa(Q) = ",
      latex_sci(pt0$cond), "$)"
    )
  }
  cli_alert_danger(paste0(
    "tau = 0 reduced-form-gamma point identification is unreliable: ", reason,
    "; reporting the point column as unreliable."
  ))
  theta_point <- NULL
  beta1_point <- NULL
  cond_note <- reason
} else {
  theta_point <- pt0$theta
  beta1_point <- recover_structural_coefficients(beta1r, beta2r, theta_point)
  cli_alert_info(paste0(
    "tau = 0 point identification: condition number cond(Q) = ",
    formatC(pt0$cond, format = "e", digits = 2)
  ))
  cond_note <- paste0(
    "the point-identification linear system has condition ",
    "number $\\kappa(Q) = ", latex_sci(pt0$cond), "$"
  )
}

point_cell <- function(value) {
  if (point_unreliable || is.null(value) || !is.finite(value)) {
    return("unreliable")
  }
  formatC(value, format = "f", digits = TABLE_DIGITS)
}

# === SET IDENTIFICATION (tau = tau_set, optimized gamma) ===
qs_set <- build_quadratic_system(gamma_opt, tau_set, moments)

# beta1 (constant + PC) coefficients: exact range over Theta of the linear
# functional beta1_p(theta) = beta1R_p - c_p' theta with c_p = beta2R[, p].
# The interval's UPPER end uses the MIN of c_p'theta and vice versa (sign flip).
p_dim <- length(beta1r)
beta1_lo <- numeric(p_dim)
beta1_hi <- numeric(p_dim)
beta1_lo_valid <- logical(p_dim)
beta1_hi_valid <- logical(p_dim)
for (p in seq_len(p_dim)) {
  c_p <- as.numeric(beta2r[, p])
  fmin <- solve_linear_functional_bound(qs_set$quadratic, c_p, "min")
  fmax <- solve_linear_functional_bound(qs_set$quadratic, c_p, "max")
  beta1_lo[p] <- beta1r[p] - fmax$bound
  beta1_hi[p] <- beta1r[p] - fmin$bound
  beta1_lo_valid[p] <- isTRUE(fmax$valid)
  beta1_hi_valid[p] <- isTRUE(fmin$valid)
}

# theta (SDF-news) set rows: reuse the stage-05 optimized bounds verbatim.
ob <- optimized$optimized_bounds
ob <- ob[order(ob$component), , drop = FALSE]
stopifnot(nrow(ob) == i_dim)

# --- Assemble the 8 rows (constant, PC1..PCn, then the I SDF-news theta rows) ---
bond_years <- lookup$bond_maturity / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
row_labels <- c(
  "Constant $\\bar{\\beta}$",
  paste0("$\\beta_{\\mathrm{PC}", seq_len(n_pcs), "}$"),
  paste0("$\\theta_{", seq_len(i_dim), "}$ (SDF news, ", bond_years, "-year)")
)
point_col <- c(
  vapply(seq_len(p_dim), function(p) point_cell(beta1_point[p]), character(1)),
  vapply(seq_len(i_dim), function(i) {
    if (point_unreliable) "unreliable" else point_cell(theta_point[i])
  }, character(1))
)
set_col <- c(
  vapply(seq_len(p_dim), function(p) {
    interval_cell(beta1_lo[p], beta1_hi[p], beta1_lo_valid[p], beta1_hi_valid[p])
  }, character(1)),
  vapply(seq_len(i_dim), function(i) {
    interval_cell(ob$lower[i], ob$upper[i], ob$valid_lower[i], ob$valid_upper[i])
  }, character(1))
)

# --- Caption + notes (pdfLaTeX-safe LaTeX math, not Unicode) ---
n_obs <- resid$n_obs
year_quarter <- function(d) {
  paste0(format(d, "%Y"), "Q", (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L)
}
period <- if ("date" %in% names(data)) {
  rng <- range(as.Date(data$date))
  paste0(year_quarter(rng[1]), "--", year_quarter(rng[2]))
} else {
  "the quarterly sample"
}
caption <- paste(
  "Structural coefficients of the consumption-growth equation under point and",
  "set identification."
)
notes <- c(
  paste0(
    "The table reports the coefficients of the consumption-growth ",
    "equation $Y_{1,t+1} = \\bar{\\beta} + \\sum_{n=1}^{", n_pcs,
    "} \\beta_n \\mathrm{PC}_{n,t} + \\sum_{m=1}^{", i_dim,
    "} \\theta_m \\mathrm{SDFnews}_{m,t+1} + \\varepsilon_{1,t+1}$, where ",
    "$Y_1$ is consumption growth, $\\mathrm{PC}_1,\\ldots,\\mathrm{PC}_{", n_pcs,
    "}$ the principal components, and $\\mathrm{SDFnews}_m$ the ",
    "stochastic-discount-factor news of the listed bond maturities."
  ),
  paste0(
    "The reduced-form projections $\\beta_1^{R}$ (consumption growth on ",
    "the PCs) and $\\beta_2^{R}$ (each SDF-news series on the PCs) are ",
    "invariant to the instrument loadings and identical across columns; the ",
    "structural coefficients are recovered exactly as $\\beta_1(\\theta) = ",
    "\\beta_1^{R} - (\\beta_2^{R})'\\theta$, so all identification content ",
    "enters through $\\theta$."
  ),
  paste0(
    "\\textit{Point identification} ($\\tau = 0$) identifies $\\theta$ ",
    "from the reduced-form loadings $\\Gamma^{R}$ built from $\\beta_2^{R}$; ",
    "each moment restriction is then a perfect square, so the identified set ",
    "collapses to the single point solving the exactly-identified linear ",
    "system $Q\\theta = L$, and ", cond_note, "."
  ),
  paste0(
    "\\textit{Set identification} ($\\tau = ", tau_disp, "$) uses the ",
    "width-minimizing optimized loadings $\\Gamma^{\\star}$. Each reported ",
    "interval is the \\emph{exact range of the coefficient over the identified ",
    "set} $\\Theta$: for $\\theta_m$, $[\\min_{\\theta\\in\\Theta}\\theta_m, ",
    "\\max_{\\theta\\in\\Theta}\\theta_m]$; for $\\bar{\\beta}$ and $\\beta_n$, ",
    "obtained by optimizing the linear functional $\\beta_{1,k}(\\theta)$ ",
    "directly over $\\Theta$ (coordinate extremes would not be exact, as each ",
    "$\\beta_{1,k}$ mixes all coordinates of $\\theta$)."
  ),
  paste0(
    "Intervals are exact identified-set ranges, \\emph{not} confidence ",
    "intervals. The second-stage coefficients $\\beta_{20}$ (the $Y_2$ ",
    "equation) are point-identified throughout and are not tabulated. ",
    "``unbounded'' marks an infinite range over $\\Theta$; ``unreliable'' a ",
    "failed feasibility/validity check. Quarterly ACM data, ", period,
    ", $N = ", n_obs, "$ observations."
  )
)

table_lines <- build_simple_latex_table(
  row_labels = row_labels,
  columns = list(point_col, set_col),
  col_headers = c(
    "Point ID ($\\tau = 0$)",
    paste0("Set ID ($\\tau = ", tau_disp, "$)")
  ),
  caption = caption,
  label = "tab:consumption_equation_structural",
  notes = notes,
  rule_after = p_dim
)

table_paths <- write_latex_table(
  table_lines,
  file.path(OUTPUT_PAPER_DIR, "identification"),
  "consumption_equation_structural"
)

cli_alert_success("Consumption-equation structural-coefficient table written:")
cli_ul(basename(table_paths))
cli_alert_info(
  "Outputs saved to: {.path {file.path(OUTPUT_PAPER_DIR, 'identification')}}"
)
