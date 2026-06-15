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
# The lag coefficients psi_h are NO LONGER point-identified: the spec
# conditions Y2 on the SAME common X_t = (1, PC, H lags of Y1) as Y1, so beta2R
# carries nonzero lag columns and psi(theta) = psi^R - (beta2R_psi)'theta is a
# set-valued linear image of Theta under the estimate-B (default) model. Under
# imposed exact news (B = 0) those columns are zero and psi collapses to a point.

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/latex_simple_table.R"))

cli_h1("Creating Consumption-Equation Structural-Coefficient Table")

# build_reduced_form_gamma() (the Y2-on-PC slope block of beta2R) now
# lives in gamma_source.R, shared with the tau* benchmark; sourced via common_settings.

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
resid <- compute_identification_residuals(data)
stopifnot(
  isTRUE(all.equal(unname(resid$w1), unname(baseline$residuals$w1))),
  isTRUE(all.equal(unname(unclass(resid$w2)), unname(unclass(baseline$residuals$w2))))
)
# beta1R and beta2R come from INDEPENDENT full fits (existing pipeline
# behavior). The exact recovery identity beta1(theta) = beta1R - (beta2R)'theta
# assumes both are OLS on the SAME sample; here they are full-sample projections
# that may differ slightly from the common date-aligned sample (resid$dates).
# This is a PRE-EXISTING approximation, NOT introduced by the common-
# conditioning change; do not refit here. Its magnitude is to be quantified in
# the validation stage.
beta1r <- resid$w1_result$coefficients
beta2r <- resid$w2_coefficients
stopifnot(nrow(beta2r) == i_dim)

# Classify the common-design columns by NAME (not by width): the spec
# conditions BOTH Y1 and Y2 on the same X_t = (1, PC, H lags of Y1), so beta2R
# is now FULL-WIDTH and column-matched to beta1R. PCs are the only instruments
# (^pc[0-9]+$); y1_lag* columns are the predetermined conditioning lags whose
# coefficient psi_h is a set-valued linear image of Theta under estimate-B.
pc_cols <- grep("^pc[0-9]+$", names(beta1r))
lag_cols <- grep("^y1_lag[0-9]+$", names(beta1r))
n_pcs <- length(pc_cols)
n_lag <- length(lag_cols)

# beta2R is full-width and column-matched to beta1R since A1-A4 routed W2
# through the common conditioning vector. The exact recovery identity
# beta1(theta) = beta1R - (beta2R)'theta then carries every column (constant,
# PCs, AND lags) through directly; no zero-padding is needed. Fail loudly if a
# legacy PC-only beta2R is ever passed -- do NOT silently re-pad, since that
# would falsely point-identify psi.
if (!(length(beta1r) == ncol(beta2r) &&
  identical(names(beta1r), colnames(beta2r)))) {
  cli_abort(c(
    "beta2R is not column-matched to beta1R.",
    "i" = "Expected the FULL common design (1, PC, y1_lag*) in both fits.",
    "x" = paste0(
      "got beta1R names {.val {names(beta1r)}} vs beta2R columns ",
      "{.val {colnames(beta2r)}}"
    ),
    ">" = paste0(
      "This requires the common-conditioning change (A1-A4): W2 must be ",
      "regressed on the same X_t as W1."
    )
  ))
}
beta2r_full <- beta2r

# === POINT IDENTIFICATION (tau = 0, reduced-form gamma) ===
# Under imposed exact news (B = 0) the PC slope block of beta2R is all zeros, so
# build_reduced_form_gamma() is undefined (it errors) and the tau = 0 point is
# degenerate. Detect that mode and SKIP the reduced-form-gamma point block,
# routing it through the existing point_unreliable machinery. The estimate-B
# (default) path is unaffected.
impose_b_zero <- impose_news_projection_zero() ||
  all(beta2r[, pc_cols, drop = FALSE] == 0)
cond_max <- 1e12
if (impose_b_zero) {
  cli_alert_warning(paste0(
    "Imposed exact-news projection (B = 0): the reduced-form gamma is ",
    "undefined (PC slopes are zero), so the tau = 0 point column is reported ",
    "as undefined; theta stays SET-identified via the stage-05 bounds."
  ))
  pt0 <- NULL
  point_unreliable <- TRUE
  reason <- paste0(
    "the news projection is imposed exactly ($B = 0$), so the reduced-form ",
    "$\\Gamma^{R}$ is undefined and the $\\tau = 0$ point is degenerate"
  )
  theta_point <- NULL
  # Under B = 0, beta1(theta) = beta1R for ALL theta, so the beta1 rows are
  # point cells equal to beta1R; the set-column intervals below collapse to
  # [beta1R_p, beta1R_p] automatically (every c_p = 0).
  beta1_point <- beta1r
  cond_note <- reason
} else {
  gamma_rf <- build_reduced_form_gamma(beta2r)
  qs0 <- build_pipeline_quadratic_system(gamma_rf, rep(0, i_dim), moments)
  pt0 <- solve_point_identification(qs0$components)

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
    beta1_point <- recover_structural_coefficients(beta1r, beta2r_full, theta_point)
    cli_alert_info(paste0(
      "tau = 0 point identification: condition number cond(Q) = ",
      formatC(pt0$cond, format = "e", digits = 2)
    ))
    cond_note <- paste0(
      "the point-identification linear system has condition ",
      "number $\\kappa(Q) = ", latex_sci(pt0$cond), "$"
    )
  }
}

# theta is undefined whenever the reduced-form point is unreliable OR exact news
# is imposed; the beta1 rows, in contrast, ARE point-valued under imposed B = 0
# (beta1(theta) = beta1R for every theta), so they render from beta1_point.
theta_point_unreliable <- point_unreliable
beta1_point_available <- !is.null(beta1_point)

point_cell <- function(value, available = !point_unreliable) {
  if (!available || is.null(value) || !is.finite(value)) {
    return("unreliable")
  }
  formatC(value, format = "f", digits = TABLE_DIGITS)
}

# === SET IDENTIFICATION (tau = tau_set, optimized gamma) ===
qs_set <- build_pipeline_quadratic_system(gamma_opt, tau_set, moments)

# beta1 (constant + PC) coefficients: exact range over Theta of the linear
# functional beta1_p(theta) = beta1R_p - c_p' theta with c_p = beta2R[, p].
# The interval's UPPER end uses the MIN of c_p'theta and vice versa (sign flip).
p_dim <- length(beta1r)
beta1_lo <- numeric(p_dim)
beta1_hi <- numeric(p_dim)
beta1_lo_valid <- logical(p_dim)
beta1_hi_valid <- logical(p_dim)
for (p in seq_len(p_dim)) {
  c_p <- as.numeric(beta2r_full[, p])
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

# --- Assemble the rows: constant, PC1..PCn, the n_lag lagged-Y1 psi rows, then
# the I SDF-news theta rows (the beta1 functional block has p_dim rows). ---
bond_years <- lookup$bond_maturity / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
row_labels <- c(
  "Constant $\\bar{\\beta}$",
  paste0("$\\beta_{\\mathrm{PC}", seq_len(n_pcs), "}$"),
  if (n_lag > 0L) paste0("$\\psi_{", seq_len(n_lag), "}$ (lagged $Y_1$)"),
  paste0("$\\theta_{", seq_len(i_dim), "}$ (SDF news, ", bond_years, "-year)")
)
# The beta1 rows are point-valued whenever beta1_point is available -- including
# under imposed B = 0, where beta1(theta) = beta1R. The theta rows are point-
# valued only when the reduced-form tau = 0 point itself is reliable.
point_col <- c(
  vapply(
    seq_len(p_dim),
    function(p) point_cell(beta1_point[p], available = beta1_point_available),
    character(1)
  ),
  vapply(seq_len(i_dim), function(i) {
    if (theta_point_unreliable) "unreliable" else point_cell(theta_point[i])
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
# Lagged-Y1 terms in the displayed equation and an N-drop note (empty when off).
lag_eq <- if (n_lag > 0L) {
  paste0(" + \\sum_{h=1}^{", n_lag, "} \\psi_h Y_{1,t+1-h}")
} else {
  ""
}
lag_note <- if (n_lag > 0L) {
  paste0(
    " The ", n_lag, " lagged outcomes $Y_{1,t+1-h}$ enter the common ",
    "conditioning vector $X_t = (1, \\mathrm{PC}_t', Y_{1,t}, \\ldots)'$ of ",
    "\\emph{both} equations, so the lag slopes $\\psi_h$ are set-valued linear ",
    "images of $\\Theta$ (not point-identified) under the estimated-$B$ model; ",
    "lagging drops the first ", n_lag - 1L, " observations, so $N$ is the ",
    "post-lag sample."
  )
} else {
  ""
}
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
    "} \\beta_n \\mathrm{PC}_{n,t}", lag_eq, " + \\sum_{m=1}^{", i_dim,
    "} \\theta_m \\mathrm{SDFnews}_{m,t+1} + \\varepsilon_{1,t+1}$, where ",
    "$Y_1$ is consumption growth, $\\mathrm{PC}_1,\\ldots,\\mathrm{PC}_{", n_pcs,
    "}$ the principal components, and $\\mathrm{SDFnews}_m$ the ",
    "stochastic-discount-factor news of the listed bond maturities.", lag_note
  ),
  paste0(
    "The reduced-form projections $\\beta_1^{R}$ (consumption growth on the ",
    "common $X_t$) and $\\beta_2^{R}$ (each SDF-news series on the same $X_t$) ",
    "are invariant to the instrument loadings; the structural coefficients are ",
    "recovered exactly as $\\beta_1(\\theta) = \\beta_1^{R} - ",
    "(\\beta_2^{R})'\\theta$, so all identification content enters through ",
    "$\\theta$. Because $X_t$ is common to both equations, the lag rows ",
    "$\\psi_h$ obey the same map and are therefore set-valued, not point-",
    "identified."
  ),
  if (impose_b_zero) {
    paste0(
      "\\textit{Point identification} ($\\tau = 0$) is not available here ",
      "because ", cond_note, "; the $\\theta$ point column is reported as ",
      "undefined. The $\\beta_1$ rows are nonetheless point cells, since ",
      "imposing $B = 0$ gives $\\beta_1(\\theta) = \\beta_1^{R}$ for every ",
      "$\\theta$. $\\theta$ remains \\emph{set}-identified (the set column)."
    )
  } else {
    paste0(
      "\\textit{Point identification} ($\\tau = 0$) identifies $\\theta$ ",
      "from the reduced-form loadings $\\Gamma^{R}$ built from $\\beta_2^{R}$; ",
      "each moment restriction is then a perfect square, so the identified set ",
      "collapses to the single point solving the exactly-identified linear ",
      "system $Q\\theta = L$, and ", cond_note, "."
    )
  },
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
    if (impose_b_zero) {
      "Imposed exact news ($B = 0$)"
    } else {
      "Point ID ($\\tau = 0$)"
    },
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
