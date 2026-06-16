# Builders for the three for_paper tables. Each returns list(lines = LaTeX
# fragment, csv = data frame, caption = title string). Captions are pdfLaTeX-safe
# (math in $...$, escaped underscores); the bottom-line title is computed from
# the result. Consumed by render_paper_tables.R.

.fmt <- function(x, d = 4) formatC(x, format = "f", digits = d)

.sumstat <- function(x) {
  x <- x[is.finite(x)]
  ac1 <- if (length(x) > 2L) stats::acf(x, lag.max = 1L, plot = FALSE)$acf[2L] else NA_real_
  c(
    N = length(x), Mean = mean(x), SD = stats::sd(x), Min = min(x),
    Median = stats::median(x), Max = max(x), AC1 = ac1
  )
}

# Identified-set interval cell as a pdfLaTeX-safe string.
.interval <- function(lo, hi, bounded, valid, d = 4) {
  if (!isTRUE(valid)) {
    return("unreliable")
  }
  if (!isTRUE(bounded) || !is.finite(lo) || !is.finite(hi)) {
    return("unbounded")
  }
  paste0("$[", .fmt(lo, d), ",\\,", .fmt(hi, d), "]$")
}

# Quarter labels via the shared year_quarter() (format_utils.R).
.qq <- year_quarter

# ---- Table 1: summary statistics of the set-identification variables ----
build_table1_summary <- function(res) {
  r <- res$resid
  d <- res$resid$design_aligned
  series <- list(
    "$\\Delta c$ (consumption growth)" = r$y1_level,
    "$\\mathrm{PC}_1$" = d[, "pc1"], "$\\mathrm{PC}_2$" = d[, "pc2"],
    "$\\mathrm{PC}_3$" = d[, "pc3"], "$\\mathrm{PC}_4$" = d[, "pc4"],
    "$\\Delta c_{t-1}$" = d[, "l.y1"], "$\\Delta c_{t-2}$" = d[, "l2.y1"],
    "$\\Delta c_{t-3}$" = d[, "l3.y1"], "$\\Delta c_{t-4}$" = d[, "l4.y1"],
    "$Y_2$ (SDF-news PC)" = r$y2, "$W_2$ (news residual)" = r$w2,
    "$Z$ (de-meaned VFCI)" = r$z
  )
  stats <- t(vapply(series, .sumstat, numeric(7)))
  cols <- list(
    as.character(stats[, "N"]), .fmt(stats[, "Mean"], 3), .fmt(stats[, "SD"], 3),
    .fmt(stats[, "Min"], 3), .fmt(stats[, "Median"], 3), .fmt(stats[, "Max"], 3),
    .fmt(stats[, "AC1"], 2)
  )
  span <- paste0(.qq(min(r$dates)), "--", .qq(max(r$dates)))
  ve <- round(100 * res$est$pc_var_explained, 1)
  nm <- res$spec$n_news_maturities
  rng <- res$spec$news_maturity_range
  caption <- paste0(
    "Stationary, weakly persistent quarterly series of the single-instrument ",
    "heteroskedasticity identification, ", span, " ($N=", r$n_obs, "$)."
  )
  notes <- c(
    sprintf(
      "Quarterly, %s ($N=%d$). $\\Delta c$ = log growth of real PCE (FRED pcecc96).",
      span, r$n_obs
    ),
    "$\\mathrm{PC}_1$--$\\mathrm{PC}_4$ are principal components of a cross-section",
    "of financial-asset returns (not yields), entered predetermined (date $t$) in",
    "the date-$t{+}1$ equations. $\\Delta c_{t-1}$--$\\Delta c_{t-4}$ are four",
    "consumption-growth lags. $Y_2$ is the first principal component (correlation",
    sprintf(
      "PCA) of the SDF news across %d maturities (%d--%d months), explaining %.1f\\%%",
      nm, rng[1], rng[2], ve
    ),
    "of the standardized news cross-section; the per-maturity loadings are in the",
    "companion CSV. $W_2$ is $Y_2$ residualized on $X_t=(1,\\mathrm{PC}_{1:4},",
    "\\Delta c_{t-1:t-4})$; under correlation PCA $Y_2$ (and hence $\\theta$) is in",
    "units of one pooled SDF-news standard deviation. $Z=\\mathrm{VFCI}-",
    "\\overline{\\mathrm{VFCI}}$; VFCI is the Volatility Financial Conditions Index",
    "of Adrian, DeHaven, Duarte and Iyer. AC(1) is the first-order autocorrelation."
  )
  lines <- build_simple_latex_table(
    row_labels = names(series), columns = cols,
    col_headers = c("$N$", "Mean", "SD", "Min", "Median", "Max", "AC(1)"),
    caption = caption, label = "tab:paper_summary_statistics",
    notes = notes, stub = "Variable", rule_after = c(9L, 11L)
  )
  csv <- data.frame(
    variable = names(series), stats,
    loading = NA_real_, weight = NA_real_, row.names = NULL
  )
  load_csv <- data.frame(
    variable = paste0("news_load_", res$resid$used_maturities, "m"),
    N = NA, Mean = NA, SD = NA, Min = NA, Median = NA, Max = NA, AC1 = NA,
    loading = unname(res$resid$news_loadings),
    weight = unname(res$resid$news_weights), row.names = NULL
  )
  list(lines = lines, csv = rbind(csv, load_csv), caption = caption)
}

# ---- Table 2: the structural price-of-risk equation ----
build_table2_structural <- function(res) {
  e <- res$est
  ct <- e$coef_table
  labs <- c(
    "(Intercept)" = "$\\bar\\beta$",
    pc1 = "$\\beta_{\\mathrm{PC}1}$", pc2 = "$\\beta_{\\mathrm{PC}2}$",
    pc3 = "$\\beta_{\\mathrm{PC}3}$", pc4 = "$\\beta_{\\mathrm{PC}4}$",
    l.y1 = "$\\psi_1$", l2.y1 = "$\\psi_2$", l3.y1 = "$\\psi_3$", l4.y1 = "$\\psi_4$",
    theta = "$\\theta$"
  )
  dg <- 2L # report two decimals throughout
  stars <- function(p) {
    if (!is.finite(p)) {
      return("")
    }
    if (p < 0.01) "$^{***}$" else if (p < 0.05) "$^{**}$" else if (p < 0.10) "$^{*}$" else ""
  }
  # One OLS column from a fitted spec: estimate (2dp) + HAC stars per coef row,
  # "--" for coefficients absent from that specification.
  spec_col <- function(s) {
    cells <- vapply(ct$coef, function(cn) {
      key <- if (cn == "theta") ".y2" else cn
      if (!(key %in% names(s$coef))) {
        return("--")
      }
      paste0(.fmt(unname(s$coef[key]), dg), stars(unname(s$p[key])))
    }, character(1))
    c(unname(cells), .fmt(s$r2, dg), as.character(e$n_obs)) # R^2 and N rows
  }
  setcol <- function(lo, hi, bnd, vld) {
    c(vapply(
      seq_along(lo), function(i) .interval(lo[i], hi[i], bnd[i], vld[i], dg),
      character(1)
    ), "--", as.character(e$n_obs)) # R^2 = "--", N rows
  }
  ols_columns <- lapply(e$ols_specs, spec_col)
  set05 <- setcol(ct$set_lower, ct$set_upper, ct$bounded, ct$valid)
  set50 <- setcol(ct$set50_lower, ct$set50_upper, ct$set50_bounded, ct$set50_valid)
  row_labels <- c(unname(labs[ct$coef]), "$R^2$", "$N$")

  span <- paste0(.qq(min(e$dates)), "--", .qq(max(e$dates)))
  th <- ct[ct$coef == "theta", ]
  boot_up <- res$boot$upper["p95"]
  fragile <- !is.finite(boot_up) || boot_up >= 0
  title <- if (e$set_status == "interval") {
    sprintf(
      paste0(
        "Heteroskedasticity set-identifies the price of SDF-news risk to ",
        "$\\theta\\in[%s,%s]$ at $\\tau=0.05$%s."
      ),
      .fmt(th$set_lower, dg), .fmt(th$set_upper, dg),
      if (fragile) "; the bootstrap bounds are imprecise (cross zero)" else ""
    )
  } else if (e$set_status == "unbounded") {
    "The identified set for the price of SDF-news risk $\\theta$ is unbounded at $\\tau=0.05$ (weak identification)."
  } else if (e$set_status == "empty") {
    "No $\\theta$ satisfies the estimated restrictions at $\\tau=0.05$ (estimated set empty)."
  } else {
    "The price of SDF-news risk $\\theta$ is not reliably identified at $\\tau=0.05$."
  }
  notes <- c(
    "Coefficients of the structural consumption-growth (Euler) equation",
    "$\\Delta c_{t+1}=\\bar\\beta+\\sum_k\\beta_{\\mathrm{PC}k}\\mathrm{PC}_{k,t}+",
    "\\sum_h\\psi_h\\Delta c_{t+1-h}+\\theta\\,Y_{2,t+1}+\\varepsilon_{1,t+1}$,",
    "with the single SDF-news PC $Y_2$ instrumented by the de-meaned VFCI $Z$",
    "via Lewbel (2012) heteroskedasticity:",
    "$\\mathrm{Cov}(Z,\\varepsilon_1\\varepsilon_2)=\\tau\\,\\mathrm{Var}(\\varepsilon_2\\mid Z)$.",
    "OLS columns (treating $Y_2$ as exogenous, all on the same sample):",
    "(1) with $Y_2$, no consumption lags; (2) without $Y_2$, no lags;",
    "(3) with $Y_2$, one lag; (4) without $Y_2$, one lag;",
    "(5) with $Y_2$, four lags; (6) without $Y_2$, four lags.",
    sprintf(paste0(
      "Stars $^{*}/^{**}/^{***}$ denote significance at $10/5/1\\%%$ using ",
      "Newey--West HAC standard errors (%d lags); $R^2$ is the OLS fit; ``--'' ",
      "marks a coefficient not in that specification."
    ), e$nw_lag),
    "The last two columns give the exact identified set for each coefficient at",
    "$\\tau=0.05$ and $\\tau=0.5$ (baseline four-lag design); non-$\\theta$",
    "coefficients are recovered as",
    "$\\beta_1(\\theta)=\\beta_1^{R}-(\\beta_2^{R})'\\theta$ and bounded over the set.",
    sprintf(
      "Sample %s. These cells are exact identified-set ranges, not confidence",
      span
    ),
    "intervals; sampling uncertainty is characterized by the bootstrap band in",
    "Table 3. $\\theta$ is in units of one pooled SDF-news standard deviation."
  )
  lines <- build_simple_latex_table(
    row_labels = row_labels,
    columns = c(ols_columns, list(set05, set50)),
    col_headers = c(
      "(1)", "(2)", "(3)", "(4)", "(5)", "(6)",
      "$\\tau{=}0.05$", "$\\tau{=}0.5$"
    ),
    caption = title, label = "tab:paper_structural_equation",
    notes = notes, stub = "", rule_after = c(nrow(ct) - 1L, nrow(ct)),
    fontsize = "\\small\\setlength{\\tabcolsep}{4pt}",
    spanners = list(
      list(label = "OLS", n = 6L),
      list(
        label = "\\shortstack{Identification through\\\\heteroskedasticity}",
        n = 2L
      )
    )
  )
  # CSV: one row per coefficient (+ an R2 row), every spec's estimate and HAC
  # p-value, and the two identified-set columns.
  spec_keys <- c(
    "with_y2_0lag", "no_y2_0lag", "with_y2_1lag",
    "no_y2_1lag", "with_y2_4lag", "no_y2_4lag"
  )
  csv <- data.frame(coefficient = ct$coef, stringsAsFactors = FALSE)
  for (j in seq_along(e$ols_specs)) {
    s <- e$ols_specs[[j]]
    pick <- function(tab) {
      vapply(ct$coef, function(cn) {
        key <- if (cn == "theta") ".y2" else cn
        if (key %in% names(tab)) unname(tab[key]) else NA_real_
      }, numeric(1))
    }
    csv[[paste0(spec_keys[j], "_est")]] <- pick(s$coef)
    csv[[paste0(spec_keys[j], "_p")]] <- pick(s$p)
  }
  csv$set05_lower <- ct$set_lower
  csv$set05_upper <- ct$set_upper
  csv$set50_lower <- ct$set50_lower
  csv$set50_upper <- ct$set50_upper
  r2row <- csv[1, ]
  r2row[] <- NA
  r2row$coefficient <- "R2"
  for (j in seq_along(e$ols_specs)) r2row[[paste0(spec_keys[j], "_est")]] <- e$ols_specs[[j]]$r2
  csv <- rbind(csv, r2row)
  list(lines = lines, csv = csv, caption = title, landscape = TRUE)
}

# ---- Table 3: estimator properties (relevance + strength), with bootstrap ----
build_table3_properties <- function(res) {
  e <- res$est
  b <- res$boot
  rel <- e$relevance
  pv <- e$hetero_pvals
  sig <- e$significance_level
  pcell <- function(nm) if (nm %in% names(pv) && is.finite(pv[[nm]])) .fmt(pv[[nm]], 3) else "--"
  band <- function(x) sprintf("$[%s,\\,%s]$", .fmt(x["p05"], 3), .fmt(x["p95"], 3))
  span <- paste0(.qq(min(e$dates)), "--", .qq(max(e$dates)))
  snr <- rel$cov_z_w2sq / b$cov_se

  rows <- c(
    "White ($p$)", "Breusch--Pagan ($p$)", "Goldfeld--Quandt ($p$)",
    "Harvey ($p$)", "Glejser ($p$)", "Breusch--Pagan LM ($p$)", "ARCH(1) ($p$)",
    "$\\mathrm{Cov}(Z,W_2^2)$ (SNR)", "$\\mathrm{corr}(Z,W_2^2)$",
    "$W_2$-on-$Z$ mean slope $t$", "$\\mathrm{corr}(W_1,W_2)$ (endogeneity)",
    "$\\tau^\\ast$ (bootstrap 90\\% band)", "$\\Theta$ width at $\\tau=0.05$",
    "Identified-set status", "First-stage $R^2$ ($W_1$)",
    "$r^2_{Y_2}$ (news PC absorbed by $X_t$)", "PC1 variance explained",
    "Observations $N$", "Sample"
  )
  vals <- c(
    pcell("White"), pcell("BP"), pcell("GQ"), pcell("Harvey"),
    pcell("Glejser"), pcell("BPLM"), pcell("ARCH"),
    sprintf("%s (%.2f)", .fmt(rel$cov_z_w2sq, 3), snr), .fmt(rel$cor_z_w2sq, 3),
    .fmt(rel$mean_slope_t, 3), .fmt(rel$cor_w1_w2, 3),
    sprintf("%s %s", .fmt(e$tau_star, 3), band(b$tau_star)),
    if (is.finite(e$width)) .fmt(e$width, 4) else "unbounded",
    e$set_status, .fmt(e$r2_w1, 3), .fmt(e$r2_y2, 3),
    sprintf("%.1f\\%%", 100 * e$pc_var_explained), as.character(e$n_obs), span
  )
  hetero_reject <- isTRUE(pv[["BP"]] < sig) || isTRUE(pv[["ARCH"]] < sig) ||
    isTRUE(pv[["GQ"]] < sig)
  title <- sprintf(
    paste0(
      "VFCI %s conditional heteroskedasticity in the news residual ",
      "(relevance), and the price of risk stays bounded up to $\\tau^\\ast=%s$, ",
      "but the bootstrap shows the identifying moment is imprecise (SNR$\\approx%.1f$)."
    ),
    if (hetero_reject) "exhibits significant" else "shows weak",
    .fmt(e$tau_star, 2), snr
  )
  notes <- c(
    "Lewbel (2012) identification needs two conditions. Relevance:",
    "$\\mathrm{Cov}(Z,\\varepsilon_2^2)\\neq0$ -- testable; the heteroskedasticity",
    "tests (null: homoskedasticity of $W_2$ given $Z$) bear on this. The identifying",
    "restriction $\\mathrm{Cov}(Z,\\varepsilon_1\\varepsilon_2)=\\tau\\,",
    "\\mathrm{Var}(\\varepsilon_2\\mid Z)$ is untestable; $\\tau$ is the slack that",
    "substitutes for it and, with one instrument, the system is just-identified (no",
    "over-identification test). $\\tau^\\ast$ is the largest common slack keeping the",
    "set bounded (scale-free). In this sample VFCI is exactly a linear combination of",
    "$\\mathrm{PC}_{1:4}$, so $Z\\in\\mathrm{span}(X_t)$ and its $W_2$-mean slope is",
    "$\\approx0$ by construction -- the heteroskedasticity tests are uncontaminated by",
    "a mean effect. Bands are 5th--95th percentiles of a moving-block bootstrap",
    sprintf("($B=%d$, block $=%d$ quarters). $N=%d$, %s.", b$b_reps, b$block, e$n_obs, span)
  )
  lines <- build_simple_latex_table(
    row_labels = rows, columns = list(vals),
    col_headers = "Value", caption = title,
    label = "tab:paper_estimator_properties", notes = notes,
    stub = "Property", rule_after = 11L
  )
  csv <- data.frame(property = rows, value = vals, row.names = NULL)
  list(lines = lines, csv = csv, caption = title)
}
