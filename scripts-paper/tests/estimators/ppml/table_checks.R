# Panel-ordering and coverage-output checks for the combined log-variance
# panels table. The ordering block pins logvar_panel_order(n_cross, tau), the
# mechanical panel-order helper keyed to the benchmark crossing count at the
# fixed editorial baseline tau = 0.05. The coverage block pins the
# space-filling grid_selector path the coverage gate uses: the as_selected
# large-grid bypass, the Morton coverage selector, and the figure plot-data canonicalizer.
# Sourced by test_ppml.R after engine_checks.R, whose peng_*
# fixtures (peng_try, peng_dummy, peng_run, peng_sel, peng_bkey) this file
# reuses. Red targets are guarded so their absence fails cleanly.

# guarded evaluation: FALSE unless the helper exists and expr is TRUE
ptbl_baseline <- PAPER_ANALYSIS_CONTRACT$tau$baseline
ptbl_order <- function(expr) {
  exists("logvar_panel_order") && tryCatch(isTRUE(expr), error = function(e) FALSE)
}

# The editorial rule puts the PPML panel first when the tau = 0.05 crossing
# count is positive and the log-OLS panel first when it is zero
check(
  "panel order is ppml-first when the tau = 0.05 crossings exceed zero",
  ptbl_order(identical(
    logvar_panel_order(
      c("0.05" = 22), c(0.05), ptbl_baseline
    ),
    c("ppml", "logols")
  ))
)
check(
  "panel order is logols-first when tau = 0.05 has no crossings",
  ptbl_order(identical(
    logvar_panel_order(
      c("0.05" = 0), c(0.05), ptbl_baseline
    ),
    c("logols", "ppml")
  ))
)

# The rule keys off numeric tau = 0.05, not row position: permuting the input
# rows leaves the order unchanged
check(
  "panel order is invariant to input-row permutation, keyed to tau = 0.05",
  ptbl_order({
    n_cross <- c("0.1" = 5, "0.05" = 22, "0.2" = 3)
    tau <- c(0.1, 0.05, 0.2)
    perm <- c(3L, 1L, 2L)
    ord1 <- logvar_panel_order(n_cross, tau, ptbl_baseline)
    ord2 <- logvar_panel_order(
      n_cross[perm], tau[perm], ptbl_baseline
    )
    identical(ord1, ord2) && identical(ord1[[1]], "ppml")
  })
)

# A missing or duplicated tau = 0.05 baseline is a loud error, so a future
# retune cannot silently reshuffle the panels
check(
  "panel order errors when the tau = 0.05 baseline is absent",
  ptbl_order(tryCatch(
    {
      logvar_panel_order(
        c("0.1" = 5), c(0.1), ptbl_baseline
      )
      FALSE
    },
    error = function(e) TRUE
  ))
)
check(
  "panel order errors when the tau = 0.05 baseline is duplicated",
  ptbl_order(tryCatch(
    {
      logvar_panel_order(
        c(3, 7), c(0.05, 0.05), ptbl_baseline
      )
      FALSE
    },
    error = function(e) TRUE
  ))
)

# The rule is a pure function of the crossing diagnostics: swapping in a
# different positive crossing count returns the same ids, and the return is an
# id vector (character or integer) with no numeric panel content
check(
  "panel order returns ids only, independent of the crossing magnitude",
  ptbl_order({
    ord_a <- logvar_panel_order(
      c("0.05" = 22), c(0.05), ptbl_baseline
    )
    ord_b <- logvar_panel_order(
      c("0.05" = 7), c(0.05), ptbl_baseline
    )
    identical(ord_a, ord_b) &&
      (is.character(ord_a) || is.integer(ord_a)) && length(ord_a) == 2L
  })
)

# The shared primary/panel assembly must select PPML values and exact tau keys,
# keep inference slots blank, and never substitute the mean-log benchmark.
ptbl_coef <- c("(Intercept)", "l.pc1")
ptbl_set <- function(lo, hi) {
  data.frame(
    coef = ptbl_coef, set_lower = lo, set_upper = hi,
    status = "bounded", stringsAsFactors = FALSE
  )
}
ptbl_ppml <- list(
  sample = list(n = 12L),
  table = data.frame(
    coef = ptbl_coef, reference = c(-1.3, 0.2), point = c(-1.2, 0.18),
    set_lower = c(-1.25, 0.17), set_upper = c(-1.15, 0.19),
    status = "bounded", stringsAsFactors = FALSE
  ),
  sets = stats::setNames(
    list(
      ptbl_set(c(-1.25, 0.17), c(-1.15, 0.19)),
      ptbl_set(c(-1.3, 0.16), c(-1.1, 0.2))
    ),
    vapply(c(0.05, 0.1), paper_tau_key, character(1))
  )
)
ptbl_parts <- logvar_ppml_table_parts(ptbl_ppml, c(0.05, 0.1), 1L)
check("primary table parts use PPML reference, point, and display-tau hulls", {
  identical(
    ptbl_parts$headers,
    c("OLS", "$\\tau{=}0$", "$\\tau{=}0.05$", "$\\tau{=}0.1$")
  ) &&
    ptbl_parts$rows[[1]] == "$\\theta_0$" &&
    ptbl_parts$rows[[3]] == "$\\theta_{1,R}$" &&
    ptbl_parts$columns[[1]][1] == "-1.300" &&
    ptbl_parts$columns[[2]][1] == "-1.200" &&
    ptbl_parts$columns[[3]][1] == "$[-1.250,\\,-1.150]$" &&
    ptbl_parts$columns[[4]][3] == "$[0.160,\\,0.200]$" &&
    all(vapply(ptbl_parts$columns, `[[`, character(1), length(ptbl_parts$rows) - 1L) == "--")
})
ptbl_note_fixture <- list(
  estimator = list(metadata = list(response_scale_value = 1)),
  coverage_audit = list(meta = list(grid_cap = 2L, fit_budget = 3L))
)
ptbl_notes <- paste(
  build_ppml_table_notes(
    ptbl_note_fixture,
    0.05,
    1L,
    2L,
    se_type = PAPER_REPORTING_CONTROL$ppml$se_type,
    se_hac_lags = PAPER_REPORTING_CONTROL$ppml$hac_lags
  ),
  collapse = " "
)
check("PPML notes model the squared-residual level, not its first difference", {
  grepl("$\\varepsilon_{t+1}^{2}=", ptbl_notes, fixed = TRUE) &&
    !grepl("$\\Delta\\varepsilon_{t+1}^{2}=", ptbl_notes, fixed = TRUE)
})

paper_source_once(paper_path(
  "tests", "estimators", "ppml", "table_coverage_checks.R"
))
