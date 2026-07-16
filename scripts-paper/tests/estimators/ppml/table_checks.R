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
ptbl_order <- function(expr) {
  exists("logvar_panel_order") && tryCatch(isTRUE(expr), error = function(e) FALSE)
}

# The editorial rule puts the PPML panel first when the tau = 0.05 crossing
# count is positive and the log-OLS panel first when it is zero
check(
  "panel order is ppml-first when the tau = 0.05 crossings exceed zero",
  ptbl_order(identical(
    logvar_panel_order(c("0.05" = 22), c(0.05)), c("ppml", "logols")
  ))
)
check(
  "panel order is logols-first when tau = 0.05 has no crossings",
  ptbl_order(identical(
    logvar_panel_order(c("0.05" = 0), c(0.05)), c("logols", "ppml")
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
    ord1 <- logvar_panel_order(n_cross, tau)
    ord2 <- logvar_panel_order(n_cross[perm], tau[perm])
    identical(ord1, ord2) && identical(ord1[[1]], "ppml")
  })
)

# A missing or duplicated tau = 0.05 baseline is a loud error, so a future
# retune cannot silently reshuffle the panels
check(
  "panel order errors when the tau = 0.05 baseline is absent",
  ptbl_order(tryCatch(
    {
      logvar_panel_order(c("0.1" = 5), c(0.1))
      FALSE
    },
    error = function(e) TRUE
  ))
)
check(
  "panel order errors when the tau = 0.05 baseline is duplicated",
  ptbl_order(tryCatch(
    {
      logvar_panel_order(c(3, 7), c(0.05, 0.05))
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
    ord_a <- logvar_panel_order(c("0.05" = 22), c(0.05))
    ord_b <- logvar_panel_order(c("0.05" = 7), c(0.05))
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
    sprintf("%.17g", c(0.05, 0.1))
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
ptbl_notes <- paste(build_ppml_table_notes(ptbl_note_fixture, 0.05, 1L, 2L), collapse = " ")
check("PPML notes model the squared-residual level, not its first difference", {
  grepl("$\\varepsilon_{t+1}^{2}=", ptbl_notes, fixed = TRUE) &&
    !grepl("$\\Delta\\varepsilon_{t+1}^{2}=", ptbl_notes, fixed = TRUE)
})

# an as_selected selector handing back an 8000-row grid reaches the scan without
# the 5000-point nearest-neighbor cap and scans in callback order
peng_fine <- function(...) {
  g <- seq(-0.99, 0.99, length.out = 130L)
  m <- as.matrix(expand.grid(g, g))
  dimnames(m) <- NULL
  m[rowSums(m^2) <= 1, , drop = FALSE][seq_len(8000L), , drop = FALSE]
}
peng_asel <- peng_sel("sel-asel", "as_selected", function(g) g)
local({
  old <- logvar_feasible_grid
  on.exit(assign("logvar_feasible_grid", old, envir = globalenv()), add = TRUE)
  ok <- peng_try({
    assign("logvar_feasible_grid", peng_fine, envir = globalenv())
    d <- peng_dummy()
    res <- peng_run(d$est, grid_selector = peng_asel)
    gg <- peng_fine()
    ord <- all(vapply(seq_len(5L), function(i) {
      isTRUE(all.equal(d$cc$order[[i]], unname(gg[i, ])))
    }, logical(1)))
    is.data.frame(res$table) && ord
  })
  check("peng as_selected 8000-row grid bypasses the NN cap in callback order", ok)
})

# Morton coverage selector: exact count, permutation invariance, an
# input-only subset, sensitivity to a new spatial cell, and callback usability
peng_mpts <- local({
  m <- matrix(runif(500L * 2L, -1, 1), 500L, 2L)
  m[rowSums(m^2) <= 1, , drop = FALSE]
})
check("peng Morton selector is exact-count, permutation-invariant, input-bound", peng_try({
  if (!exists("logvar_ppml_morton_select")) stop("absent")
  g1 <- logvar_ppml_morton_select(peng_mpts, 40L)$grid
  g2 <- logvar_ppml_morton_select(peng_mpts[sample.int(nrow(peng_mpts)), ], 40L)$grid
  g3 <- logvar_ppml_morton_select(rbind(peng_mpts, c(0.999, -0.001)), 40L)$grid
  nrow(g1) == 40L && setequal(peng_bkey(g1), peng_bkey(g2)) &&
    all(peng_bkey(g1) %in% peng_bkey(peng_mpts)) && !setequal(peng_bkey(g1), peng_bkey(g3))
}))
check("peng Morton selector works as the engine grid_selector callback", peng_try({
  if (!exists("logvar_ppml_morton_select")) stop("absent")
  res <- peng_run(peng_dummy()$est,
    grid_selector = logvar_ppml_morton_select, max_grid_points = 10L
  )
  is.data.frame(res$table) && nrow(res$table) == 2L
}))

# The plot-data canonicalizer fixes column types and orders rows by numeric
# tau, declared coefficient order, then side (lower before upper)
peng_row <- function(tau) {
  s <- data.frame(
    tau = tau, coef = c("t0", "t1"), lower = c(-1, -2), upper = c(1, 2),
    lower_status = "bounded", upper_status = "bounded", lower_provenance = "grid",
    upper_provenance = "polish", stringsAsFactors = FALSE
  )
  s$arg_lower <- I(list(c(0, 0), c(0.1, 0.1)))
  s$arg_upper <- I(list(c(0.2, 0.2), c(0.3, 0.3)))
  s
}
check("peng plot-data canonicalizer orders rows and fixes column types", peng_try({
  d <- logvar_bounds_plot_data(list(peng_row(0.1), peng_row(0.05)),
    sets = list(), sample_id = "s", spec_id = "p", output_path = "x"
  )$data
  identical(d$tau, c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1)) &&
    identical(d$coef, c("t0", "t0", "t1", "t1", "t0", "t0", "t1", "t1")) &&
    identical(d$side, rep(c("lower", "upper"), 4L)) && is.double(d$tau) &&
    is.character(d$coef) && is.double(d$value) && is.character(d$status) &&
    is.character(d$side) && is.character(d$endpoint_b)
}))
