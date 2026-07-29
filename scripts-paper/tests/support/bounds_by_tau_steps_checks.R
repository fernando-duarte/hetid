# Shape diagnostic for the bounds-by-tau grid (bounds_by_tau_steps.R). The
# measure exists to say whether a branch switch renders as a knee or a cliff, so
# it has to separate those two shapes and refuse the degenerate inputs that
# would otherwise report a large ratio with no cliff behind it.

paper_source_once(paper_path(
  "log_variance", "figures", "bounds_by_tau_steps.R"
))

bs_rows <- function(lower, coef = "l.pc1", tau = seq_along(lower)) {
  data.frame(
    tau = tau, coef = coef, lower = lower,
    lower_status = PAPER_ENDPOINT_STATUS[["bounded"]],
    stringsAsFactors = FALSE
  )
}
bs_one <- function(...) unname(logvar_bounds_tau_steps(bs_rows(...)))

# the shape the plan recorded for the old grid: flat 0.00006 steps, then 0.0111
bs_cliff <- cumsum(c(0, rep(-0.00006, 5), -0.0111))
# the same descent once the grid resolves it
bs_knee <- cumsum(c(0, rep(-0.00006, 3), -0.0011, -0.0022, -0.0045, -0.0033))

check(
  "an unresolved cliff scores far above the threshold",
  bs_one(bs_cliff) > 50
)
check(
  "a resolved knee scores below the threshold",
  bs_one(bs_knee) < 5
)
check(
  "a uniform descent scores one",
  isTRUE(all.equal(bs_one(cumsum(c(0, rep(-0.002, 6)))), 1))
)

# Degeneracies. A repeated endpoint makes a neighbouring step exactly zero;
# dividing by it reported ratios of 1e13 on the median map before this guard.
check(
  "a repeated endpoint yields no verdict rather than a huge ratio",
  is.na(bs_one(c(1, 1, 1, 1, 1)))
)
bs_mixed <- bs_one(cumsum(c(0, -0.002, 0, -0.002, 0, -0.002, -0.002)))
check(
  "a finite ratio survives alongside repeated endpoints",
  is.finite(bs_mixed) && bs_mixed < 50
)
check(
  "too few steps yield no verdict",
  is.na(bs_one(c(1, 2, 3)))
)

# A downgraded row leaves a hole, and a difference across it spans two grid
# steps. The withdrawn taus stay in the frame carrying their downgraded status,
# which is what the renderer passes in, so the gap is visible as a gap: the jump
# from -0.004 to -0.30 across taus 4 and 5 must not be read as one huge step.
bs_hole <- bs_rows(
  lower = c(0, -0.002, -0.004, NA, NA, -0.30, -0.302, -0.304, -0.306),
  tau = 1:9
)
bs_hole$lower_status[4:5] <- PAPER_ENDPOINT_STATUS[["unreliable"]]
check(
  "a step across withdrawn taus is not counted",
  is.finite(unname(logvar_bounds_tau_steps(bs_hole))) &&
    unname(logvar_bounds_tau_steps(bs_hole)) < 5
)

# a map whose lower side never certifies has no boundary to measure
bs_unbounded <- bs_rows(c(1, 2, 3, 4))
bs_unbounded$lower_status <- PAPER_ENDPOINT_STATUS[["unbounded"]]
check(
  "an everywhere-unbounded lower side reports none, not an empty line",
  identical(
    logvar_bounds_tau_steps_report(logvar_bounds_tau_steps(bs_unbounded)),
    "none (no bounded lower side on the grid)"
  )
)
check(
  "the report names each coefficient it measured",
  grepl(
    "l.pc1",
    logvar_bounds_tau_steps_report(logvar_bounds_tau_steps(bs_rows(bs_knee)))
  )
)

rm(bs_rows, bs_one, bs_cliff, bs_knee, bs_mixed, bs_hole, bs_unbounded)
