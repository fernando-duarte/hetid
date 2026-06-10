# Grid designs for the specification / instrument / tau comparison.
# Single source of truth shared by the compute script (spec_comparison.R) and
# the reporting script (spec_comparison_report.R): the report classifies a
# result grid's coverage (full / quick subgrid / partial) by comparing the
# observed cells against these designs, so they must never drift from the
# compute loop's applicability rules.
#
# Requires common_settings.R to be sourced first (DEFAULT_ID_MATURITIES).

SPEC_COMPARISON_DESIGNS <- list(
  full = list(
    tau_grid = c(0, 0.01, 0.05, 0.1, 0.2),
    npcs_grid = 2:6,
    factor_sets = list(c(1, 2), c(1, 2, 3), c(1, 2, 3, 4)),
    mat_sets = list(DEFAULT_ID_MATURITIES, c(2), c(2, 5)),
    n_starts_opt = 12L
  ),
  quick = list(
    tau_grid = c(0, 0.05, 0.2),
    npcs_grid = c(4),
    factor_sets = list(c(1, 2, 3)),
    mat_sets = list(DEFAULT_ID_MATURITIES, c(2), c(2, 5)),
    n_starts_opt = 6L
  )
)

spec_comparison_design <- function(name = c("full", "quick")) {
  SPEC_COMPARISON_DESIGNS[[match.arg(name)]]
}

# Enumerate every (mode, n_pcs, components, gamma, tau) cell the compute loop
# attempts under a design, mirroring its applicability rules exactly:
#   - fixed "vfci" weights exist only at n_pcs = 4 (the VFCI loading),
#   - fixed "reduced_form" loadings exist only in factors mode,
#   - "optimized" and "separate" (I x J) run only at tau > 0.
spec_comparison_design_cells <- function(design) {
  groups <- rbind(
    expand.grid(
      mode = "factors", n_pcs = design$npcs_grid,
      components = vapply(design$factor_sets, paste, "", collapse = "-"),
      stringsAsFactors = FALSE
    ),
    expand.grid(
      mode = "maturities", n_pcs = design$npcs_grid,
      components = vapply(design$mat_sets, paste, "", collapse = "-"),
      stringsAsFactors = FALSE
    )
  )
  cells <- lapply(seq_len(nrow(groups)), function(i) {
    g <- groups[i, ]
    fixed <- c(if (g$n_pcs == 4) "vfci", if (g$mode == "factors") "reduced_form")
    do.call(rbind, lapply(design$tau_grid, function(tau) {
      gammas <- c(fixed, if (tau > 0) c("optimized", "separate"))
      if (!length(gammas)) {
        return(NULL)
      }
      data.frame(
        mode = g$mode, n_pcs = g$n_pcs, components = g$components,
        gamma = gammas, tau = tau, stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, cells)
}
