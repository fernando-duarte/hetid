# Shared fixtures for the post-selection report tests. FUNCTION
# BUILDERS ONLY -- no top-level constructed objects -- so sourcing
# order stays safe: worst_state (from postsel_split_utils.R) is
# resolved at CALL time, after the test file has sourced that utils
# file. Sourcing contract for consumers:
#   source("scripts/utils/postsel_split_utils.R")              # first
#   source("scripts/utils/tests/postsel_report_fixtures.R")    # then
# No test_ prefix: the scripts sweep glob (test_*.R) must never
# execute this file standalone.

# Toy raw simulation results with controlled coverage per (K, arm)
mk_results <- function(cov_map) {
  do.call(rbind, lapply(names(cov_map), function(key) {
    parts <- strsplit(key, ":", fixed = TRUE)[[1]]
    k <- as.integer(parts[1])
    n <- 50L
    n_cov <- round(cov_map[[key]] * n)
    data.frame(
      k_inst = k, phi = 0.5, rep = seq_len(n), arm = parts[2],
      covered = c(rep(TRUE, n_cov), rep(FALSE, n - n_cov)),
      sel_objective = NA_real_, eval_state = NA_character_,
      total_width = NA_real_, stringsAsFactors = FALSE
    )
  }))
}

# Grid where selection clearly degrades coverage and the split
# clearly repairs it (passes every acceptance margin)
postsel_good_results <- function() {
  mk_results(list(
    "2:fixed_full" = 0.96, "2:fixed_e" = 0.94, "2:opt_full" = 0.90,
    "2:split" = 0.92, "2:self_e" = 0.70,
    "8:fixed_full" = 0.96, "8:fixed_e" = 0.94, "8:opt_full" = 0.60,
    "8:split" = 0.92, "8:self_e" = 0.40
  ))
}

# Grid where the split misses the fixed benchmark (fails the match
# margin)
postsel_bad_results <- function() {
  mk_results(list(
    "2:fixed_full" = 0.96, "2:fixed_e" = 0.94, "2:opt_full" = 0.90,
    "2:split" = 0.70, "2:self_e" = 0.70,
    "8:fixed_full" = 0.96, "8:fixed_e" = 0.94, "8:opt_full" = 0.60,
    "8:split" = 0.50, "8:self_e" = 0.40
  ))
}

# Grid where selection degrades coverage and the split repairs it on
# the registered K <= 4 cells (passes every frozen margin with K = 4
# as the largest registered cell)
postsel_good_results_k4 <- function() {
  mk_results(list(
    "2:fixed_full" = 0.96, "2:fixed_e" = 0.94, "2:opt_full" = 0.90,
    "2:split" = 0.92, "2:self_e" = 0.70,
    "4:fixed_full" = 0.96, "4:fixed_e" = 0.94, "4:opt_full" = 0.70,
    "4:split" = 0.92, "4:self_e" = 0.50
  ))
}

# Full-simulation settings stub carrying an explicit K grid. The
# scope gate checks BOTH settings$k_grid and the realized cells in
# results$k_inst; legacy artifacts without a k_grid must fail closed
postsel_full_settings <- function(k_grid) {
  list(
    quick = FALSE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 50L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04,
    k_grid = k_grid
  )
}

# One grid row with chosen per-side states (worst_state resolved at
# call time, per the sourcing contract above)
mk_grid_row <- function(arm, weights, sel, eval_w, t_eval,
                        state_lower, state_upper, lower, upper) {
  st <- worst_state(c(state_lower, state_upper))
  data.frame(
    arm = arm, weights = weights, sel_window = sel,
    eval_window = eval_w, t_eval = t_eval, component = 1L,
    component_label = "maturity_1", lower = lower, upper = upper,
    width = upper - lower, state_lower = state_lower,
    state_upper = state_upper, state = st, system_state = st,
    total_width = if (identical(st, "bounded")) {
      upper - lower
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

# Hand-built study grid with deliberately mixed solver states
postsel_mini_grid <- function() {
  rbind(
    mk_grid_row(
      "fixed_full", "vfci_fixed", "none", "full", 241L,
      "unbounded", "bounded", -Inf, 2.1
    ),
    mk_grid_row(
      "opt_full_insample", "optimized", "full", "full", 241L,
      "bounded", "bounded", -0.8, 0.9
    ),
    mk_grid_row(
      "fixed_e", "vfci_fixed", "none", "e", 118L,
      "no-certified-bound", "bounded", NA_real_, 3.0
    ),
    mk_grid_row(
      "split_primary", "optimized", "s", "e", 118L,
      "bounded", "bounded", -1.2, 1.5
    ),
    mk_grid_row(
      "fixed_s", "vfci_fixed", "none", "s", 118L,
      "bounded", "bounded", -2, 2
    ),
    mk_grid_row(
      "split_reverse", "optimized", "e", "s", 118L,
      "bounded", "unbounded", -1.0, Inf
    ),
    mk_grid_row(
      "insample_s", "optimized", "s", "s", 118L,
      "bounded", "bounded", -0.5, 0.6
    ),
    mk_grid_row(
      "insample_e", "optimized", "e", "e", 118L,
      "bounded", "bounded", -0.4, 0.5
    )
  )
}

# Hand-built study object wrapping the mini grid
postsel_mini_study <- function() {
  list(
    settings = list(
      prop = 0.5, gap = 4L, tau = 0.2, n_starts = 30L,
      maxeval = 500L, seed = 123L
    ),
    windows = list(
      full = list(
        n_obs = 241L,
        date_range = as.Date(c("1962-06-29", "2022-09-30"))
      ),
      s = list(
        n_obs = 118L,
        date_range = as.Date(c("1962-06-29", "1991-12-31"))
      ),
      e = list(
        n_obs = 118L,
        date_range = as.Date(c("1993-03-31", "2022-09-30"))
      )
    ),
    grid = postsel_mini_grid()
  )
}
