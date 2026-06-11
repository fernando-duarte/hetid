# Post-Selection Split Utilities -- contiguous temporal blocks for
# selection-honest weight choice: select width-minimizing weights on
# one block, evaluate the identified set on the other. NOT sourced by
# common_settings.R: the post-selection study scripts and their tests
# source this file directly, so the default pipeline never loads it.
#
# Honesty contract (docs/lewbel_multivariate_set_identification.tex:
# theta_0 lies in Theta(Lambda, tau) "for any fixed, ex ante
# admissible Lambda"; a data-selected Lambda needs the bound to hold
# UNIFORMLY over the admissible class): weights selected on the
# selection block are fixed RELATIVE TO the evaluation block, so the
# evaluated bound's validity argument needs no uniformity over the
# admissibility class. Two caveats remain and must travel with every
# artifact: serial dependence across the block boundary makes "fixed
# relative to" approximate (the gap attenuates, never removes it),
# and the maintained bound must hold AT the selected weights -- a
# population property the data cannot certify.

# Deterministic contiguous block partition of n_rows data rows into a
# selection block, excluded gap rows, and an evaluation block. No RNG.
split_block_rows <- function(n_rows, prop = 0.5, gap = 4L) {
  if (!is.numeric(n_rows) || length(n_rows) != 1L) {
    stop("split_block_rows: n_rows must be a single number")
  }
  if (!is.numeric(prop) || length(prop) != 1L ||
    prop <= 0 || prop >= 1) {
    stop("split_block_rows: prop must be a single number in (0, 1)")
  }
  if (!is.numeric(gap) || length(gap) != 1L || gap < 0) {
    stop("split_block_rows: gap must be a single non-negative number")
  }
  n_rows <- as.integer(n_rows)
  gap <- as.integer(gap)
  usable <- n_rows - gap
  n_s <- as.integer(floor(usable * prop))
  n_e <- usable - n_s
  if (n_s < 2L || n_e < 2L) {
    stop(
      "split_block_rows: blocks too small (selection ", n_s,
      ", evaluation ", n_e, ") for n_rows = ", n_rows,
      " with gap = ", gap
    )
  }
  list(
    s_rows = seq_len(n_s),
    gap_rows = if (gap > 0L) {
      seq.int(n_s + 1L, n_s + gap)
    } else {
      integer(0)
    },
    e_rows = seq.int(n_s + gap + 1L, n_rows)
  )
}

# Residuals + moments for one contiguous block of the merged
# quarterly data frame. The first stage is REFIT inside the block, so
# the selection event never touches evaluation-block data, not even
# through estimated reduced-form coefficients.
block_moments <- function(data, rows,
                          maturities = DEFAULT_ID_MATURITIES,
                          n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS) {
  block <- data[rows, , drop = FALSE]
  resid <- compute_identification_residuals(
    block,
    maturities = maturities, n_pcs = n_pcs
  )
  list(
    moments = compute_identification_moments(
      resid$w1, resid$w2, resid$pcs_aligned
    ),
    n_obs = resid$n_obs,
    date_range = if ("date" %in% names(block)) {
      range(block$date)
    } else {
      NULL
    }
  )
}

# House three-state vocabulary per profile side, mapped from the
# solver's (bounded, valid) pair and never collapsed downstream:
#   bounded             solver-certified finite bound
#   unbounded           solver-certified unbounded direction
#   no-certified-bound  fail-closed solver outcome (no claim made)
side_state <- function(bounded, valid) {
  ifelse(
    bounded & valid, "bounded",
    ifelse(!bounded & valid, "unbounded", "no-certified-bound")
  )
}

# Worst state of a collection under fail-closed precedence:
# no-certified-bound > unbounded > bounded. Summaries use this; the
# per-side states stay in the table so nothing is collapsed away.
worst_state <- function(states) {
  for (s in c("no-certified-bound", "unbounded", "bounded")) {
    if (s %in% states) {
      return(s)
    }
  }
  stop("worst_state: no recognized state in input")
}

# Evaluate FIXED weights on a moments container: build the general
# quadratic system, solve every profile bound, attach per-side and
# per-component states, and report the honest total width (a number
# only when every side of every component is solver-certified
# bounded; NA otherwise -- the states say why).
evaluate_lambda_set <- function(lambda, tau, moments) {
  qs <- hetid::build_general_quadratic_system(lambda, tau, moments)
  bounds <- solve_all_profile_bounds(qs$quadratic)
  bounds$state_lower <- side_state(
    bounds$bounded_lower, bounds$valid_lower
  )
  bounds$state_upper <- side_state(
    bounds$bounded_upper, bounds$valid_upper
  )
  bounds$state <- vapply(
    seq_len(nrow(bounds)),
    function(k) {
      worst_state(c(bounds$state_lower[k], bounds$state_upper[k]))
    },
    character(1)
  )
  system_state <- worst_state(bounds$state)
  list(
    bounds = bounds,
    system_state = system_state,
    total_width = if (identical(system_state, "bounded")) {
      sum(bounds$width)
    } else {
      NA_real_
    },
    n_constraints = nrow(qs$labels)
  )
}

# Long-format rows (one per component) for one evaluated arm. Every
# per-side state is carried so downstream reporting can never
# collapse the three-state vocabulary.
arm_rows <- function(arm, weights, sel_window, eval_window, t_eval,
                     evaluation, component_labels) {
  b <- evaluation$bounds
  data.frame(
    arm = arm, weights = weights, sel_window = sel_window,
    eval_window = eval_window, t_eval = t_eval,
    component = b$component, component_label = component_labels,
    lower = b$lower, upper = b$upper, width = b$width,
    state_lower = b$state_lower, state_upper = b$state_upper,
    state = b$state, system_state = evaluation$system_state,
    total_width = evaluation$total_width,
    stringsAsFactors = FALSE
  )
}
