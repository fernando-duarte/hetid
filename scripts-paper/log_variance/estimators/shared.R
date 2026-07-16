# Cross-estimator science helpers shared by the log-OLS, PPML, Harvey and LAD
# modules and their set drivers. These are estimator-neutral utilities the
# engine does not own but every estimator needs, so they load through the
# engine's front door (engine/api.R), the single source point common to the
# pipeline foundation (log_ols/run.R) and every estimator test suite. All
# definitions only, no top-level execution; the symbols they reference
# (validators, grid helpers, the quadratic-system builder) resolve at call
# time, after the estimator modules that supply them have been sourced.

# Canonical fit-identity string: bytewise-sorted key=value records over every
# fit-changing field, each numeric rendered at full 17-digit precision so the
# stamp is deterministic and independent of options(digits); non-numerics via
# as.character. Vectors collapse with ",". Records join with a literal newline.
logvar_spec_id <- function(fields) {
  render <- function(x) {
    if (is.numeric(x)) {
      x <- formatC(x, digits = 17, format = "fg", flag = "#")
    } else {
      x <- as.character(x)
    }
    paste(x, collapse = ",")
  }
  records <- vapply(
    names(fields),
    function(k) paste0(k, "=", render(fields[[k]])),
    character(1)
  )
  paste(sort(records, method = "radix"), collapse = "\n")
}

# A schema's certified bounded endpoint args (NA args dropped): the warm-chain
# extra-start idiom shared by the set drivers and the bounds-by-tau figure.
logvar_bounded_args <- function(s) {
  a <- c(
    s$arg_lower[s$lower_status == "bounded"],
    s$arg_upper[s$upper_status == "bounded"]
  )
  a[!vapply(a, anyNA, logical(1))]
}

# The fragility line: the smallest pointwise min_t |eps_hat_t(b)| over each
# bounded endpoint arg and the search seed -- how close the attained set gets to
# a residual crossing. Returns NA when there is nothing certified to measure.
logvar_min_feasible_eps <- function(schema, w1, w2, seed) {
  args <- logvar_bounded_args(schema)
  if (!is.null(seed) && !anyNA(seed)) args <- c(args, list(seed))
  if (length(args) == 0L) {
    return(NA_real_)
  }
  min(vapply(args, function(a) min(abs(drop(w1 - w2 %*% a))), numeric(1)))
}

# One preparation path for the set-map drivers: validate the frozen inputs and
# the mean-zero PC_R convention, build the design matrix, and derive the scale
# anchor/seed base (Lewbel point, baseline quadratic system, its warm-refined
# box, unit-omega feasibility, and the coarsened baseline grid at grid_cap).
# Every piece is a pure read of the shared products, so a single up-front call
# is order-independent of the estimator-specific work interleaved around it.
logvar_prepare_map_context <- function(inputs, contract, mean_eq, bounds_tau,
                                       grid_cap) {
  v <- logvar_ppml_validate_inputs(inputs, contract)
  stopifnot(max(abs(colMeans(v$pcr))) < 1e-8)
  x_mat <- cbind(1, v$pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(v$pcr))
  b_point <- mean_eq$theta_table$point
  tau_base <- mean_eq$tau_display[1]
  qs_base <- tau_quadratic_system(mean_eq$gamma, tau_base, mean_eq$moments)
  b_tab_base <- bounds_tau[[sprintf("%.17g", tau_base)]]
  stopifnot(!is.null(b_tab_base))
  point_feasible <- !anyNA(b_point) &&
    .feasibility_residual(qs_base, b_point, rep(1, length(qs_base$A_i))) <= 0
  grid_base <- logvar_coarsen_grid(
    logvar_feasible_grid(qs_base, b_tab_base$set_lower, b_tab_base$set_upper, 41L),
    grid_cap
  )
  stopifnot(nrow(grid_base) > 0L)
  list(
    w1 = v$w1, w2 = v$w2, pcr = v$pcr, qtr = v$qtr, x_mat = x_mat,
    b_point = b_point, tau_base = tau_base, qs_base = qs_base,
    b_tab_base = b_tab_base, point_feasible = point_feasible, grid_base = grid_base
  )
}
