# Moving-block bootstrap band for the paper spec (I = 1, J = 1). Resamples
# quarters in moving blocks, recomputes the moments under the fixed unit gamma,
# and re-derives tau*, the set endpoints at tau_set, and the heteroskedasticity-
# relevance moment Cov(Z, W2^2). Deterministic given seed. Reuses the
# results-companion block-bootstrap design (block = 15, 200 reps).

source("scripts/utils/set_id_inference.R")

compute_paper_spec_bootstrap <- function(resid, tau_set = BASELINE_TAU,
                                         b_reps = 200L, block = 15L, seed = SEED) {
  set.seed(seed)
  w1 <- resid$w1
  w2v <- as.numeric(resid$w2)
  zv <- as.numeric(resid$z)
  n <- length(w1)
  gamma <- matrix(1, 1, 1)
  taus <- seq(0, OPT_TAU_CAP, by = 0.005)

  one <- function(idx) {
    mom <- tryCatch(
      compute_identification_moments(
        w1[idx], matrix(w2v[idx], ncol = 1),
        matrix(zv[idx], ncol = 1, dimnames = list(NULL, "vfci_dm"))
      ),
      error = function(e) NULL
    )
    if (is.null(mom)) {
      return(c(
        tau_star = NA_real_, lower = NA_real_, upper = NA_real_,
        cov = NA_real_, capped = NA_real_
      ))
    }
    coarse <- tryCatch(sweep_fixed_gamma(gamma, mom, taus, "coarse"), error = function(e) NULL)
    # Keep capped alongside tau_star: a capped draw ran off the top of the grid
    # without finding a transition, so its tau_star is a censored lower bound at
    # the cap, not a located tau*. The band renderer needs to tell the two apart.
    tsr <- if (is.null(coarse)) {
      list(tau_star = NA_real_, capped = NA)
    } else {
      tryCatch(
        tau_star_fixed(gamma, mom, coarse)[c("tau_star", "capped")],
        error = function(e) list(tau_star = NA_real_, capped = NA)
      )
    }
    bnd <- tryCatch(
      solve_all_profile_bounds(
        build_pipeline_quadratic_system(gamma, tau_set, mom)$quadratic
      ),
      error = function(e) NULL
    )
    zb <- zv[idx]
    wb <- w2v[idx]
    c(
      tau_star = tsr$tau_star,
      lower = if (is.null(bnd)) NA_real_ else bnd$lower[1],
      upper = if (is.null(bnd)) NA_real_ else bnd$upper[1],
      cov = mean(zb * wb^2) - mean(zb) * mean(wb^2),
      capped = as.numeric(tsr$capped)
    )
  }

  draws <- t(vapply(
    seq_len(b_reps),
    function(b) one(mbb_index(n, block)),
    numeric(5)
  ))
  colnames(draws) <- c("tau_star", "lower", "upper", "cov", "capped")
  # Count the censored draws (mirrors set_id_bootstrap_core's n_capped): the
  # table cannot read the tau_star band as a sampling interval when the draws
  # are censored lower bounds rather than located transitions. capped is
  # aggregate metadata about tau_star, so it stays a scalar count and out of the
  # public draws matrix, which carries only the four band quantities.
  n_capped <- sum(draws[, "capped"] == 1, na.rm = TRUE)

  list(
    draws = draws[, c("tau_star", "lower", "upper", "cov"), drop = FALSE],
    b_reps = b_reps, block = block,
    tau_star = boot_band(draws[, "tau_star"]),
    lower = boot_band(draws[, "lower"]),
    upper = boot_band(draws[, "upper"]),
    cov_z_w2sq = boot_band(draws[, "cov"]),
    cov_se = stats::sd(draws[, "cov"], na.rm = TRUE),
    n_capped = n_capped
  )
}
