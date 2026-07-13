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
      return(c(tau_star = NA_real_, lower = NA_real_, upper = NA_real_, cov = NA_real_))
    }
    coarse <- tryCatch(sweep_fixed_gamma(gamma, mom, taus, "coarse"), error = function(e) NULL)
    ts <- if (is.null(coarse)) {
      NA_real_
    } else {
      tryCatch(tau_star_fixed(gamma, mom, coarse)$tau_star, error = function(e) NA_real_)
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
      tau_star = ts,
      lower = if (is.null(bnd)) NA_real_ else bnd$lower[1],
      upper = if (is.null(bnd)) NA_real_ else bnd$upper[1],
      cov = mean(zb * wb^2) - mean(zb) * mean(wb^2)
    )
  }

  draws <- t(vapply(
    seq_len(b_reps),
    function(b) one(mbb_index(n, block)),
    numeric(4)
  ))
  colnames(draws) <- c("tau_star", "lower", "upper", "cov")

  list(
    draws = draws, b_reps = b_reps, block = block,
    tau_star = boot_band(draws[, "tau_star"]),
    lower = boot_band(draws[, "lower"]),
    upper = boot_band(draws[, "upper"]),
    cov_z_w2sq = boot_band(draws[, "cov"]),
    cov_se = stats::sd(draws[, "cov"], na.rm = TRUE)
  )
}
