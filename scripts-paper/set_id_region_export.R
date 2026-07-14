# Export the b_N identified-set quadratic system (mean equation) to JSON for the
# Python region figures. For each display slack it writes the constraint
# quadratics (A_i, b_i, c_i) and the profile-bound box, plus the tau = 0 point
# and the news-PC standard deviations that put the coefficients in
# standard-deviation units. Reuses the set_id_mean_eq object, so nothing is
# re-estimated here. Consumed by plot_set_id_projections.py and
# plot_set_id_region_3d.py.
# Writes set_id_region.json to scripts-paper/output/.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")

local({
  gamma <- set_id_mean_eq$gamma
  moments <- set_id_mean_eq$moments
  # tau = 0 (point identified) plus the display slacks of the structural table
  taus <- c(0, set_id_mean_eq$tau_display)

  systems <- lapply(taus, function(tau) {
    qs <- tau_quadratic_system(gamma, tau, moments)
    bounds <- solve_all_profile_bounds(qs)
    list(
      tau = tau,
      A = lapply(qs$A_i, function(m) unname(as.matrix(m))),
      b = lapply(qs$b_i, function(v) unname(as.numeric(v))),
      c = unname(as.numeric(unlist(qs$c_i))),
      box_lower = bounds$lower, box_upper = bounds$upper
    )
  })
  names(systems) <- sprintf("tau_%.2g", taus)

  out <- list(
    coefs = set_id_mean_eq$theta_table$coef,
    taus = taus,
    point0 = unname(set_id_mean_eq$theta_table$point),
    # sd(PC_{N,i}); under the beta2R = 0 null the identification Y2 = w2 is the
    # news PC itself, so sd(w2[, i]) = sd(PC_{N,i})
    sd_pc_n = unname(apply(set_id_mean_eq$w2, 2, stats::sd)),
    systems = systems
  )
  jsonlite::write_json(
    out, file.path(out_dir, "set_id_region.json"),
    auto_unbox = TRUE, digits = 12, pretty = TRUE
  )
})

cat("set_id_region_export: wrote", file.path(out_dir, "set_id_region.json"), "\n")
