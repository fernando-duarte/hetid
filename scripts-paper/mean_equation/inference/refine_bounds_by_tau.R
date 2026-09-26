# Full-sample and bootstrap use the same complete coefficient refinement.
paper_source_once(paper_path("mean_equation", "inference", "theta_box_multistart.R"))

set_id_display_tau_refinement_full <- function(tau_display, seed_theta,
                                               gamma, moments, beta1r, beta2r) {
  warm <- if (is.null(seed_theta) || any(!is.finite(seed_theta))) list() else list(seed_theta)
  refined <- list()
  for (tau in sort(tau_display)) {
    system <- mean_profile_system(gamma, tau, moments)
    tables <- coef_interval_tables_widened(system$quadratic, beta1r, beta2r,
      points = system$points, warm = warm
    )
    warm <- attr(tables, "profile_points")
    refined[[paper_tau_key(tau)]] <- tables
  }
  refined[vapply(tau_display, paper_tau_key, character(1))]
}

set_id_display_tau_refinement <- function(tau_display, seed_theta,
                                          gamma, moments, beta1r, beta2r) {
  lapply(set_id_display_tau_refinement_full(
    tau_display, seed_theta, gamma, moments, beta1r, beta2r
  ), `[[`, "theta")
}
