# Fixed-gamma transition evidence; numerical widths do not decide boundedness.
paper_source_once(paper_path("support", "identification", "coefficient_interval_tables.R"))
paper_source_once(paper_path("support", "identification", "tau_star_bracket.R"))

tau_quadratic_system <- function(gamma, tau, moments) {
  build_pipeline_quadratic_system(gamma, rep(tau, ncol(gamma)), moments)$quadratic
}

mean_quadratic_system_factory <- function(mean_eq, builder = tau_quadratic_system) {
  stopifnot(
    is.list(mean_eq), !is.null(mean_eq$gamma),
    !is.null(mean_eq$moments), is.function(builder)
  )
  gamma <- mean_eq$gamma
  moments <- mean_eq$moments
  force(builder)
  function(tau) builder(gamma, tau, moments)
}

eval_width_at_tau <- function(gamma, tau, moments) {
  system <- mean_profile_system(gamma, tau, moments)
  evidence <- paper_profile_evidence(system$quadratic, diag(ncol(gamma)), system$points)
  states <- c(evidence$summary$lower_state, evidence$summary$upper_state)
  bounded <- all(states == "bounded")
  unbounded <- any(states == "unbounded")
  status <- if (unbounded) "unbounded" else if (bounded) "bounded" else "unreliable"
  total <- if (unbounded) Inf else NA_real_
  valid <- unbounded
  if (bounded) {
    bounds <- solve_all_profile_bounds(system$quadratic, evidence = evidence)
    total <- sum(bounds$width)
    valid <- all(bounds$valid_lower & bounds$valid_upper)
  }
  list(total = total, bounded = bounded, valid = valid, status = status)
}

.sweep_row <- function(tau, w, grid_label) {
  data.frame(
    tau = tau, total_width = w$total, all_bounded = w$bounded,
    all_valid = w$valid, status = w$status, grid = grid_label, stringsAsFactors = FALSE
  )
}

sweep_fixed_gamma <- function(gamma, moments, taus, grid_label) {
  point <- mean_profile_system(gamma, 0, moments)$points
  rows <- lapply(taus, function(tau) {
    if (tau == 0) {
      known <- !is.null(point)
      w <- list(
        total = if (known) 0 else NA_real_, bounded = known, valid = known,
        status = if (known) "bounded" else "unreliable"
      )
    } else {
      w <- eval_width_at_tau(gamma, tau, moments)
    }
    .sweep_row(tau, w, grid_label)
  })
  do.call(rbind, rows)
}

fine_tau_grid <- function(
  coarse, n_fine = PAPER_INFERENCE_SEARCH_CONTROL$tau_star$fine_grid_points
) {
  unbounded <- coarse$tau[coarse$status == PAPER_ENDPOINT_STATUS[["unbounded"]]]
  hi <- if (length(unbounded)) min(unbounded) else max(coarse$tau)
  taus <- seq(0, hi, length.out = n_fine + 2L)
  taus <- taus[taus > 0 & taus < hi]
  taus[!round(taus, 10) %in% round(coarse$tau, 10)]
}
