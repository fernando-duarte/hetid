#' Tau Grid with a Refined Tail
#'
#' @param tau_star Positive finite bounded endpoint of a tau bracket.
#' @param control List containing the MEAN_TAU_CONTROL settings.
#' @return Increasing numeric vector strictly between zero and tau_star. Its
#'   maximum is the last interior backbone point. No rounding is applied to keys.
#' @export

#' @examples
#' control <- MEAN_TAU_CONTROL
#' control$grid_backbone <- 6L
#' control$grid_tail_fraction <- 0.5
#' control$grid_tail_subdivisions <- 2L
#' mean_tau_grid(tau_star = 0.1, control = control)
mean_tau_grid <- function(tau_star, control = MEAN_TAU_CONTROL) {
  validate_mean_tau_control(control)
  assert_scalar_finite(tau_star, "tau_star")
  assert_bad_argument_ok(tau_star > 0, "tau_star must be positive", arg = "tau_star")
  backbone <- seq(0, tau_star, length.out = control$grid_backbone)
  backbone <- backbone[backbone > 0 & backbone < tau_star]
  tail_start <- control$grid_tail_fraction * tau_star
  below <- backbone[backbone < tail_start]
  tail_points <- backbone[backbone >= tail_start]
  assert_bad_argument_ok(length(below) >= 1L && length(tail_points) >= 1L,
    "The backbone must contain points before and after the tail threshold",
    arg = "control"
  )
  ends <- c(max(below), tail_points)
  dense <- unlist(lapply(seq_len(length(ends) - 1L), function(i) {
    seq(ends[i], ends[i + 1L], length.out = control$grid_tail_subdivisions + 1L)[-1L]
  }))
  tau_values <- sort(c(below, dense))
  assert_bad_argument_ok(
    all(diff(tau_values) > 0) &&
      all(tau_values > 0 & tau_values < tau_star) &&
      identical(max(tau_values), max(backbone)) && !anyDuplicated(profile_tau_key(tau_values)) &&
      length(tau_values) == length(below) + (length(ends) - 1L) * control$grid_tail_subdivisions,
    "Tau grid cannot be represented with distinct interior points",
    arg = "tau_star"
  )
  tau_values
}
