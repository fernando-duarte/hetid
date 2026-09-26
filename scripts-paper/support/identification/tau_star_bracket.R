# The legacy scalar is a conservative plotting cap, never an unresolved midpoint.
tau_star_result <- function(lower, upper, trace, coarse, inconclusive) {
  sweep_max <- max(coarse$tau)
  capped <- is.na(upper) && identical(lower, sweep_max)
  state <- if (capped) {
    "capped"
  } else if (is.na(upper)) {
    "unresolved_above"
  } else if (lower == 0) {
    "unresolved_below"
  } else if (length(inconclusive)) {
    "unresolved_band"
  } else {
    "bracketed"
  }
  list(
    tau_star = lower, trace = trace, capped = capped,
    bracket = list(
      lower = lower, upper = upper, status = state,
      inconclusive = sort(unique(inconclusive)), sweep_max = sweep_max
    )
  )
}

tau_star_fixed <- function(
  gamma, moments, coarse,
  iters = PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bisection_iterations
) {
  bounded <- coarse$tau[coarse$status == PAPER_ENDPOINT_STATUS[["bounded"]]]
  unbounded <- coarse$tau[coarse$status == PAPER_ENDPOINT_STATUS[["unbounded"]]]
  unknown <- coarse$tau[coarse$status == PAPER_ENDPOINT_STATUS[["unreliable"]]]
  lo <- if (length(bounded)) max(bounded) else 0
  hi <- if (length(unbounded)) min(unbounded) else NA_real_
  if (is.na(hi)) {
    return(tau_star_result(lo, hi, NULL, coarse, unknown[unknown > lo]))
  }
  stopifnot(lo < hi)
  trace <- list()
  for (iteration in seq_len(iters)) {
    mid <- (lo + hi) / 2
    if (mid == lo || mid == hi) break
    value <- eval_width_at_tau(gamma, mid, moments)
    trace[[length(trace) + 1L]] <- .sweep_row(mid, value, "bisection")
    if (identical(value$status, PAPER_ENDPOINT_STATUS[["bounded"]])) {
      lo <- mid
    } else if (identical(value$status, PAPER_ENDPOINT_STATUS[["unbounded"]])) {
      hi <- mid
    } else {
      unknown <- c(unknown, mid)
      break
    }
  }
  tau_star_result(lo, hi, do.call(rbind, trace), coarse, unknown[unknown > lo & unknown < hi])
}

format_tau_star_bracket <- function(bracket, digits = 4L) {
  format_side <- function(value, upper = FALSE) {
    if (value == 0) {
      return("0")
    }
    step <- 10^(floor(log10(abs(value))) - digits + 1)
    scaled <- value / step
    rounded <- (if (upper) ceiling(scaled) else floor(scaled)) * step
    format(rounded, digits = digits, trim = TRUE)
  }
  lower <- format_side(bracket$lower)
  if (is.na(bracket$upper)) {
    paste0(
      "tau* >= ", lower, " (", bracket$status, "; sweep max ",
      format(signif(bracket$sweep_max, digits), trim = TRUE), ")"
    )
  } else {
    paste0(
      "tau* in [", lower, ", ", format_side(bracket$upper, upper = TRUE),
      "] (", bracket$status, ")"
    )
  }
}
