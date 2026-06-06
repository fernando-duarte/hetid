# Shared finite/Inf-aware formatters for identification bounds + width metrics.
# Four-state aware: valid=FALSE/NA -> "unreliable"; !finite -> "unbounded".

# Vector-safe: build element-wise on length(x). (A nested ifelse() collapses to
# the length of its scalar `valid` test, silently recycling row 1 -- avoid it.)
format_bound <- function(x, valid = TRUE, digits = 4) {
  valid <- rep_len(valid, length(x))
  out <- formatC(x, format = "f", digits = digits)
  out[!is.finite(x)] <- "unbounded"
  out[is.na(valid) | !valid] <- "unreliable"
  out
}

format_width <- function(w, valid = TRUE, digits = 4) {
  format_bound(w, valid = valid, digits = digits)
}

# Per-row reduction label. Distinguishes NA (failure) from Inf (unbounded),
# guards baseline_width <= 0 (e.g. the tau=0 point) and unbounded optimized, and
# renders "unreliable" when EITHER side failed its feasibility/validity check --
# so a percentage never sits beside an "unreliable" bound. Validity args default
# to TRUE (length-1) and are rep_len'd to match, mirroring format_bound.
format_reduction <- function(baseline_width, optimized_width,
                             baseline_valid = TRUE, optimized_valid = TRUE,
                             digits = 1) {
  baseline_valid <- rep_len(baseline_valid, length(baseline_width))
  optimized_valid <- rep_len(optimized_valid, length(optimized_width))
  invalid <- is.na(baseline_valid) | !baseline_valid |
    is.na(optimized_valid) | !optimized_valid
  out <- ifelse(is.na(baseline_width) | is.na(optimized_width), "n/a",
    ifelse(is.infinite(baseline_width),
      ifelse(is.finite(optimized_width),
        "baseline unbounded -> optimized bounded", "both unbounded"
      ),
      ifelse(!is.finite(optimized_width) | baseline_width <= 0, "n/a",
        paste0(formatC((baseline_width - optimized_width) /
          baseline_width * 100, format = "f", digits = digits), "%")
      )
    )
  )
  out[invalid] <- "unreliable"
  out
}

# Mean pct reduction over ONLY the finite, positive-baseline (bounded) components.
mean_pct_reduction <- function(baseline_width, optimized_width) {
  finite <- is.finite(baseline_width) & is.finite(optimized_width) &
    baseline_width > 0
  if (!any(finite)) {
    return(NA_real_)
  }
  mean((baseline_width[finite] - optimized_width[finite]) /
    baseline_width[finite] * 100)
}
