# Plot-frame construction for the log-variance bounds-by-tau figure: the
# per-row endpoint-status category, the finite side and divergence direction of
# one-sided rows, and the per-facet status strip. Split from bounds_by_tau_plot.R
# for the repository line cap. Definitions only; sourced there.

# Strip tiles take a per-row width from local tau spacing rather than one fixed
# width: the grid is refined near tau*, so a width tuned for the uniform
# backbone would span several refined steps and let a neighbouring tile paint
# over an unreliable or one-sided status the strip exists to disclose. A global
# min(diff(tau)) is wrong instead -- display taus sit within 0.0018 of grid
# points, which would draw hairlines everywhere.
logvar_bounds_tau_strip <- function(rows, tau_star) {
  default_width <- tau_star / 30
  strip <- do.call(rbind, lapply(levels(rows$coef), function(cf) {
    sub <- rows[rows$coef == cf, ]
    vals <- c(
      sub$lower[is.finite(sub$lower)], sub$upper[is.finite(sub$upper)],
      sub$finite_side[is.finite(sub$finite_side)]
    )
    ymin <- if (length(vals)) min(vals) else 0
    rng <- max(1e-4, if (length(vals)) diff(range(vals)) else 0)
    tau_u <- sort(unique(sub$tau))
    gap <- if (length(tau_u) > 1L) {
      d <- diff(tau_u)
      pmin(c(d, Inf), c(Inf, d))
    } else {
      default_width
    }
    data.frame(
      coef = cf, tau = sub$tau, category = sub$category,
      y = ymin - 0.06 * rng, h = 0.04 * rng,
      w = pmin(default_width, 0.9 * gap[match(sub$tau, tau_u)])
    )
  }))
  strip$coef <- factor(strip$coef, levels = levels(rows$coef))
  strip
}

logvar_bounds_tau_frame <- function(rows, tau_star) {
  rows$category <- ifelse(
    rows$lower_status == PAPER_ENDPOINT_STATUS[["bounded"]] &
      rows$upper_status == PAPER_ENDPOINT_STATUS[["bounded"]],
    "two-sided",
    ifelse(
      (rows$lower_status == PAPER_ENDPOINT_STATUS[["bounded"]] &
        rows$upper_status == PAPER_ENDPOINT_STATUS[["unbounded"]]) |
        (rows$upper_status == PAPER_ENDPOINT_STATUS[["bounded"]] &
          rows$lower_status == PAPER_ENDPOINT_STATUS[["unbounded"]]),
      "one-sided",
      ifelse(
        rows$lower_status == PAPER_ENDPOINT_STATUS[["unbounded"]] &
          rows$upper_status == PAPER_ENDPOINT_STATUS[["unbounded"]],
        "unbounded", "unreliable"
      )
    )
  )
  rows$finite_side <- ifelse(
    rows$category == "one-sided",
    ifelse(
      rows$lower_status == PAPER_ENDPOINT_STATUS[["bounded"]],
      rows$lower, rows$upper
    ),
    NA_real_
  )
  rows$direction <- ifelse(
    rows$category == "one-sided",
    ifelse(
      rows$upper_status == PAPER_ENDPOINT_STATUS[["unbounded"]], "up", "down"
    ),
    NA_character_
  )
  rows$coef <- factor(rows$coef, levels = unique(rows$coef))
  two <- rows[rows$category == "two-sided", ]
  one <- rows[rows$category == "one-sided", ]
  n_tally <- table(factor(
    rows$category,
    levels = c("two-sided", "one-sided", "unbounded", "unreliable")
  ))
  cat(
    "  bounds-by-tau rows: two-sided", n_tally[["two-sided"]],
    "one-sided", n_tally[["one-sided"]],
    "unbounded", n_tally[["unbounded"]],
    "unreliable", n_tally[["unreliable"]], "\n"
  )
  stopifnot(
    nrow(rows) > 0L,
    all(is.finite(two$lower)), all(is.finite(two$upper)),
    all(is.finite(one$finite_side)), !anyNA(one$direction)
  )
  list(
    rows = rows, two = two, one = one,
    strip = logvar_bounds_tau_strip(rows, tau_star)
  )
}
