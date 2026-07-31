# Alternative interval constructions, computed beside the published Target P so
# the choice of reference distribution is visible rather than asserted. None of
# these reaches a table: they are diagnostics.
#
# All three are pure functions of the SAME stored endpoint draws, which is why
# they can be added without new resampling. The studentised (bootstrap-t)
# construction is deliberately absent: it needs a per-draw scale s*, hence a
# nested bootstrap, and is out of scope.
#
# NORMAL pads the set by the textbook Wald multiple. It is the construction a
# reader would reach for by default, and on this data it is the wrong one -- the
# root distribution is heavy through its body, with a ninetieth percentile near
# 3.4 against 1.645 -- so it earns its place as the thing being argued against.
#
# PERCENTILE takes the draws' own quantiles. BASIC reflects them through the
# full-sample endpoint. The two disagree in opposite directions under skew, which
# is the point of carrying both: where they disagree, the shape of the draw
# distribution is doing the work rather than the data's location.

# Both sides must be gated. A cell with one live side publishes a half-infinite
# interval whose finite end is already the live side's own quantile, so an
# alternative construction there would compare against nothing.
endpoint_alternative_intervals <- function(lower, upper, lc, uc, f, alpha) {
  blank <- list(
    ci_normal_lower = NA_real_, ci_normal_upper = NA_real_,
    ci_pct_lower = NA_real_, ci_pct_upper = NA_real_,
    ci_basic_lower = NA_real_, ci_basic_upper = NA_real_
  )
  if (!isTRUE(lc$gate) || !isTRUE(uc$gate)) {
    return(blank)
  }
  lo <- lower[lc$ok]
  up <- upper[uc$ok]
  if (!length(lo) || !length(up)) {
    return(blank)
  }
  half <- alpha / 2
  ql <- stats::quantile(lo, c(half, 1 - half), names = FALSE)
  qu <- stats::quantile(up, c(half, 1 - half), names = FALSE)
  list(
    ci_normal_lower = f$set_lower - stats::qnorm(1 - half) * lc$se,
    ci_normal_upper = f$set_upper + stats::qnorm(1 - half) * uc$se,
    ci_pct_lower = ql[[1L]],
    ci_pct_upper = qu[[2L]],
    # reflected through the full-sample endpoint: the upper draw quantile sets
    # the lower bound and vice versa, which is what makes basic and percentile
    # move oppositely when the draws are skewed
    ci_basic_lower = 2 * f$set_lower - ql[[2L]],
    ci_basic_upper = 2 * f$set_upper - qu[[1L]]
  )
}
