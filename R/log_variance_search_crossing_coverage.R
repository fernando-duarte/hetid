lv_log_crossing_coverage <- function(groups, census, labels) {
  pending <- vapply(groups$rows, function(rows) any(rows %in% census$unresolved), logical(1))
  active <- vapply(groups$rows, function(rows) any(rows %in% census$cross), logical(1))
  sides <- lv_log_group_sides(groups, census$cross, labels)
  signs <- groups$signs[, pending, drop = FALSE]
  # an unknown sign can threaten either direction and cannot establish coverage
  lower <- apply(signs > 0 | is.na(signs), 1L, any)
  upper <- apply(signs < 0 | is.na(signs), 1L, any)
  complete <- !length(groups$ambiguous) &&
    !anyNA(groups$signs[, pending | active, drop = FALSE]) &&
    all(!lower | sides$lower_unbounded) && all(!upper | sides$upper_unbounded)
  list(
    unresolved = census$unresolved, groups = which(pending), group_rows = groups$rows[pending],
    lower_unbounded = sides$lower_unbounded, upper_unbounded = sides$upper_unbounded,
    threatened_lower = lower, threatened_upper = upper, complete = complete
  )
}

lv_set_unresolved_precheck <- function(precheck) {
  coverage <- precheck$unresolved_coverage
  length(precheck$unresolved) > 0L &&
    !(is.list(coverage) && identical(coverage$complete, TRUE))
}
