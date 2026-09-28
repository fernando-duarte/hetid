# "Bounded" is supplied inference eligibility, not a geometric certificate
validate_bootstrap_side <- function(value, status, side) {
  assert_bad_argument_ok(
    bootstrap_is_numeric(value) && is.character(status) &&
      length(value) == length(status) && !anyNA(status) &&
      all(status %in% BOOTSTRAP_ENDPOINT_STATUS),
    "endpoint values and statuses must use the documented types and vocabulary"
  )
  missing_value <- is.na(value) & !is.nan(value)
  directed <- is.infinite(value) & if (side == "lower") value < 0 else value > 0
  allowed <- (status == "bounded" & is.finite(value)) |
    (status == "unbounded" & (missing_value | directed)) |
    (status == "unreliable" & (missing_value | is.finite(value) | directed)) |
    (status == "failed" & missing_value)
  assert_bad_argument_ok(
    !any(is.nan(value)) && all(allowed),
    paste0(side, " endpoint values disagree with statuses")
  )
  invisible(TRUE)
}

validate_bootstrap_interval_order <- function(lower, upper, lower_status, upper_status) {
  both <- lower_status == "bounded" & upper_status == "bounded"
  assert_bad_argument_ok(
    all(lower[both] <= upper[both]),
    "jointly eligible lower endpoints must not exceed upper endpoints"
  )
  invisible(TRUE)
}

validate_bootstrap_full <- function(full) {
  required <- c("coef", "lower", "upper", "lower_status", "upper_status")
  assert_bad_argument_ok(
    is.data.frame(full) && nrow(full) > 0L &&
      !anyDuplicated(names(full)) && all(required %in% names(full)),
    "full must contain coefficient names, endpoints and per-side statuses",
    arg = "full"
  )
  plain <- vapply(full[required], function(x) {
    is.null(dim(x)) && length(x) == nrow(full)
  }, logical(1))
  assert_bad_argument_ok(all(plain), "full endpoint fields must be plain vectors")
  assert_bad_argument_ok(is.character(full$coef), "full$coef must be character")
  assert_instrument_names(full$coef, "full$coef")
  assert_bad_argument_ok(length(full$coef) == nrow(full), "full coefficient axis is malformed")
  for (side in c("lower", "upper")) {
    validate_bootstrap_side(full[[side]], full[[paste0(side, "_status")]], side)
  }
  validate_bootstrap_interval_order(full$lower, full$upper, full$lower_status, full$upper_status)
  invisible(TRUE)
}

validate_bootstrap_matrix <- function(x, coefs, type, dimensions = NULL, rows = NULL) {
  assert_bad_argument_ok(is.matrix(x) && type(x), "bootstrap fields must have matrix types")
  assert_dimension_ok(ncol(x) == length(coefs), "bootstrap coefficient dimensions disagree")
  assert_bad_argument_ok(
    identical(colnames(x), coefs),
    "bootstrap coefficient names must match full exactly and in order"
  )
  if (!is.null(dimensions)) {
    assert_dimension_ok(identical(dim(x), dimensions), "bootstrap matrix dimensions disagree")
    assert_bad_argument_ok(identical(rownames(x), rows), "bootstrap draw identities disagree")
  }
  if (!is.null(rownames(x))) assert_instrument_names(rownames(x), "bootstrap draw identities")
  invisible(TRUE)
}

validate_bootstrap_draws <- function(draws, coefs) {
  fields <- c("lower", "upper", "lower_status", "upper_status")
  assert_bad_argument_ok(
    is.list(draws) && !anyDuplicated(names(draws)) &&
      all(fields %in% names(draws)), "draws must contain four paired endpoint matrices",
    arg = "draws"
  )
  validate_bootstrap_matrix(draws$lower, coefs, bootstrap_is_numeric)
  dimensions <- dim(draws$lower)
  rows <- rownames(draws$lower)
  for (field in fields[-1]) {
    type <- if (field == "upper") bootstrap_is_numeric else is.character
    validate_bootstrap_matrix(draws[[field]], coefs, type, dimensions, rows)
  }
  for (side in c("lower", "upper")) {
    validate_bootstrap_side(draws[[side]], draws[[paste0(side, "_status")]], side)
  }
  validate_bootstrap_interval_order(
    draws$lower, draws$upper, draws$lower_status, draws$upper_status
  )
  if (any(c("point", "point_status") %in% names(draws))) {
    validate_bootstrap_point_mirrors(draws, coefs)
  }
  invisible(TRUE)
}

validate_bootstrap_point_mirrors <- function(draws, coefs) {
  assert_bad_argument_ok(
    all(c("point", "point_status") %in% names(draws)),
    "point and point_status must be supplied together"
  )
  for (field in c("point", "point_status")) {
    type <- if (field == "point") bootstrap_is_numeric else is.character
    validate_bootstrap_matrix(
      draws[[field]], coefs, type, dim(draws$lower),
      rownames(draws$lower)
    )
  }
  assert_bad_argument_ok(
    !any(draws$point_status == "unbounded") &&
      identical(draws$lower, draws$point) && identical(draws$upper, draws$point) &&
      identical(draws$lower_status, draws$point_status) &&
      identical(draws$upper_status, draws$point_status),
    "point endpoints and statuses must be exact mirrors; a point cannot be unbounded"
  )
  invisible(TRUE)
}
