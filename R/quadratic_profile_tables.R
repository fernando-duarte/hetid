profile_interval_tables <- function(quadratic, beta1r, beta2r, evidence, control) {
  dimension <- nrow(beta2r)
  containing <- profile_containing_bounds(evidence, dimension)
  side_frame <- function(coef, lower, upper, lo, hi) {
    lower_status <- profile_status_from_flags(lo$bounded, lo$valid)
    upper_status <- profile_status_from_flags(hi$bounded, hi$valid)
    data.frame(
      coef = coef, set_lower = lower, set_upper = upper,
      status = profile_status_worst(c(lower_status, upper_status)),
      lower_status = lower_status, upper_status = upper_status,
      row.names = NULL, stringsAsFactors = FALSE
    )
  }
  solved <- function(objective, index) {
    lapply(c(min = "min", max = "max"), function(direction) {
      profile_linear_bound(quadratic, objective, direction, evidence, index, control)
    })
  }
  theta_rows <- lapply(seq_len(dimension), function(k) {
    objective <- numeric(dimension)
    objective[[k]] <- 1
    side <- solved(objective, k)
    list(
      tab = side_frame(
        rownames(beta2r)[k], side$min$bound, side$max$bound,
        side$min, side$max
      ),
      points = Filter(Negate(is.null), list(side$min$theta, side$max$theta))
    )
  })
  beta_rows <- lapply(seq_along(beta1r), function(j) {
    side <- profile_negligible_loading(beta1r[[j]], beta2r[, j], containing, evidence)
    if (is.null(side)) side <- solved(beta2r[, j], dimension + j)
    list(
      tab = side_frame(
        names(beta1r)[j], unname(beta1r[j]) - side$max$bound,
        unname(beta1r[j]) - side$min$bound, side$max, side$min
      ),
      points = Filter(Negate(is.null), list(side$min$theta, side$max$theta))
    )
  })
  bind <- function(rows) do.call(rbind, lapply(rows, `[[`, "tab"))
  out <- list(
    beta1 = bind(beta_rows),
    theta = cbind(bind(theta_rows), containing)
  )
  attr(out, "profile_points") <- unlist(lapply(c(theta_rows, beta_rows), `[[`, "points"),
    recursive = FALSE
  )
  out
}

# A loading that can move its coefficient by no more than rounding error over
# the verified theta enclosure leaves the coefficient numerically constant, so
# its range comes from that enclosure; searching along a loading that is only
# rounding noise would follow a platform-dependent direction.
profile_negligible_loading <- function(offset, loading, containing, evidence) {
  lower <- containing$outer_lower
  upper <- containing$outer_upper
  if (all(loading == 0) || !isTRUE(evidence$nonempty) ||
    !all(is.finite(c(lower, upper)))) {
    return(NULL)
  }
  low <- sum(pmin(loading * lower, loading * upper))
  high <- sum(pmax(loading * lower, loading * upper))
  magnitudes <- abs(c(offset, loading * pmax(abs(lower), abs(upper))))
  rounding <- HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps *
    length(magnitudes) * sum(magnitudes)
  if (!is.finite(high - low) || high - low > rounding) {
    return(NULL)
  }
  side <- function(bound) list(bound = bound, bounded = TRUE, valid = TRUE)
  list(min = side(low), max = side(high))
}

profile_tables_widened <- function(quadratic, beta1r, beta2r, points, warm, control) {
  dimension <- nrow(beta2r)
  evidence <- profile_evidence(quadratic, cbind(diag(dimension), beta2r), points)
  tables <- profile_interval_tables(quadratic, beta1r, beta2r, evidence, control)
  widened <- profile_multistart(
    quadratic, c(warm, attr(tables, "profile_points")),
    evidence, control
  )
  statuses <- unlist(lapply(tables, function(tab) c(tab$lower_status, tab$upper_status)))
  retry <- any(statuses == "unreliable") && length(widened$points) > 0L
  if (retry) {
    # newly checked points may repair a boundary candidate within the same
    # movement caps, under the same geometry and acceptance rules
    widened$evidence <- profile_evidence(quadratic, evidence$objectives,
      points = profile_point_matrix(widened$points, dimension)
    )
  }
  if (retry || !identical(evidence$summary, widened$evidence$summary)) {
    tables <- profile_interval_tables(
      quadratic, beta1r, beta2r, widened$evidence,
      control
    )
  }
  tables$theta <- profile_widen_theta(tables$theta, widened$points)
  tables$beta1 <- profile_widen_beta1(tables$beta1, beta1r, beta2r, widened$points)
  attr(tables, "profile_points") <- widened$points
  tables
}
