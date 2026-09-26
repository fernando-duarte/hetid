# Pool membership is verified against the current system, including warm points.
.theta_start_key <- function(point) {
  paste(signif(point, PAPER_QUADRATIC_CONTROL$box_multistart_dedup_digits), collapse = "|")
}

.dedup_theta_starts <- function(points) {
  points <- Filter(function(point) {
    !is.null(point) && length(point) && all(is.finite(point))
  }, points)
  points[!duplicated(vapply(points, .theta_start_key, character(1)))]
}

theta_box_start_pool <- function(qs, warm = NULL) {
  dimension <- ncol(qs$A_i[[1L]])
  delta <- .derive_theta_scale(qs)
  axes <- unlist(lapply(seq_len(dimension), function(k) {
    axis <- numeric(dimension)
    axis[k] <- delta
    list(axis, -axis)
  }), recursive = FALSE)
  .dedup_theta_starts(c(list(numeric(dimension)), axes, warm))
}

profile_widen_theta_points <- function(tab, points) {
  if (!length(points)) {
    return(tab)
  }
  values <- do.call(rbind, points)
  bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
  for (k in seq_len(nrow(tab))) {
    if (tab$lower_status[k] == bounded) tab$set_lower[k] <- min(tab$set_lower[k], values[, k])
    if (tab$upper_status[k] == bounded) tab$set_upper[k] <- max(tab$set_upper[k], values[, k])
  }
  tab
}

profile_apply_theta_tails <- function(tab, evidence) {
  for (k in seq_len(nrow(tab))) {
    if (evidence$summary$lower_state[k] == "unbounded") {
      tab$set_lower[k] <- -Inf
      tab$lower_status[k] <- PAPER_ENDPOINT_STATUS[["unbounded"]]
    }
    if (evidence$summary$upper_state[k] == "unbounded") {
      tab$set_upper[k] <- Inf
      tab$upper_status[k] <- PAPER_ENDPOINT_STATUS[["unbounded"]]
    }
  }
  tab$status <- paper_endpoint_status_reduce(tab$lower_status, tab$upper_status)
  tab
}
