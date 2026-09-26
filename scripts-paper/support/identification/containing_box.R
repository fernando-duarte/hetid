# Containing bounds of the news coefficients, kept apart from attained endpoints.
# set_lower/set_upper are values at checked member points: they display and
# bootstrap the set, but a search domain or a no-crossing verdict needs bounds
# that contain it. outer_lower/outer_upper carry those verified bounds, from the
# evidence's positive definite combination, on the theta table only.

# Producer side: containing columns for the first `dimension` evidence
# objectives, which are the coordinates in every theta-table producer. A side
# the evidence proves unbounded is infinite; without a verified certificate a
# finite side is unknown (NA), never an attained value.
profile_containing_bounds <- function(evidence, dimension) {
  out <- data.frame(
    outer_lower = rep(NA_real_, dimension),
    outer_upper = rep(NA_real_, dimension)
  )
  if (!is.null(evidence$boundedness)) {
    outer <- evidence$outer_bounds(diag(dimension), refine = TRUE)
    if (isTRUE(attr(outer, "empty")) && isTRUE(evidence$nonempty)) {
      stop("geometry conflict: a checked point is feasible but a verified ",
        "combination proves the set empty",
        call. = FALSE
      )
    }
    out$outer_lower <- outer$lower
    out$outer_upper <- outer$upper
  }
  states <- evidence$summary[seq_len(dimension), ]
  out$outer_lower[states$lower_state == "unbounded"] <- -Inf
  out$outer_upper[states$upper_state == "unbounded"] <- Inf
  out
}

# Consumer side: the only way a search domain or census screen reads the box.
# There is no fallback to set_lower/set_upper; a table without containing
# columns, or a bounded row without finite ones, is a contract breach. A
# bounded row's attained endpoints are checked members, so they must lie
# inside its containing bounds. A search domain needs every row bounded; a
# caller that decides from the status itself passes require_bounded = FALSE
# and receives infinite or NA bounds for the other rows.
paper_containing_box <- function(tab, require_bounded = TRUE) {
  if (!all(c("outer_lower", "outer_upper") %in% names(tab))) {
    stop("theta table lacks containing bounds outer_lower/outer_upper", call. = FALSE)
  }
  lower <- tab$outer_lower
  upper <- tab$outer_upper
  if (is.null(tab$status) || length(tab$status) != length(lower) || anyNA(tab$status)) {
    stop("a theta row has no endpoint status", call. = FALSE)
  }
  if (any(!is.na(lower) & !is.na(upper) & lower > upper)) {
    stop("a containing lower bound exceeds its upper bound", call. = FALSE)
  }
  bounded <- tab$status == PAPER_ENDPOINT_STATUS[["bounded"]]
  if (isTRUE(require_bounded) && !all(bounded)) {
    stop("a containing search domain needs every theta row bounded", call. = FALSE)
  }
  if (any(bounded & !(is.finite(lower) & is.finite(upper)))) {
    stop("a bounded theta row lacks finite containing bounds", call. = FALSE)
  }
  inside <- lower[bounded] <= tab$set_lower[bounded] &
    tab$set_upper[bounded] <= upper[bounded]
  if (!all(inside)) {
    stop("an attained theta endpoint lies outside its containing bound", call. = FALSE)
  }
  list(lower = lower, upper = upper)
}
