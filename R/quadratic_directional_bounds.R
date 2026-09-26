# Conservative side-specific certificates for explicitly supported degeneracies.
# A singular block is accepted only when excluded rows are structurally zero.
quadratic_directional_bounds <- function(quadratic, objectives) {
  count <- ncol(objectives)
  dimension <- nrow(objectives)
  lower <- upper <- rep(FALSE, count)
  certificates <- list()
  for (i in seq_along(quadratic$c_i)) {
    a <- quadratic$A_i[[i]]
    b <- quadratic$b_i[[i]]
    active <- which(rowSums(abs(a)) != 0 | b != 0)
    if (all(a == 0) && length(active) == 1L) {
      k <- active[1L]
      only <- colSums(objectives[-k, , drop = FALSE] != 0) == 0
      projection_sign <- sign(objectives[k, ]) * sign(b[k])
      lower <- lower | (only & projection_sign < 0)
      upper <- upper | (only & projection_sign > 0)
      certificates[[length(certificates) + 1L]] <- list(
        type = "coordinate_halfspace", constraint = i, coordinate = k
      )
    } else if (length(active)) {
      block <- a[active, active, drop = FALSE]
      magnitude <- max(abs(block))
      if (magnitude == 0) next
      block <- block / magnitude
      error <- HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps *
        nrow(block) * sum(abs(block))
      if (min(eigen(block, symmetric = TRUE, only.values = TRUE)$values) <= error) next
      outside <- setdiff(seq_len(dimension), active)
      only <- colSums(objectives[outside, , drop = FALSE] != 0) == 0
      lower <- lower | only
      upper <- upper | only
      certificates[[length(certificates) + 1L]] <- list(
        type = "positive_definite_block", constraint = i, coordinates = active
      )
    }
  }
  list(lower = lower, upper = upper, certificates = certificates)
}
