# Candidate helpers for the joint-null grid scan.

.jn_omega <- function(qs) {
  .derive_constraint_scales(qs, .derive_theta_scale(qs))
}

.jn_grid_candidates <- function(grid, gobj) {
  keep <- which(is.finite(gobj$q))
  lapply(keep, function(r) {
    list(b = grid[r, ], q = gobj$q[r], s = gobj$s[, r], type = "grid")
  })
}

.jn_pool_order <- function(pool) {
  qv <- vapply(pool, function(p) p$q, numeric(1))
  bm <- do.call(rbind, lapply(pool, function(p) p$b))
  do.call(order, c(list(qv), lapply(seq_len(ncol(bm)), function(j) bm[, j])))
}

.jn_prior_q <- function(prior_minima) {
  if (!length(prior_minima)) {
    return(NA_real_)
  }
  qr <- vapply(prior_minima, function(b) {
    a <- attr(b, "q_reported")
    if (is.null(a)) NA_real_ else as.numeric(a)
  }, numeric(1))
  if (all(is.na(qr))) NA_real_ else min(qr, na.rm = TRUE)
}

.jn_separated_starts <- function(pool, lower, upper, control) {
  if (!length(pool)) {
    return(list())
  }
  ord <- .jn_pool_order(pool)
  bm <- do.call(rbind, lapply(pool, function(p) p$b))
  width <- upper - lower
  width[!is.finite(width) | width <= 0] <- 1
  norm <- sweep(sweep(bm, 2L, lower, "-"), 2L, width, "/")
  min_dist <- control$separated_start_fraction * sqrt(ncol(bm))
  sel <- integer(0)
  for (idx in ord) {
    if (length(sel)) {
      d <- sqrt(colSums((t(norm[sel, , drop = FALSE]) - norm[idx, ])^2))
      if (min(d) < min_dist) next
    }
    sel <- c(sel, idx)
    if (length(sel) >= control$separated_starts) break
  }
  lapply(sel, function(i) {
    structure(pool[[i]]$b, start_type = pool[[i]]$type)
  })
}

.jn_scan_best <- function(pool) {
  if (!length(pool)) {
    return(list(
      b = NA_real_,
      q = Inf,
      s = rep(NA_real_, PAPER_ANALYSIS_CONTRACT$model$n_return_pc),
      source = "grid",
      type = "grid_arg"
    ))
  }
  top <- pool[[.jn_pool_order(pool)[1L]]]
  source <- if (identical(top$type, "carry")) "carry" else "grid"
  type <- switch(top$type,
    grid = "grid_arg",
    b_seed = "b_seed",
    carry = "carry",
    top$type
  )
  list(b = top$b, q = top$q, s = top$s, source = source, type = type)
}
