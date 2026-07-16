# Inactive offline second-pass refinement schedule for the median (LAD) map.
# Around every pass-one arg-extremum and every verified crossing witness,
# form the tensor offsets {-r, 0, r}^K at r = logvar_lad_refine_radii times the
# frozen b_scales, clip to the box, keep the normalized-feasible rows plus the
# verified anchor/path points, deduplicate at full precision, and order
# lexicographically by center type, row, radius and offset. These lattices enter
# the engine through extra_starts, whose attained-first semantics mean refinement
# can only sharpen the attained hull, never lose an earlier value. Definitions
# only; sourced by test_outer_map.R and never by the production pipeline.

# Build the refinement centers from a pass-one schema (each bounded side's
# attaining arg) and the precheck's verified witnesses (each crossing's b_cross).
# type/row order deterministically downstream so the emitted lattice is stable.
logvar_lad_refine_centers <- function(schema, witnesses = list()) {
  centers <- list()
  add <- function(type, row, b) {
    if (is.null(b) || anyNA(b)) {
      return(invisible())
    }
    centers[[length(centers) + 1L]] <<- list(type = type, row = row, b = unname(b))
  }
  for (j in seq_len(nrow(schema))) {
    if (identical(schema$lower_status[j], "bounded")) {
      add("extremum", j, schema$arg_lower[[j]])
    }
    if (identical(schema$upper_status[j], "bounded")) {
      add("extremum", j, schema$arg_upper[[j]])
    }
  }
  for (wid in names(witnesses)) {
    add("witness", as.integer(witnesses[[wid]]$row), witnesses[[wid]]$b_cross)
  }
  centers
}

# The verified feasible anchors from every witness, flattened to bare numeric
# points; anchors may be bare vectors or records carrying $b.
logvar_lad_refine_anchors <- function(witnesses = list()) {
  anchors <- list()
  for (wid in names(witnesses)) {
    for (a in witnesses[[wid]]$anchors) {
      b <- if (is.list(a)) a$b else a
      if (!is.null(b) && !anyNA(b)) anchors[[length(anchors) + 1L]] <- unname(b)
    }
  }
  anchors
}

# The pinned lattice: centers ordered by (type, row), then per center the radii
# in logvar_lad_refine_radii, then the {-1,0,1}^K offsets in expand.grid order.
# Clipped to [lower, upper], filtered by the normalized feasibility check, with
# the verified anchors retained; full-precision deduplication keeps the first
# occurrence so the lexicographic order survives. Returns a list of b vectors
# suitable for the engine's extra_starts.
logvar_lad_refinement_points <- function(centers, b_scales, lower, upper,
                                         check_feasible, anchors = list()) {
  scale_vec <- b_scales$delta
  k <- length(scale_vec)
  types <- vapply(centers, function(ct) as.character(ct$type), character(1))
  rows <- vapply(centers, function(ct) {
    if (is.null(ct$row)) 0L else as.integer(ct$row)
  }, integer(1))
  centers <- centers[order(types, rows, method = "radix")]
  offsets <- as.matrix(
    expand.grid(rep(list(c(-1, 0, 1)), k), KEEP.OUT.ATTRS = FALSE)
  )
  keys <- character(0)
  pts <- list()
  add <- function(b) {
    key <- logvar_b_key(b)
    if (key %in% keys) {
      return(invisible())
    }
    keys[[length(keys) + 1L]] <<- key
    pts[[length(pts) + 1L]] <<- b
  }
  for (ctr in centers) {
    for (r in logvar_lad_refine_radii) {
      for (oi in seq_len(nrow(offsets))) {
        b <- pmin(pmax(ctr$b + offsets[oi, ] * r * scale_vec, lower), upper)
        if (isTRUE(check_feasible(b)$feasible)) add(b)
      }
    }
  }
  for (a in anchors) {
    b <- pmin(pmax(a, lower), upper)
    if (isTRUE(check_feasible(b)$feasible)) add(b)
  }
  pts
}
