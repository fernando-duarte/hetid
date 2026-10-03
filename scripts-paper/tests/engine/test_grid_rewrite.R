#!/usr/bin/env Rscript
# The feasible-grid filter and the nearest-neighbor ordering against frozen
# copies of their earlier bodies: both rewrites are speedups only, so every
# output must match the old one byte for byte. A later change that is meant to
# move either output replaces these frozen bodies. Run from the package root:
#   Rscript scripts-paper/tests/engine/test_grid_rewrite.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# Frozen references ------------------------------------------------------------
old_feasible_filter <- function(values) {
  apply(values <= PAPER_QUADRATIC_CONTROL$admission_tolerance, 1L, all)
}
old_feasible_grid <- function(qs, lower, upper, n_axis) {
  axes <- Map(function(lo, hi) seq(lo, hi, length.out = n_axis), lower, upper)
  b_grid <- as.matrix(expand.grid(axes, KEEP.OUT.ATTRS = FALSE))
  dimnames(b_grid) <- NULL
  omega <- .derive_constraint_scales(qs, .derive_theta_scale(qs))
  values <- quadratic_constraint_values(b_grid, qs, omega)
  b_grid[old_feasible_filter(values), , drop = FALSE]
}
old_order_grid_nn <- function(b_feas, b_seed = NULL) {
  m <- nrow(b_feas)
  if (m == 0L) {
    return(integer(0))
  }
  cur <- if (is.null(b_seed) || anyNA(b_seed)) {
    1L
  } else {
    which.min(colSums((t(b_feas) - b_seed)^2))
  }
  ord <- integer(m)
  left <- rep(TRUE, m)
  for (k in seq_len(m)) {
    ord[k] <- cur
    left[cur] <- FALSE
    if (k == m) break
    d <- colSums((t(b_feas) - b_feas[cur, ])^2)
    d[!left] <- Inf
    cur <- which.min(d)
  }
  ord
}

same_bytes <- function(a, b) {
  identical(serialize(a, NULL, version = 3), serialize(b, NULL, version = 3)) &&
    identical(a, b, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE)
}

check("the byte oracle tells signed zeros and attribute order apart", {
  !same_bytes(0, -0) &&
    !same_bytes(structure(1, a = 1, b = 2), structure(1, b = 2, a = 1))
})

# Feasible grid ----------------------------------------------------------------
qs_ball <- list(A_i = list(diag(3)), b_i = list(c(0, 0, 0)), c_i = -1)
qs_three <- list(
  A_i = list(diag(3), diag(c(1, 0, 2)), diag(c(0.5, 1, 0))),
  b_i = list(c(0, 0, 0), c(0.1, 0, 0), c(0, -0.2, 0)), c_i = c(-1, -0.5, -0.6)
)
qs_thin <- list(A_i = list(diag(c(1, 1, 400))), b_i = list(c(0, 0, 0)), c_i = -1)
qs_empty <- list(A_i = list(diag(3)), b_i = list(c(0, 0, 0)), c_i = 1)
qs_full <- list(A_i = list(diag(3)), b_i = list(c(0, 0, 0)), c_i = -100)
qs_none <- list(A_i = list(), b_i = list(), c_i = numeric(0))
qs_na <- list(A_i = list(diag(3)), b_i = list(c(0, 0, 0)), c_i = NA_real_)
qs_nan <- list(A_i = list(diag(3)), b_i = list(c(0, 0, 0)), c_i = NaN)
check("the grid matches its earlier body across systems and axis sizes", {
  cases <- expand.grid(
    qs = c("ball", "three", "thin", "empty", "full", "none", "na", "nan"),
    n = c(1L, 2L, 5L, 41L),
    stringsAsFactors = FALSE
  )
  systems <- list(
    ball = qs_ball, three = qs_three, thin = qs_thin, empty = qs_empty, full = qs_full,
    none = qs_none, na = qs_na, nan = qs_nan
  )
  all(vapply(seq_len(nrow(cases)), function(i) {
    qs <- systems[[cases$qs[i]]]
    lo <- c(-1.2, -1.1, -1)
    hi <- c(1.2, 1, 1.1)
    same_bytes(
      old_feasible_grid(qs, lo, hi, cases$n[i]),
      logvar_feasible_grid(qs, lo, hi, cases$n[i])
    )
  }, logical(1)))
})

# Nearest-neighbor ordering ----------------------------------------------------
set.seed(20261002)
grids <- list(
  empty = matrix(numeric(0), 0L, 3L),
  single = matrix(c(0.1, 0.2, 0.3), 1L),
  ties = rbind(diag(3), -diag(3), diag(3)),
  symmetric = as.matrix(expand.grid(-1:1, -1:1, -1:1)),
  random = matrix(rnorm(600), 200L, 3L)
)
seeds <- list(NULL, c(NA_real_, 0, 0), c(0, 0, 0), c(0.5, -0.4, 2))
check("the ordering matches its earlier body on every grid and seed", {
  all(vapply(grids, function(g) {
    all(vapply(seeds, function(s) {
      same_bytes(old_order_grid_nn(g, s), logvar_order_grid_nn(g, s))
    }, logical(1)))
  }, logical(1)))
})

.test$finish()
