# Offline checks for the log-variance map machinery
# (scripts-paper/log_variance/core/residual_map.R): the projection map equals lm() on the
# same data, the analytic gradient matches central differences, the crossing
# census resolves rows a box screen alone cannot (including zero-w2 rows and
# multi-constraint sets), the feasible grid respects the quadratic
# constraints, the scan's sign tracker flags crossings, and the polish
# accepts interior-feasible optima and guards blow-ups. Run from the package
# root:
#   Rscript scripts-paper/tests/engine/test_residual_map.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

set.seed(42)
n <- 160L
w2 <- matrix(rnorm(n * 2L), n, 2L)
w1 <- drop(w2 %*% c(0.5, -0.3)) + rnorm(n, sd = 2)
pcr <- matrix(rnorm(n * 3L), n, 3L, dimnames = list(NULL, paste0("l.pc", 1:3)))
pcr <- scale(pcr, center = TRUE, scale = FALSE)
proj <- logvar_projection(pcr)
b0 <- c(0.2, -0.1)

# The map equals a literal lm() of log(eps^2) on (1, PC_R)
lv <- log(drop(w1 - w2 %*% b0)^2)
fit <- stats::lm(lv ~ pcr)
check(
  "theta_hat matches lm coefficients",
  max(abs(logvar_theta_hat(b0, w1, w2, proj) - unname(stats::coef(fit)))) < 1e-10
)
check(
  "projection rownames are intercept then PC_R columns",
  identical(rownames(proj), c("(Intercept)", colnames(pcr)))
)

# Analytic gradient matches central finite differences for each coefficient
h <- 1e-6
grad_ok <- vapply(seq_len(nrow(proj)), function(j) {
  g <- logvar_theta_grad(b0, w1, w2, proj[j, ])
  fd <- vapply(seq_along(b0), function(k) {
    e <- numeric(length(b0))
    e[k] <- h
    (logvar_theta_hat(b0 + e, w1, w2, proj)[j] -
      logvar_theta_hat(b0 - e, w1, w2, proj)[j]) / (2 * h)
  }, numeric(1))
  max(abs(g - fd)) < 1e-5 * max(1, max(abs(g)))
}, logical(1))
check("analytic gradient matches central differences", all(grad_ok))

# Crossing census on the unit ball: the range of w2_t' b over the ball is
# [-|w2_t|, |w2_t|]. The first row crosses (0.9 < 1); the second does not
# (1.5 > 1); the third does not either (1.5 > sqrt(2)) yet its box range
# [-2, 2] contains 1.5, so a box screen alone cannot rule it out -- the
# functional solves must
qs_ball <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
census <- logvar_crossing_census(
  qs_ball,
  lower = c(-1, -1), upper = c(1, 1),
  w1 = c(0.9, 1.5, 1.5),
  w2 = rbind(c(1, 0), c(1, 0), c(1, 1))
)
check("census flags exactly the true crossing", identical(census$cross, 1L))
check("census resolves every ambiguous row", length(census$unresolved) == 0L)

# A zero w2 row survives the box screen only when w1_t = 0 (eps identically
# zero over the set): certified as a crossing without a functional solve
census_zero <- logvar_crossing_census(
  qs_ball,
  lower = c(-1, -1), upper = c(1, 1),
  w1 = c(0, 3),
  w2 = rbind(c(0, 0), c(0, 0))
)
check(
  "census certifies the identically-zero residual and screens the nonzero one",
  identical(census_zero$cross, 1L) && length(census_zero$unresolved) == 0L
)

# Feasible grid respects the ball constraint and drops the box corners (the
# roundoff-scale admission tolerance keeps boundary lattice points)
b_feas <- logvar_feasible_grid(qs_ball, c(-1, -1), c(1, 1), 21L)
check(
  "feasible grid stays inside the ball and excludes corners",
  nrow(b_feas) > 0 && max(rowSums(b_feas^2)) <= 1 + 1e-8 &&
    nrow(b_feas) < 21L^2
)

# Multi-constraint lens (unit ball intersected with b1^2 <= 0.25): the grid
# honors both constraints and the census's functional solves work over the
# lens's tighter range
qs_lens <- list(
  A_i = list(diag(2), diag(c(1, 0))),
  b_i = list(c(0, 0), c(0, 0)),
  c_i = c(-1, -0.25)
)
lens_grid <- logvar_feasible_grid(qs_lens, c(-0.5, -1), c(0.5, 1), 21L)
check(
  "feasible grid honors every constraint of a multi-constraint set",
  nrow(lens_grid) > 0 && max(rowSums(lens_grid^2)) <= 1 + 1e-8 &&
    max(lens_grid[, 1]^2) <= 0.25 + 1e-8
)
census_lens <- logvar_crossing_census(
  qs_lens,
  lower = c(-0.5, -1), upper = c(0.5, 1),
  w1 = c(0.4, 0.9),
  w2 = rbind(c(1, 0), c(1, 0))
)
check(
  "census crosses inside and screens outside the lens's functional range",
  identical(census_lens$cross, 1L) && length(census_lens$unresolved) == 0L
)

# Scan fixtures shared by the sign-tracker and polish checks
w2_1 <- matrix(1, 50L, 1L)
w1_2 <- rep(3, 50L)
pcr_1 <- scale(matrix(rnorm(50L), 50L, 1L, dimnames = list(NULL, "l.pc1")),
  center = TRUE, scale = FALSE
)
proj_1 <- logvar_projection(pcr_1)

# The scan's sign tracker flags an observation whose residual changes sign
# across the scanned points, and stays quiet when every sign is stable
w1_1 <- c(0.5, rep(3, 49L))
b_line <- matrix(seq(0, 1, length.out = 41L), ncol = 1L)
scan_cross <- logvar_grid_scan(b_line, w1_1, w2_1, proj_1)
check("scan sign tracker flags the crossing observation", 1L %in% scan_cross$cross_grid)
b_safe <- matrix(seq(-0.4, 0.4, length.out = 41L), ncol = 1L)
scan_safe <- logvar_grid_scan(b_safe, w1_2, w2_1, proj_1)
check(
  "scan sign tracker is quiet on a sign-stable set",
  length(scan_safe$cross_grid) == 0L
)

# Polish on a smooth one-dimensional problem: set b^2 <= 0.25, all residuals
# positive, so the intercept mean(log((w1 - b)^2)) is monotone decreasing in
# b and its extremes sit at the interval endpoints
qs_1 <- list(A_i = list(matrix(1, 1, 1)), b_i = list(0), c_i = -0.25)
scan_1 <- logvar_grid_scan(
  logvar_feasible_grid(qs_1, -0.5, 0.5, 11L), w1_2, w2_1, proj_1
)
truth_min <- logvar_theta_hat(0.5, w1_2, w2_1, proj_1)[1]
truth_max <- logvar_theta_hat(-0.5, w1_2, w2_1, proj_1)[1]
scale_1 <- max(abs(scan_1$min[1]), abs(scan_1$max[1]))
pol_min <- logvar_polish_bound(
  qs_1, "min", scan_1$arg_min[1, ], scale_1, w1_2, w2_1, proj_1[1, ]
)
pol_max <- logvar_polish_bound(
  qs_1, "max", scan_1$arg_max[1, ], scale_1, w1_2, w2_1, proj_1[1, ]
)
check(
  "polish reaches the true endpoint bounds",
  !is.null(pol_min$bound) && !is.null(pol_max$bound) &&
    abs(pol_min$bound - truth_min) < 1e-4 && abs(pol_max$bound - truth_max) < 1e-4
)
check(
  "polish tightens or matches the coarse grid scan",
  pol_min$bound <= scan_1$min[1] + 1e-10 && pol_max$bound >= scan_1$max[1] - 1e-10
)
pol_default <- logvar_polish_bound(
  qs_1, "min", scan_1$arg_min[1, ], scale_1, w1_2, w2_1, proj_1[1, ],
  blow_factor = PAPER_QUADRATIC_CONTROL$polish_blow_factor
)
check("polish default uses the canonical blow guard", identical(pol_min, pol_default))

# Blow-up guard: an artificially tiny blow_factor makes any finite bound
# read as an explosion, exercising the suspect branch
pol_guard <- logvar_polish_bound(
  qs_1, "min", scan_1$arg_min[1, ], scale_1, w1_2, w2_1, proj_1[1, ],
  blow_factor = 1e-6
)
check(
  "blow-up guard flags a suspect side and returns no bound",
  is.null(pol_guard$bound) && isTRUE(pol_guard$suspect)
)

.test$finish()
