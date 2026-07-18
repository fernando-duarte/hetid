# Standalone synthetic solver-toy checks for the bespoke SLSQP epigraph solver
# of the joint log-variance GMM.
# This script is deliberately not wired into the committed entry point
# test_joint_gmm.R: it exercises the disabled compatibility-search branch on
# four synthetic epigraph programs whose geometry is known in closed form, so it
# validates the normalized-coordinate Jacobian assembly, the constructive search
# statuses, and the fail-closed direction-certificate contract without any
# empirical inputs. Sources the committed replication and compatibility-search
# modules, then runs its own check() harness. Run from the worktree root:
#   Rscript scripts-paper/tests/diagnostics/joint_gmm/test_epigraph_solver.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "moments.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "basis.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "profiles.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "identity.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "candidates.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "budget.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "epigraph.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# A scalar single-moment epigraph spec in identity normalized coordinates.
scalar_spec <- function(moment, moment_jac) {
  list(center = 0, scale = 1, s_g = 1, moment = moment, moment_jac = moment_jac)
}
root_tol <- logvar_joint_gmm_constants$root_tol

# Run the epigraph solver from a pool of starts at one width and classify by the
# smallest attained scaled sup-norm, treating every start as configured coverage.
run_search <- function(spec, starts, box_half_width) {
  sols <- lapply(starts, function(s0) {
    logvar_joint_epigraph_solve(spec, s0, box_half_width)
  })
  linf <- vapply(sols, function(s) {
    if (identical(s$numerical_status, "attained")) s$scaled_linf else Inf
  }, numeric(1))
  best <- sols[[which.min(linf)]]
  status <- logvar_joint_classify_search(
    attained = best$scaled_linf, has_root_tol_candidate = min(linf) <= root_tol,
    coverage_complete = TRUE, material_failure = FALSE
  )
  list(best = best, status = status)
}

# Linear toy: g(x) = 2x - 1, root at x = 0.5. Its matrix inequalities and the
# target rate (the scaled slope 2) are verified against hand computation.
lin <- scalar_spec(function(x) 2 * x - 1, function(x) matrix(2, 1, 1))
prog <- logvar_joint_epigraph_program(lin, box_half_width = 16)
hjac <- prog$hinjac(c(0.25, 0.4))
check("linear toy hin equals the hand-computed +/- S^{-1} g - r rows", isTRUE(
  all.equal(prog$hin(c(0.25, 0.4)), c(-0.9, 0.1))
))
check("linear toy hinjac is cbind(+/- scaled slope, -1) with the exact rate", {
  identical(dim(hjac), c(2L, 2L)) && all(as.vector(hjac) == c(2, -2, -1, -1))
})
lin_sol <- logvar_joint_epigraph_solve(lin, x0 = -1, box_half_width = 16)
check("linear toy solves to the exact root within root_tol", {
  abs(lin_sol$par - 0.5) < 1e-6 && lin_sol$scaled_linf < root_tol &&
    identical(lin_sol$numerical_status, "attained")
})

# Convex toy: g(x) = x^2 + 1 has a known positive global minimum of 1, so the
# search attains a positive upper bound and reports not_found_within_search with
# no lower-bound certificate ever attached.
cvx <- scalar_spec(function(x) x^2 + 1, function(x) matrix(2 * x, 1, 1))
cvx_run <- run_search(cvx, list(0.3), 16)
check("convex toy attains its positive minimum above root_tol", {
  cvx_run$best$scaled_linf > root_tol &&
    abs(cvx_run$best$scaled_linf - 1) < 1e-5 &&
    identical(cvx_run$best$numerical_status, "attained")
})
check("convex positive-minimum toy is not_found_within_search", {
  identical(cvx_run$status, "not_found_within_search")
})

# Nonconvex toy: g(x) = 0.5 - exp(-(x - 4)^2) has roots near 3.167 and 4.833 and
# a flat plateau of 0.5 far away. A start pool truncated to the plateau hides the
# root, but never certifies emptiness; a near-root start finds it.
nc <- scalar_spec(
  function(x) 0.5 - exp(-(x - 4)^2),
  function(x) matrix(2 * (x - 4) * exp(-(x - 4)^2), 1, 1)
)
nc_hidden <- run_search(nc, list(-8), 16)
nc_found <- run_search(nc, list(-8, 3.3), 16)
check("nonconvex hidden root: a truncated pool never emits empty", {
  identical(nc_hidden$status, "not_found_within_search") &&
    !(nc_hidden$status %in% c("empty", "exact_root", "region_unchanged"))
})
check("nonconvex hidden root: a within-root_tol start is found_within_root_tol", {
  identical(nc_found$status, "found_within_root_tol")
})

# Unbounded toy: g(x) = exp(-x) drives r toward zero at the box edge, so the
# winner keeps moving as the box grows. Movement is unbounded only with a
# verified analytic direction certificate; a probe or no certificate fails closed.
unb <- scalar_spec(function(x) exp(-x), function(x) matrix(-exp(-x), 1, 1))
s4 <- logvar_joint_epigraph_solve(unb, x0 = 0, box_half_width = 4)
s8 <- logvar_joint_epigraph_solve(unb, x0 = 0, box_half_width = 8)
two_agree <- abs(s4$r - s8$r) <= 1e-5 * max(1, abs(s4$r))
moving <- !s4$box_interior && !s8$box_interior && !two_agree
cert <- logvar_joint_direction_certificate(
  direction = 1, rate = 1, sign = -1,
  record = "analytic: exp(-x) decreases monotonically as x grows",
  analytic_proof = TRUE
)
probe <- logvar_joint_direction_certificate(
  direction = 1, rate = 1, sign = -1, record = "finite probe",
  analytic_proof = FALSE
)
box_with <- function(dc) {
  logvar_joint_box_status(
    interior = s8$box_interior, two_width_agree = two_agree,
    box_active = !s8$box_interior, moving = moving, direction_certificate = dc
  )
}
check("unbounded toy keeps moving to the box edge across two widths", {
  isTRUE(moving) && isFALSE(two_agree)
})
check("unbounded toy with a verified direction certificate is unbounded", {
  identical(box_with(cert), "unbounded")
})
check("unbounded toy without a certificate fails closed to unreliable", {
  identical(box_with(probe), "unreliable") && identical(box_with(NULL), "unreliable")
})
check("a finite probe can never certify recession", {
  isFALSE(probe$certified) && isTRUE(cert$certified)
})

.test$finish()
