# Profile-bounds solver tests. Run from package root:
#   Rscript scripts/utils/tests/test_profile_bounds.R
suppressMessages(library(nloptr))
source("scripts/utils/profile_bounds.R")
source("scripts/utils/format_utils.R")

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}
approx <- function(a, b, tol = 1e-5) {
  all(is.finite(a)) && all(is.finite(b)) &&
    max(abs(a - b) / pmax(1, abs(b))) < tol
}
quad <- function(A_list, b_list, c_vec) {
  list(A_i = A_list, b_i = b_list, c_i = c_vec)
}

# Unit ball {theta: theta'theta - 1 <= 0} -> each coord in [-1, 1], valid
r <- solve_all_profile_bounds(quad(list(diag(3)), list(rep(0, 3)), c(-1)))
check("unit ball lower = -1", approx(r$lower, c(-1, -1, -1)))
check("unit ball upper = +1", approx(r$upper, c(1, 1, 1)))
check("unit ball all bounded", all(r$bounded_lower & r$bounded_upper))
check("unit ball all valid", all(r$valid_lower & r$valid_upper))

# Scale invariance: same set at 1e-10 scale -> still [-1, 1]
rt <- solve_all_profile_bounds(quad(list(1e-10 * diag(3)), list(rep(0, 3)), c(-1e-10)))
check(
  "tiny-scale ball invariant to [-1,1]",
  approx(rt$lower, c(-1, -1, -1), 1e-4) && approx(rt$upper, c(1, 1, 1), 1e-4)
)

# Anisotropic ellipsoid {(theta1/100)^2 + theta2^2 + theta3^2 <= 1}
ra <- solve_all_profile_bounds(quad(list(diag(c(1e-4, 1, 1))), list(rep(0, 3)), c(-1)))
check(
  "anisotropic theta1 in [-100,100]",
  approx(ra$lower[1], -100, 1e-3) && approx(ra$upper[1], 100, 1e-3)
)

# Large-bounded ellipsoid theta1 in [-3e6, 3e6] (true bound in (2*box1, box2)):
# must be BOUNDED, not Inf (regression vs the old >2*box1 rule).
rb <- solve_all_profile_bounds(quad(list(diag(c((1 / 3e6)^2, 1, 1))), list(rep(0, 3)), c(-1)))
check(
  "large-bounded theta1 ~ +/-3e6 (bounded, not Inf)",
  rb$bounded_lower[1] && rb$bounded_upper[1] &&
    approx(rb$upper[1], 3e6, 1e-2) && approx(rb$lower[1], -3e6, 1e-2)
)

# Infeasible {theta1 <= -1 AND -theta1 <= -1}: feasibility oracle must reject.
ri <- solve_all_profile_bounds(quad(
  list(matrix(0, 3, 3), matrix(0, 3, 3)),
  list(c(1, 0, 0), c(-1, 0, 0)), c(1, 1)
))
check("infeasible system: theta1 not valid", !ri$valid_lower[1] && !ri$valid_upper[1])

# Unbounded half-space {theta1 <= 0}: A = 0, b = (1,0,0), c = 0 (directional)
rh <- solve_all_profile_bounds(quad(list(matrix(0, 3, 3)), list(c(1, 0, 0)), c(0)))
check(
  "half-space theta1 max = 0 (bounded)",
  approx(rh$upper[1], 0, 1e-5) && rh$bounded_upper[1]
)
check(
  "half-space theta1 min unbounded (-Inf)",
  is.infinite(rh$lower[1]) && rh$lower[1] < 0 && !rh$bounded_lower[1]
)
check(
  "half-space theta2 unbounded both sides",
  is.infinite(rh$lower[2]) && is.infinite(rh$upper[2])
)

# Stability: tiny ill-scaled bounded ellipsoid solved at 3 tolerances -> stable.
q_ill <- quad(list(1e-10 * diag(c(1e-4, 1, 1))), list(rep(0, 3)), c(-1e-10))
bnds <- lapply(c(1e-6, 1e-8, 1e-10), function(xt) {
  solve_all_profile_bounds(q_ill, xtol_rel = xt)$upper
})
check(
  "ill-scaled bounds stable across tolerances",
  approx(bnds[[1]], bnds[[2]], 1e-3) && approx(bnds[[2]], bnds[[3]], 1e-3)
)

# Closed-form point identification (tau=0): Q theta = L, here Q = I -> theta = L.
pt <- solve_point_identification(list(
  Q_i = list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)), L_i = c(2, -3, 5)
))
check(
  "point-id solves Q theta = L exactly",
  !is.null(pt) && approx(pt$theta, c(2, -3, 5))
)

# format helpers: 4-state rendering
check(
  "format_bound renders states",
  format_bound(1.5) == "1.5000" && format_bound(Inf) == "unbounded" &&
    format_bound(NA_real_, valid = FALSE) == "unreliable"
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
