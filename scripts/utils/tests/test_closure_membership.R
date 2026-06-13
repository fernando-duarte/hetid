# Constraint-checker closure membership probe tests. Run from package root:
#   Rscript scripts/utils/tests/test_closure_membership.R
suppressMessages({
  library(hetid)
})
source("scripts/utils/closure_membership.R")

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

# Unit disk in R^2: theta'theta <= 1 (A = I, b = 0, c = -1). One constraint.
disk <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
bounds_box <- data.frame(lower = c(-1, -1), upper = c(1, 1))

# Grid shape: n_per_dim^I rows, I columns; corners = 2^I.
g <- make_theta_grid(bounds_box, n_per_dim = 5L)
check(
  "grid has n_per_dim^I rows and I columns",
  nrow(g$grid) == 25L && ncol(g$grid) == 2L
)
check("corners has 2^I rows", nrow(g$corners) == 4L)
check("center is the box midpoint", all(g$center == c(0, 0)))
check("no fallback on a finite box", !any(g$fell_back))

# Membership: center inside the unit disk, all four +/-1 corners outside.
probe <- probe_set_membership(disk, bounds_box, n_per_dim = 5L)
check("center inside the unit disk", isTRUE(probe$summary$center_inside))
check("all box corners outside the unit disk", probe$summary$n_corners_inside == 0L)
check(
  "frac_inside strictly between 0 and 1",
  probe$summary$frac_inside > 0 && probe$summary$frac_inside < 1
)

# Membership agrees with a direct make_system_checker evaluation.
chk <- make_system_checker(disk)
check(
  "probe agrees with direct closure at a known interior point",
  (max(chk(c(0.5, 0.5))) <= 0) == TRUE
)

# Single constraint => binding_freq is the whole mass on constraint 1.
check(
  "binding_freq sums to 1 over in-set grid points",
  abs(sum(probe$per_constraint$binding_freq) - 1) < 1e-9
)

# Unbounded second coordinate: theta1^2 <= 1, theta2 free.
half <- list(A_i = list(matrix(c(1, 0, 0, 0), 2, 2)), b_i = list(c(0, 0)), c_i = -1)
bounds_unb <- data.frame(lower = c(-1, -Inf), upper = c(1, Inf))
g2 <- make_theta_grid(bounds_unb, n_per_dim = 5L, fallback_span = 2)
check("fallback flagged only on the unbounded axis", identical(g2$fell_back, c(FALSE, TRUE)))
check("fallback axis is finite", all(is.finite(g2$grid)))
probe2 <- probe_set_membership(half, bounds_unb, n_per_dim = 5L, fallback_span = 2)
check("any_fallback reported in the summary", isTRUE(probe2$summary$any_fallback))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
