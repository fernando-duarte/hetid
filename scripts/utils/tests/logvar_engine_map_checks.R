# Map-layer checks for the seam additions: the stacked Jacobian
# logvar_theta_jacobian agrees with the per-coefficient gradients and central
# finite differences, and the generalized polish logvar_polish_objective
# reproduces the legacy logvar_polish_bound exactly and supports the
# derivative-free COBYLA path. Sourced by test_logvar_engine.R, which
# supplies check() and the map/polish layer.

set.seed(11)

# the stacked map Jacobian equals the per-coefficient gradients and central
# finite differences at an interior point
map_n <- 40L
map_w2 <- matrix(rnorm(map_n * 2L), map_n, 2L)
map_w1 <- drop(map_w2 %*% c(0.4, -0.2)) + rnorm(map_n, sd = 2)
map_pcr <- scale(matrix(rnorm(map_n * 2L), map_n, 2L,
  dimnames = list(NULL, c("l.pc1", "l.pc2"))
), center = TRUE, scale = FALSE)
map_proj <- logvar_projection(map_pcr)
map_b0 <- c(0.1, -0.05)
map_jac <- logvar_theta_jacobian(map_b0, map_w1, map_w2, map_proj)
map_jac_ok <- vapply(seq_len(nrow(map_proj)), function(j) {
  fd <- vapply(seq_along(map_b0), function(k) {
    e <- numeric(length(map_b0))
    e[k] <- 1e-6
    (logvar_theta_hat(map_b0 + e, map_w1, map_w2, map_proj)[j] -
      logvar_theta_hat(map_b0 - e, map_w1, map_w2, map_proj)[j]) / 2e-6
  }, numeric(1))
  g <- logvar_theta_grad(map_b0, map_w1, map_w2, map_proj[j, ])
  max(abs(map_jac[j, ] - g)) < 1e-12 &&
    max(abs(map_jac[j, ] - fd)) < 1e-5 * max(1, max(abs(g)))
}, logical(1))
check(
  "map Jacobian stacks the gradients and matches central differences",
  all(map_jac_ok) && identical(dim(map_jac), c(nrow(map_proj), 2L))
)

# the generalized polish with the log-OLS closures reproduces the legacy
# wrapper exactly on the map test's smooth fixture
map_w2_1 <- matrix(1, 50L, 1L)
map_w1_2 <- rep(3, 50L)
map_pcr_1 <- scale(matrix(rnorm(50L), 50L, 1L, dimnames = list(NULL, "l.pc1")),
  center = TRUE, scale = FALSE
)
map_proj_1 <- logvar_projection(map_pcr_1)
map_qs_1 <- list(A_i = list(matrix(1, 1, 1)), b_i = list(0), c_i = -0.25)
map_scan_1 <- logvar_grid_scan(
  logvar_feasible_grid(map_qs_1, -0.5, 0.5, 11L), map_w1_2, map_w2_1, map_proj_1
)
map_scale_1 <- max(abs(map_scan_1$min[1]), abs(map_scan_1$max[1]))
map_old <- logvar_polish_bound(
  map_qs_1, "min", map_scan_1$arg_min[1, ], map_scale_1,
  map_w1_2, map_w2_1, map_proj_1[1, ]
)
map_new <- logvar_polish_objective(
  map_qs_1, "min", map_scan_1$arg_min[1, ], map_scale_1,
  fn = function(b) sum(map_proj_1[1, ] * log(drop(map_w1_2 - map_w2_1 %*% b)^2)),
  gr = function(b) logvar_theta_grad(b, map_w1_2, map_w2_1, map_proj_1[1, ])
)
check(
  "generalized polish reproduces the legacy polish exactly",
  identical(map_old$bound, map_new$bound) &&
    identical(map_old$suspect, map_new$suspect)
)

# derivative-free path: COBYLA under the same normalized constraints improves
# a nonsmooth toy objective from a feasible start inside the unit ball
map_qs_ball <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
map_cob <- logvar_polish_objective(
  map_qs_ball, "min", c(-0.5, 0), 1,
  fn = function(b) abs(b[1] - 0.2), method = "cobyla"
)
check(
  "cobyla polish returns a feasible improved nonsmooth point",
  !is.null(map_cob$bound) && map_cob$bound < abs(-0.5 - 0.2) &&
    map_cob$feas_resid <= 1e-4
)
