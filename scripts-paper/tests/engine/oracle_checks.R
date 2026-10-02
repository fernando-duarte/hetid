# Oracle-equivalence checks for the estimator-generic set engine: a frozen
# verbatim-logic copy of the driver closure logvar_set_at_tau is the oracle,
# and the engine in benchmark configuration must reproduce its legacy table
# (statuses and infinities exactly, finite endpoints to roundoff) while
# exposing the richer per-side schema. Sourced by test_engine.R, which supplies
# check() and the map/engine/log-OLS layers.
set.seed(19)
paper_source_once(paper_path("tests", "engine", "oracle_reference.R"))
# a K = 2 fixture with centered PCs (two columns -> three theta coefficients),
# the log-OLS estimator, and the all-bounded b_tab the oracle asserts against
orc_make_fixture <- function(w2, w1) {
  colnames(w2) <- c("bN1", "bN2")
  pcr <- scale(matrix(rnorm(nrow(w2) * 2L), nrow(w2), 2L,
    dimnames = list(NULL, c("l.pc1", "l.pc2"))
  ), center = TRUE, scale = FALSE)
  proj <- logvar_projection(pcr)
  qtr <- seq_len(nrow(w2))
  b_tab <- data.frame(
    coef = colnames(w2), set_lower = c(-1, -1),
    set_upper = c(1, 1), outer_lower = c(-1, -1),
    outer_upper = c(1, 1), status = "bounded"
  )
  # the package prep needs mean-zero mean-sample residuals: one balancing
  # row joins the mean sample only, so the volatility rows keep the fixture's
  # exact residuals and crossings
  prep <- hetid::prepare_log_projection(
    c(w1, -sum(w1)), rbind(w2, -colSums(w2)), pcr,
    seq_len(nrow(w2) + 1L), qtr
  )
  list(
    w1 = w1, w2 = w2, proj = proj, qtr = qtr, b_tab = b_tab,
    est = logvar_logols_estimator(prep, qtr, w1, w2, pcr)
  )
}
# unit-ball joint set b1^2 + b2^2 <= 1 with the box [-1, 1]^2 as its hull; run
# the frozen oracle and the engine (benchmark configuration) on one fixture
orc_qs_ball <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
orc_run_pair <- function(fx, b_point, b_tab = fx$b_tab,
                         grid_n = 5L, grid_floor = 100L) {
  list(
    oracle = oracle_set_at_tau(
      orc_qs_ball, b_tab, fx$w1, fx$w2, fx$proj,
      b_point, grid_n, grid_floor, fx$qtr
    ),
    engine = logvar_engine_set_at_tau(fx$est, orc_qs_ball, b_tab,
      b_seed = b_point,
      grid_n = grid_n, grid_floor = grid_floor,
      cold_start_check = FALSE
    )
  )
}
# statuses and infinities exact; finite endpoints to roundoff, since the
# engine evaluates the package map (2 log|e| on a QR operator) and the oracle
# the frozen arithmetic
orc_same_table <- function(p) {
  e <- p$engine$table
  o <- p$oracle$table
  same_side <- function(a, b) {
    identical(is.finite(a), is.finite(b)) &&
      identical(a[!is.finite(a)], b[!is.finite(b)]) &&
      isTRUE(all.equal(a[is.finite(a)], b[is.finite(b)], tolerance = 1e-10))
  }
  identical(e$coef, o$coef) && identical(e$status, o$status) &&
    same_side(e$set_lower, o$set_lower) && same_side(e$set_upper, o$set_upper)
}
orc_n <- 12L
# every residual safely positive: no crossings, both sides bounded
orc_fx_safe <- orc_make_fixture(
  matrix(runif(orc_n * 2L, -0.3, 0.3), orc_n, 2L),
  5 + runif(orc_n)
)
orc_safe <- orc_run_pair(orc_fx_safe, c(0, 0))
check(
  "no-crossing ball: engine legacy table identical to the oracle",
  orc_same_table(orc_safe)
)
check(
  "no-crossing ball: engine n_cross and n_feasible match the oracle",
  identical(orc_safe$engine$n_cross, orc_safe$oracle$n_cross) &&
    identical(orc_safe$engine$n_feasible, orc_safe$oracle$n_feasible)
)
# one residual crosses zero inside the ball (row 1: w2 = (1, 0), w1 = 0, eps =
# -b1 changes sign); the all-positive intercept projection row diverges low and
# stays finite high -- one-sided rendering data
orc_w2_cross <- matrix(runif(orc_n * 2L, -0.25, 0.25), orc_n, 2L)
orc_w2_cross[1, ] <- c(1, 0)
orc_w1_cross <- 5 + runif(orc_n)
orc_w1_cross[1] <- 0
orc_cross <- orc_run_pair(orc_make_fixture(orc_w2_cross, orc_w1_cross), c(0.1, 0.1))
orc_i0 <- which(orc_cross$engine$schema$coef == "(Intercept)")
check(
  "single crossing: engine legacy table identical to the oracle",
  orc_same_table(orc_cross)
)
check(
  "single crossing: intercept lower diverges with a finite certified upper",
  orc_cross$engine$schema$lower_status[orc_i0] == "unbounded" &&
    is.finite(orc_cross$engine$schema$upper[orc_i0]) &&
    !is.na(orc_cross$engine$schema$upper[orc_i0])
)
# unresolved census: force every functional solve to fail so the one
# genuinely crossing row (w1 = 1.3 < sqrt(2)) needs the solver and fails closed
orc_w2_amb <- matrix(runif(orc_n * 2L, -0.25, 0.25), orc_n, 2L)
orc_w2_amb[1, ] <- c(1, 1)
orc_w1_amb <- 5 + runif(orc_n)
orc_w1_amb[1] <- 1.3
check("ambiguous row crosses the unit ball by direct geometry", 1.3 < sqrt(2))
orc_fx_amb <- orc_make_fixture(orc_w2_amb, orc_w1_amb)
orc_amb <- local({
  old <- solve_linear_functional_bound
  on.exit(assign("solve_linear_functional_bound", old, envir = globalenv()), add = TRUE)
  patched <- function(quadratic, objective_vec, direction, ...) {
    list(bound = NA_real_, bounded = FALSE, valid = FALSE)
  }
  assign("solve_linear_functional_bound", patched, envir = globalenv())
  orc_run_pair(orc_fx_amb, c(0, 0))
})
check(
  "unresolved census: engine fails closed identical to the oracle",
  orc_same_table(orc_amb) && all(orc_amb$oracle$table$status == "unreliable")
)
# upstream-status propagation: a non-bounded b_N axis returns the NA table with
# the propagated word (unbounded beats unreliable here), never a computed set
for (orc_st in c("unbounded", "unreliable")) {
  orc_tab <- orc_fx_safe$b_tab
  orc_tab$status <- c(orc_st, "bounded")
  orc_up <- orc_run_pair(orc_fx_safe, c(0, 0), b_tab = orc_tab)
  check(
    sprintf("upstream %s b_N axis: engine NA table identical to the oracle", orc_st),
    orc_same_table(orc_up) && all(orc_up$oracle$table$status == orc_st)
  )
}
# point identification failed: an NA-containing b_seed skips seed injection and
# the second polish start, matching the driver's !anyNA(b_point) guards
orc_na <- orc_run_pair(orc_fx_safe, c(NA_real_, NA_real_))
check(
  "point-identification failed (NA seed): engine identical to the oracle",
  orc_same_table(orc_na) &&
    identical(orc_na$engine$n_feasible, orc_na$oracle$n_feasible)
)
