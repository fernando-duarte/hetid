# Multistart widening of the news (theta) interval table over one quadratic
# system. coef_interval_tables starts every profile solve at the origin, and on
# this non-convex set SLSQP settles on whichever local vertex it reaches from
# there; a single warm chain re-solves from one earlier argmax and so stays on
# the branch that chain is already on. Near tau* the missed branch is large --
# the reported news box clipped the set by a factor of four at the last figure
# grid tau -- and every consumer of these boxes (the log-variance grids of all
# four estimators, the bounds-by-tau figures, the fitted-volatility sweep) then
# searches a region smaller than the set it is contracted to cover.
#
# The fix is deterministic and problem-derived: solve every coefficient/side from
# a pool of starts, then re-seed from the argmaxes that round accepted and solve
# again. The vertex attaining one coordinate's extreme is routinely the start
# from which SLSQP reaches another coordinate's, so this cross-seeding round is
# what recovers the missed branch.
#
# This file is self-contained on purpose: refine_bounds_by_tau.R sources it for
# the mean-equation walk, and bootstrap_stage_draw.R sources it directly for the
# per-draw geometry, which runs without the mean-equation stage having gone
# first. It therefore sources its own solver core rather than assuming a caller
# has already put those helpers in scope.
paper_source_once(paper_path(
  "support", "identification", "profile_solver_core.R"
))

# SLSQP extremization of theta_k from an arbitrary feasible start, in the shared
# solver's scaling (mirrors .solve_scaled, which pins the start at the origin);
# returns the theta-units bound and argmax, or NULL when the solve fails or the
# endpoint misses the feasible+active certificate.
solve_theta_bound_from <- function(qs, k, direction, theta_start,
                                   box =
                                     PAPER_QUADRATIC_CONTROL$solver_boxes[[1L]],
                                   feas_tol =
                                     PAPER_QUADRATIC_CONTROL$feasibility_tolerance) {
  if (is.null(theta_start)) {
    return(NULL)
  }
  sgn <- if (direction == "min") 1 else -1
  dim_theta <- ncol(qs$A_i[[1]])
  e_k <- numeric(dim_theta)
  e_k[k] <- 1
  # Unwrapped: slsqp reports an ordinary failure -- an infeasible start, an
  # unbounded objective -- through $convergence, never by raising, so the only
  # conditions it can raise are contract breaches (a nonfinite x0, a nonfinite
  # objective at x0, a jacobian of the wrong shape). None is reachable here:
  # .derive_theta_scale returns a finite positive delta and .derive_constraint_
  # scales a finite positive omega, theta_start is finite by the caller's guard,
  # and the objective is linear in phi. A catch would only mask a defect.
  delta <- .derive_theta_scale(qs)
  res <- solve_scaled_quadratic_program(
    quadratic = qs,
    x0 = theta_start,
    objective = function(theta) {
      sgn * sum(e_k * theta)
    },
    gradient = function(theta) sgn * e_k,
    lower = rep(-delta * box, dim_theta),
    upper = rep(delta * box, dim_theta),
    method = "slsqp",
    objective_scale = "variable",
    catch_errors = FALSE
  )
  if (any(!is.finite(res$theta))) {
    return(NULL)
  }
  theta <- res$theta
  resid <- res$feasibility_residual
  if (!is.finite(resid) || abs(resid) > feas_tol) {
    return(NULL)
  }
  list(bound = theta[k], theta = theta)
}

.theta_start_key <- function(point) {
  paste(
    signif(point, PAPER_QUADRATIC_CONTROL$box_multistart_dedup_digits),
    collapse = "|"
  )
}

.dedup_theta_starts <- function(points) {
  points <- Filter(
    function(point) !is.null(point) && length(point) && !anyNA(point),
    points
  )
  points[!duplicated(vapply(points, .theta_start_key, character(1)))]
}

# Origin (the start behind the table being widened), the solver's own theta
# length scale along each axis in both directions, and whatever the caller
# carries in. The axis starts are what pull SLSQP off the origin's branch.
theta_box_start_pool <- function(qs, warm = NULL) {
  dim_theta <- ncol(qs$A_i[[1L]])
  delta <- .derive_theta_scale(qs)
  axes <- unlist(
    lapply(seq_len(dim_theta), function(k) {
      axis <- numeric(dim_theta)
      axis[[k]] <- delta
      list(axis, -axis)
    }),
    recursive = FALSE
  )
  .dedup_theta_starts(c(list(numeric(dim_theta)), axes, warm))
}

# Widen theta_tab over qs. An endpoint moves only on a certified feasible theta
# outside the current interval, so this only ever adds points the set provably
# contains; uncertified rows keep their status and are never widened. Rounds
# continue while a round produces starts no earlier round has been solved from,
# capped by box_multistart_rounds. Do NOT stop on "no endpoint moved this
# round": the axis round is routinely flat while the cross-seeding round it
# feeds is the one that recovers the branch. Returns the widened table and the
# accepted argmaxes, which the caller carries forward as the next tau's warm
# pool (feasible there by nesting when taus are walked in increasing order).
widen_theta_box <- function(qs, theta_tab, warm = NULL,
                            max_rounds =
                              PAPER_QUADRATIC_CONTROL$box_multistart_rounds) {
  bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
  queue <- theta_box_start_pool(qs, warm)
  solved <- character(0)
  accepted <- list()
  for (round in seq_len(max_rounds)) {
    queue <- Filter(function(point) !.theta_start_key(point) %in% solved, queue)
    if (!length(queue)) break
    solved <- c(solved, vapply(queue, .theta_start_key, character(1)))
    found <- list()
    for (k in seq_len(nrow(theta_tab))) {
      for (side in c("min", "max")) {
        for (theta_start in queue) {
          cand <- solve_theta_bound_from(qs, k, side, theta_start)
          if (is.null(cand)) next
          found[[length(found) + 1L]] <- cand$theta
          if (theta_tab$status[k] != bounded) next
          if (side == "max" && cand$bound > theta_tab$set_upper[k]) {
            theta_tab$set_upper[k] <- cand$bound
          } else if (side == "min" && cand$bound < theta_tab$set_lower[k]) {
            theta_tab$set_lower[k] <- cand$bound
          }
        }
      }
    }
    accepted <- .dedup_theta_starts(c(accepted, found))
    queue <- .dedup_theta_starts(found)
  }
  list(tab = theta_tab, args = accepted)
}

# The widening interval-table builder, in the shape set_id_boot_geometry expects.
# Bootstrap draws must build their news boxes exactly as the point estimate does:
# a draw whose box is the raw origin-start table is clipped the same way the
# full-sample table was, and the engine's box-escape guard then correctly marks
# the escaping sides unreliable -- which at tau = 0.2 pushed the bounded share of
# draws below the envelope's stability gate and suppressed every confidence cell
# in that column. Widening here removes the cause rather than the symptom.
# Deliberately NOT folded into coef_interval_tables_from_quadratic: that function
# also drives the tau* sweep, where widening would move the estimated transition.
coef_interval_tables_widened <- function(qs, beta1r, beta2r) {
  tables <- coef_interval_tables_from_quadratic(qs, beta1r, beta2r)
  tables$theta <- widen_theta_box(qs, tables$theta)$tab
  tables
}
