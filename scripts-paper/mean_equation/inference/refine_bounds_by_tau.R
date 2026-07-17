# SLSQP extremization of theta_k from an arbitrary feasible start, in the shared
# solver's scaling (mirrors .solve_scaled, which pins the start at the origin);
# returns the theta-units bound and argmax, or NULL when the solve fails or the
# endpoint misses the feasible+active certificate. Sourced by
# compute_bounds_by_tau.R after profile_solver_core.R, so the .derive_* and
# .feasibility_residual helpers resolve at call time.
solve_theta_bound_from <- function(qs, k, direction, theta_start,
                                   box = 1e6, feas_tol = 1e-4) {
  if (is.null(theta_start)) {
    return(NULL)
  }
  delta <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, delta)
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
  res <- nloptr::slsqp(
    x0 = pmin(pmax(theta_start / delta, -box), box),
    fn = function(phi) sgn * sum(e_k * phi),
    gr = function(phi) sgn * e_k,
    lower = rep(-box, dim_theta), upper = rep(box, dim_theta),
    hin = function(phi) {
      theta <- delta * phi
      vapply(seq_along(qs$A_i), function(i) {
        (drop(t(theta) %*% qs$A_i[[i]] %*% theta) +
          sum(qs$b_i[[i]] * theta) + qs$c_i[i]) / omega[i]
      }, numeric(1))
    },
    hinjac = function(phi) {
      theta <- delta * phi
      t(vapply(seq_along(qs$A_i), function(i) {
        (delta * (2 * drop(qs$A_i[[i]] %*% theta) + qs$b_i[[i]])) / omega[i]
      }, numeric(dim_theta)))
    },
    control = list(xtol_rel = 1e-8, maxeval = 1000),
    deprecatedBehavior = FALSE
  )
  if (any(!is.finite(res$par))) {
    return(NULL)
  }
  theta <- delta * res$par
  resid <- .feasibility_residual(qs, theta, omega)
  if (!is.finite(resid) || abs(resid) > feas_tol) {
    return(NULL)
  }
  list(bound = theta[k], theta = theta)
}

# Pure display-tau refinement of the mean-equation theta intervals, called by
# estimate_identified_set.R to widen set_tables onto the boxes the paper
# publishes and compute_bounds_by_tau.R re-keys for the log-variance census.
# It keeps its own warm chain rather than borrowing the grid walk's: that walk
# carries warm state in a closure it mutates through `<<-`, and that state's
# final argmax args live at the *largest* grid tau; because the identified set
# grows with tau (nesting), a start feasible at the largest tau can be
# infeasible at a smaller display tau, so reusing the walk's warm args would
# violate the solver's feasible-start contract. The chain here is seeded from
# the tau = 0 Lewbel point (the whole set at tau = 0, hence feasible at every
# tau by nesting) and walks the display taus in *increasing* order so each
# accepted argmax hands a still-feasible start to the next larger tau. It reads
# and writes no global, prints nothing, and leaves its inputs untouched.
#
# solve_fn is solve_theta_bound_from (defined above), passed in rather than
# reimplemented: solve_fn(qs, k, direction, theta_start) returns
# list(bound, theta) or NULL, and NULL on a NULL start -- the same cold-start
# rule the main walk relies on when the tau = 0 point is unavailable.
set_id_display_tau_refinement <- function(tau_display, seed_theta, solve_fn,
                                          gamma, moments, beta1r, beta2r) {
  # a NULL or NA seed leaves every warm entry empty, so the first solve at each
  # coefficient/side gets a NULL start and solve_fn returns NULL -- mirroring
  # the main walk's rule that an unavailable tau = 0 point yields cold starts
  seed <- if (is.null(seed_theta) || anyNA(seed_theta)) NULL else seed_theta
  warm <- NULL
  refined <- list()
  for (tau in sort(tau_display)) {
    qs <- tau_quadratic_system(gamma, tau, moments)
    it <- coef_interval_tables(gamma, tau, moments, beta1r, beta2r)
    theta_tab <- it$theta
    if (is.null(warm)) {
      # K = the theta coefficient axis, taken from the first table's rows
      k_theta <- nrow(theta_tab)
      warm <- list(
        min = rep(list(seed), k_theta),
        max = rep(list(seed), k_theta)
      )
    }
    for (k in seq_len(nrow(theta_tab))) {
      for (side in c("min", "max")) {
        cand <- solve_fn(qs, k, side, warm[[side]][[k]])
        if (is.null(cand)) next
        # carry the argmax forward even when the row is not certified bounded,
        # so the next larger tau still gets a feasible warm start
        warm[[side]][[k]] <- cand$theta
        if (theta_tab$status[k] != "bounded") next
        # keep-if-extends: replace an endpoint only when the warm solve widens
        # the interval, exactly the main grid walk's acceptance rule
        if (side == "max" && cand$bound > theta_tab$set_upper[k]) {
          theta_tab$set_upper[k] <- cand$bound
        } else if (side == "min" && cand$bound < theta_tab$set_lower[k]) {
          theta_tab$set_lower[k] <- cand$bound
        }
      }
    }
    refined[[sprintf("%.17g", tau)]] <- theta_tab
  }
  # return keyed by each tau's own %.17g key in the caller's input order;
  # consumers index by name, so the ordering itself is immaterial
  refined[sprintf("%.17g", tau_display)]
}
