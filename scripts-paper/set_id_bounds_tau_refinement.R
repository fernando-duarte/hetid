# Pure display-tau refinement of the mean-equation theta intervals, factored
# out of set_id_bounds_tau.R so the figure's display taus can be warm-start
# refined without disturbing the main grid walk. The main walk carries its warm
# state in a closure it mutates through `<<-`, and that state's final argmax
# args live at the *largest* grid tau; because the identified set grows with
# tau (nesting), a start feasible at the largest tau can be infeasible at a
# smaller display tau, so reusing the walk's warm args here would violate the
# solver's feasible-start contract. This helper instead keeps its own local
# warm chain, seeded from the tau = 0 Lewbel point (the whole set at tau = 0,
# hence feasible at every tau by nesting), and walks the display taus in
# *increasing* order so each accepted argmax hands a still-feasible start to the
# next larger tau. It reads and writes no global, prints nothing, and leaves its
# inputs untouched.
#
# solve_fn is set_id_bounds_tau.R's solve_theta_bound_from, passed in rather
# than reimplemented: solve_fn(qs, k, direction, theta_start) returns
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
