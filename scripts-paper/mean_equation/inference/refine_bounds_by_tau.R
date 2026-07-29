# Display-tau refinement of the mean-equation news intervals.

paper_source_once(paper_path(
  "mean_equation", "inference", "theta_box_multistart.R"
))

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
# Each tau's widening is widen_theta_box (theta_box_multistart.R), which adds
# the axis pool and a cross-seeding round to the chain: one chain alone stays on
# the branch it starts on and clips the set near tau*.
set_id_display_tau_refinement <- function(tau_display, seed_theta,
                                          gamma, moments, beta1r, beta2r) {
  # a NULL or NA seed just leaves the pool without its tau = 0 member; the axis
  # starts still run, so an unavailable Lewbel point no longer means no warm
  # solve at all
  warm <- if (is.null(seed_theta) || anyNA(seed_theta)) {
    list()
  } else {
    list(seed_theta)
  }
  refined <- list()
  for (tau in sort(tau_display)) {
    qs <- tau_quadratic_system(gamma, tau, moments)
    it <- coef_interval_tables(gamma, tau, moments, beta1r, beta2r)
    widened <- widen_theta_box(qs, it$theta, warm)
    # carry every accepted argmax forward, including ones from rows that are not
    # certified bounded, so the next larger tau still gets feasible warm starts
    warm <- widened$args
    refined[[paper_tau_key(tau)]] <- widened$tab
  }
  # Return keyed by each tau's canonical key in the caller's input order;
  # consumers index by name, so the ordering itself is immaterial
  refined[vapply(tau_display, paper_tau_key, character(1))]
}
