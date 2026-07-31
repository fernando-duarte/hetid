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
# Widen the beta1 interval onto the same certified points the theta box was
# widened onto. beta1(w) = beta1R - beta2R'w is a linear image of the SAME set,
# so every accepted argmax is a feasible w whose image provably belongs in the
# beta1 interval. Without this the two blocks in one table are reported over
# different sets: theta over the refined set, beta1 over the origin-start one.
#
# Invisible under spec A, where beta2R is zero and every beta1 row is a point,
# which is why it survived until spec B made beta1 set-valued.
widen_beta1_from_args <- function(beta1_tab, beta1r, beta2r, args) {
  if (!length(args)) {
    return(beta1_tab)
  }
  bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
  # A loading that is zero in exact arithmetic can arrive as 1e-13. The PCs are
  # prcomp scores, so both blocks are mean-centred and the intercept's loading is
  # a KNOWN zero corrupted by lm()'s arithmetic. Testing it with == 0 passes under
  # spec A, where beta2R is a literal zero matrix, and fails under spec B.
  #
  # Widening such a row optimizes a constant functional: every certified point
  # maps to the same value in exact arithmetic, but sum(loading * w) differs in
  # the last bits across points, so min and max separate by ~1e-15. That is
  # enough to defeat the renderer's exact degeneracy test and print a spurious
  # [x, x] cell with a confidence interval beneath a row that is point
  # identified. Compare against the matrix scale so the test is scale-free.
  null_scale <- max(abs(beta2r)) * sqrt(.Machine$double.eps)
  for (k in seq_len(nrow(beta1_tab))) {
    p <- beta1_tab$coef[[k]]
    loading <- beta2r[, p]
    # a null column is point identification, not a wide interval to tighten
    if (!any(abs(loading) > null_scale) ||
      !identical(beta1_tab$status[[k]], bounded)) {
      next
    }
    vals <- vapply(
      args, function(w) unname(beta1r[[p]] - sum(loading * w)), numeric(1)
    )
    beta1_tab$set_lower[[k]] <- min(beta1_tab$set_lower[[k]], min(vals))
    beta1_tab$set_upper[[k]] <- max(beta1_tab$set_upper[[k]], max(vals))
  }
  beta1_tab
}

set_id_display_tau_refinement_full <- function(tau_display, seed_theta,
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
    refined[[paper_tau_key(tau)]] <- list(
      theta = widened$tab,
      beta1 = widen_beta1_from_args(it$beta1, beta1r, beta2r, widened$args)
    )
  }
  # Return keyed by each tau's canonical key in the caller's input order;
  # consumers index by name, so the ordering itself is immaterial
  refined[vapply(tau_display, paper_tau_key, character(1))]
}

# Theta-only view, for the figure callers that never look at beta1.
set_id_display_tau_refinement <- function(tau_display, seed_theta,
                                          gamma, moments, beta1r, beta2r) {
  lapply(
    set_id_display_tau_refinement_full(
      tau_display, seed_theta, gamma, moments, beta1r, beta2r
    ),
    `[[`, "theta"
  )
}
