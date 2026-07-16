# The GMM-owned search budget for the joint log-variance system (joint-GMM,
# logvar-joint-gmm, dossier section 6). logvar_joint_budget_state is a mutable
# environment stamped by tau, moment block, joint_input_id, and spec_id, holding
# the seven phase counters as a named integer vector with a debit mutator and a
# cap-based exhaustion test. Cache hits are recorded but never debit a cap or
# exhaust, so a warm replay can never starve the fresh search. Definitions only;
# sourced by the joint-GMM test entrypoint and the search/projection driver.

# Build the budget environment. counts starts at zero on every phase; caps is an
# optional named list of integer ceilings; debit adds to a counter and returns
# the new value; is_exhausted is TRUE once a capped counter reaches its ceiling,
# except cache_hit, which is uncapped by contract.
logvar_joint_budget_state <- function(tau, block, joint_input_id, spec_id,
                                      caps = NULL) {
  counter_names <- c(
    "grid_eval", "ppml_seed_fit", "candidate_eval", "stage_b_solve",
    "stage_c_solve", "fresh_verify", "cache_hit"
  )
  state <- new.env(parent = emptyenv())
  counts <- rep(0L, length(counter_names))
  names(counts) <- counter_names
  state$counts <- counts
  state$caps <- caps
  state$stamp <- list(
    tau = tau, block = block, joint_input_id = joint_input_id, spec_id = spec_id
  )
  state$debit <- function(name, k = 1L) {
    if (!name %in% counter_names) {
      stop(sprintf("logvar_joint_budget_state: unknown counter '%s'", name))
    }
    state$counts[[name]] <- state$counts[[name]] + as.integer(k)
    invisible(state$counts[[name]])
  }
  state$is_exhausted <- function(name) {
    if (identical(name, "cache_hit")) {
      return(FALSE)
    }
    cap <- state$caps[[name]]
    if (is.null(cap)) {
      return(FALSE)
    }
    state$counts[[name]] >= cap
  }
  state
}
