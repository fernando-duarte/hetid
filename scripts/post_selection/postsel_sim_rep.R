# Per-replication worker for the post-selection split simulation.
# Decoupled from the driver's script-level constants via the cfg
# list (seed base, sizes, optimizer budget), so this file has no
# hidden lexical dependencies and stays independently sourceable.
#
# One replication: draw, split, select per window, record theta0
# membership for every arm (plus profile-bound states/widths for the
# deterministic leading subset of replications). All randomness is
# drawn inside draw_postsel_data under the rep seed; the optimizer
# re-seeds with the same rep seed, so results do not depend on
# mclapply scheduling.
run_one_rep <- function(rep_id, cell_idx, params, cfg) {
  seed_rep <- cfg$sim_seed + 10000L * cell_idx + rep_id
  dat <- draw_postsel_data(params, cfg$t_obs, seed = seed_rep)
  blocks <- split_block_rows(
    cfg$t_obs,
    prop = cfg$prop, gap = cfg$gap
  )
  n_comp <- length(params$a)
  tau <- rep(cfg$tau, n_comp)
  lam0 <- equal_weight_lambda(params$k_inst, n_comp)

  m_full <- sim_window_moments(dat, seq_len(cfg$t_obs))
  m_s <- sim_window_moments(dat, blocks$s_rows)
  m_e <- sim_window_moments(dat, blocks$e_rows)

  select_on <- function(m) {
    run_lambda_optimization(
      lam0, m, tau,
      n_starts = cfg$n_starts, seed = seed_rep,
      maxeval = cfg$maxeval
    )
  }
  sel_full <- select_on(m_full)
  sel_s <- select_on(m_s)
  sel_e <- select_on(m_e)

  arms <- list(
    fixed_full = list(
      lambda = lam0, moments = m_full, sel_obj = NA_real_
    ),
    fixed_e = list(
      lambda = lam0, moments = m_e, sel_obj = NA_real_
    ),
    opt_full = list(
      lambda = sel_full$lambda_optimized, moments = m_full,
      sel_obj = sel_full$objective_final
    ),
    split = list(
      lambda = sel_s$lambda_optimized, moments = m_e,
      sel_obj = sel_s$objective_final
    ),
    self_e = list(
      lambda = sel_e$lambda_optimized, moments = m_e,
      sel_obj = sel_e$objective_final
    )
  )
  do.call(rbind, lapply(names(arms), function(nm) {
    a <- arms[[nm]]
    row <- data.frame(
      k_inst = params$k_inst, phi = params$phi, rep = rep_id,
      arm = nm,
      covered = covers_theta0(
        a$lambda, tau, a$moments, params$theta0
      ),
      sel_objective = a$sel_obj,
      eval_state = NA_character_, total_width = NA_real_,
      stringsAsFactors = FALSE
    )
    if (rep_id <= cfg$width_reps) {
      ev <- evaluate_lambda_set(a$lambda, tau, a$moments)
      row$eval_state <- ev$system_state
      row$total_width <- ev$total_width
    }
    row
  }))
}
