# Tuning sensitivity for the regularized log projections (spec "Additive-log
# tuning and interpretation", "Fuller tuning and coefficient set"): every method
# at each multiplier of hetid::LOG_PROJECTION_CONTROL$SENSITIVITY_MULTIPLIERS,
# on identical candidate sets and samples, at the baseline slack only. The
# primary multiplier reuses the primary map. Definitions only; sourced by
# run_sets.R.

logvar_log_projection_tuning_rows <- function(mapped, tau_baseline) {
  table <- mapped$final[[paper_tau_key(tau_baseline)]]$table
  data.frame(
    id = mapped$estimator$metadata$estimator,
    multiplier = mapped$multiplier,
    coef = table$coef,
    point = unname(mapped$point[table$coef]),
    set_lower = table$set_lower,
    set_upper = table$set_upper,
    status = table$status,
    stringsAsFactors = FALSE
  )
}

logvar_log_projection_tuning <- function(primary_maps, ctx, tau_baseline) {
  ctrl <- hetid::LOG_PROJECTION_CONTROL
  rows <- list()
  extra_maps <- list()
  for (id in names(primary_maps)) {
    for (m in ctrl$SENSITIVITY_MULTIPLIERS) {
      mapped <- if (m == ctrl$MULTIPLIER) {
        primary_maps[[id]]
      } else {
        extra_maps[[length(extra_maps) + 1L]] <-
          logvar_log_projection_sets(id, m, tau_baseline, ctx)
      }
      rows[[length(rows) + 1L]] <- logvar_log_projection_tuning_rows(
        mapped, tau_baseline
      )
    }
  }
  list(table = do.call(rbind, rows), maps = extra_maps)
}
