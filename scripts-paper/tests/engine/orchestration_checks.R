# Injected smooth and LAD-shaped display-tau orchestration fixtures.

orchestration_taus <- c(0.2, 0.1)
orchestration_keys <- vapply(
  orchestration_taus,
  paper_tau_key,
  character(1)
)
orchestration_boxes <- stats::setNames(
  lapply(orchestration_taus, function(tau) {
    list(lower = rep(-tau, 2L), upper = rep(tau, 2L))
  }),
  orchestration_keys
)
orchestration_labels <- c("beta1", "beta2")

orchestration_mapper <- function(kind) {
  force(kind)
  function(tau, key, quadratic, box, state) {
    input_history <- state$history
    next_state <- list(
      kind = kind,
      history = c(input_history, key)
    )
    detail <- list(kind = kind, box = box)
    if (identical(kind, "lad")) {
      next_state$warm <- list(
        path = matrix(c(-tau, tau), nrow = 1L)
      )
      detail$paths <- list(
        list(key = key, warm = next_state$warm)
      )
    }
    table <- data.frame(
      coef = orchestration_labels,
      set_lower = c(tau, tau + 1),
      set_upper = c(tau + 2, tau + 3),
      status = rep("bounded", 2L),
      stringsAsFactors = FALSE
    )
    list(
      result = list(
        table = table,
        schema = list(kind = kind, key = key),
        n_feasible = as.integer(round(100 * tau)),
        diagnostics = list(
          kind = kind,
          key = key,
          state_in = input_history,
          quadratic_tau = quadratic$tau
        )
      ),
      state = next_state,
      detail = detail
    )
  }
}

run_orchestration_fixture <- function(kind) {
  mapped <- logvar_map_display_taus(
    taus = orchestration_taus,
    bounds_tau = orchestration_boxes,
    quadratic_at_tau = function(tau) list(tau = tau),
    map_one = orchestration_mapper(kind),
    initial_state = list(kind = kind, history = "initial")
  )
  core <- logvar_set_result_core(
    qtr = 1:4,
    sample_id = paste0("sample-", kind),
    coef_labels = orchestration_labels,
    reference = c(beta1 = -1, beta2 = -2),
    point = c(beta1 = 1, beta2 = 2),
    baseline_tau = 0.1,
    primary_results = mapped$results,
    final_results = mapped$results,
    w1 = 1:4,
    w2 = matrix(1:8, nrow = 4L),
    search_seed = c(0, 0)
  )
  list(mapped = mapped, core = core)
}

orchestration_runs <- lapply(
  c("smooth", "lad"),
  run_orchestration_fixture
)
names(orchestration_runs) <- c("smooth", "lad")

for (kind in names(orchestration_runs)) {
  mapped <- orchestration_runs[[kind]]$mapped
  first_key <- orchestration_keys[[1L]]
  second_key <- orchestration_keys[[2L]]
  check(
    sprintf("%s mapper preserves canonical key order", kind),
    identical(mapped$keys, orchestration_keys) &&
      identical(names(mapped$records), orchestration_keys) &&
      identical(names(mapped$boxes), orchestration_keys)
  )
  check(
    sprintf("%s mapper propagates its warm state", kind),
    identical(
      mapped$results[[second_key]]$diagnostics$state_in,
      mapped$records[[first_key]]$state$history
    ) &&
      identical(
        mapped$final_state$history,
        c("initial", first_key, second_key)
      )
  )
  check(
    sprintf("%s mapper retains tau, key, box, and quadratic records", kind),
    identical(mapped$records[[first_key]]$tau, orchestration_taus[[1L]]) &&
      identical(mapped$records[[first_key]]$key, first_key) &&
      identical(mapped$records[[first_key]]$box, orchestration_boxes[[first_key]]) &&
      identical(mapped$records[[first_key]]$quadratic$tau, orchestration_taus[[1L]])
  )
}

expected_core_fields <- c(
  "sample", "sample_id", "tau_baseline", "table", "sets",
  "schema", "n_feasible", "counts", "w1", "w2", "search_seed"
)
baseline_key <- paper_tau_key(0.1)
check(
  "set-result cores expose one common field contract",
  all(vapply(orchestration_runs, function(run) {
    identical(names(run$core), expected_core_fields)
  }, logical(1)))
)
check(
  "set-result baseline selection is by tau value after reordering",
  all(vapply(orchestration_runs, function(run) {
    identical(run$core$tau_baseline, 0.1) &&
      identical(
        run$core$table$set_lower,
        run$core$sets[[baseline_key]]$set_lower
      ) &&
      identical(run$core$table$set_lower, c(0.1, 1.1))
  }, logical(1)))
)

missing_key <- orchestration_keys[[2L]]
missing_box_error <- tryCatch(
  {
    logvar_map_display_taus(
      orchestration_taus,
      orchestration_boxes[1L],
      function(tau) list(tau = tau),
      orchestration_mapper("smooth"),
      list(kind = "smooth", history = "initial")
    )
    ""
  },
  error = conditionMessage
)
check(
  "display-tau mapping fails loudly when a box is missing",
  identical(
    missing_box_error,
    sprintf("Missing mean-equation box for tau %s", missing_key)
  )
)
