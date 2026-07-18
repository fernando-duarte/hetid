# LAD-specific display-tau callback for the shared set-map iterator.

logvar_lad_display_mapper <- function(
  estimator,
  config,
  cache,
  w1,
  w2,
  x_mat,
  e_scale_ref,
  coef_labels,
  qtr,
  sample_id,
  spec_id,
  search_seed
) {
  function(tau, key, quadratic, box, state) {
    at_tau <- logvar_lad_map_tau(
      estimator,
      quadratic,
      box,
      tau,
      config,
      cache
    )
    result <- at_tau$res
    delta <- .derive_theta_scale(quadratic)
    omega <- .derive_constraint_scales(quadratic, delta)
    geometry <- list(
      w1 = w1,
      w2 = w2,
      x_mat = x_mat,
      e_scale_ref = e_scale_ref,
      delta = delta,
      omega = omega,
      check_feasible = local({
        qq <- quadratic
        om <- omega
        function(b) {
          value <- .feasibility_residual(qq, b, om)
          list(
            feasible =
              value <= PAPER_QUADRATIC_CONTROL$admission_tolerance,
            max_violation = value
          )
        }
      })
    )
    paths <- logvar_lad_probe_report(
      result,
      tau,
      estimator,
      geometry,
      coef_labels,
      qtr
    )
    precheck <- result$domain_info$precheck
    witnesses <- if (is.null(precheck)) {
      list()
    } else {
      .lad_or(precheck$info$witnesses, list())
    }
    list(
      result = result,
      state = NULL,
      detail = list(
        audit = at_tau$audit,
        paths = paths,
        closure = logvar_lad_closure_rows(
          paths,
          "lad",
          sample_id,
          spec_id
        ),
        witness_coverage = list(
          n_witnesses = length(witnesses),
          n_flagged = if (is.na(result$n_cross)) 0L else result$n_cross,
          n_unresolved = if (is.null(precheck)) {
            0L
          } else {
            length(precheck$unresolved)
          },
          n_paths = sum(vapply(
            witnesses,
            function(witness) length(witness$anchors),
            integer(1)
          ))
        ),
        min_eps = logvar_min_feasible_eps(
          result$schema,
          w1,
          w2,
          search_seed
        )
      )
    )
  }
}
