# Execution and ownership checks for the canonical inference/search control.

paper_source_once(paper_path(
  "support", "identification", "identified_set_inference.R"
))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path(
  "log_variance", "inference", "set_envelope.R"
))

inference_control <- PAPER_INFERENCE_SEARCH_CONTROL
check(
  "inference root solvers retain semantically distinct controls",
  !identical(
    inference_control$im_root$tolerance,
    inference_control$stoye_root$tolerance
  )
)
check(
  "bootstrap failure and sensitivity shares execute through shared helpers",
  paper_bootstrap_failure_limit(200L) == 50L &&
    paper_bootstrap_sensitivity_reps(100L) == 50L &&
    paper_bootstrap_sensitivity_reps(800L) == 200L
)
check("IM root brackets execute from the canonical control", {
  narrow <- inference_control
  narrow$im_root$bracket_upper <- 1
  failed <- tryCatch(
    {
      im_critical(0, 1, 1, control = narrow)
      FALSE
    },
    error = function(e) TRUE
  )
  failed && is.finite(im_critical(0, 1, 1))
})
check("BVN degeneracy cutoffs execute from the canonical control", {
  early <- inference_control
  early$bvn$degenerate_correlation <- 0.1
  actual <- pbvn_le_ge(0.4, -0.2, 0.2, early)
  expected <- stats::pnorm(0.4) - stats::pnorm(-0.2)
  isTRUE(all.equal(actual, expected))
})
check("robust endpoint minimum pairs execute from the canonical control", {
  lower <- seq_len(7L)
  upper <- lower + c(0.1, -0.1, 0.2, -0.2, 0.1, -0.1, 0.2)
  permissive <- inference_control
  permissive$robust_endpoint_correlation$minimum_pairs <- 4L
  is.na(robust_endpoint_cor(lower, upper)) &&
    is.finite(robust_endpoint_cor(lower, upper, permissive))
})
check(
  "tau-star and log-variance defaults derive from the canonical control",
  eval(formals(fine_tau_grid)$n_fine) ==
    inference_control$tau_star$fine_grid_points &&
    eval(formals(tau_star_fixed)$iters) ==
      inference_control$tau_star$bisection_iterations &&
    eval(formals(logvar_endpoint_envelope)$stability) ==
      inference_control$logvar_endpoint$stability_share
)

threaded_fields <- list(
  "support/identification/inference_calibration.R" = c(
    "im_root", "stoye_root", "bvn", "robust_endpoint_correlation"
  ),
  "support/identification/tau_star.R" = c(
    "fine_grid_points", "bisection_iterations"
  ),
  "support/identification/identified_set_bootstrap.R" =
    "bootstrap_bisection_iterations",
  "mean_equation/inference/run_bootstrap.R" = c(
    "paper_bootstrap_failure_limit", "progress_report_every"
  ),
  "log_variance/inference/run_set_bootstrap.R" = c(
    "paper_bootstrap_failure_limit", "paper_bootstrap_sensitivity_reps",
    "stability_share"
  ),
  "log_variance/inference/set_envelope.R" = "stability_share"
)
threaded_code <- vapply(
  names(threaded_fields),
  function(path) paste(readLines(paper_path(path), warn = FALSE), collapse = "\n"),
  character(1)
)
threaded <- vapply(names(threaded_fields), function(path) {
  all(vapply(
    threaded_fields[[path]],
    grepl,
    logical(1),
    x = threaded_code[[path]],
    fixed = TRUE
  ))
}, logical(1))
check(
  "every inference/search policy is threaded from its sole owner",
  all(threaded)
)
retired_literals <- c(
  "interval = c(0, 10)", "interval = c(1e-6, 8)",
  "rho > 0.999", "rho < -0.999", "rel.tol = 1e-9",
  "n_fine = 20L", "iters = 40L", "iters = 15L",
  "boot_reps %/% 4L", "max(50L", "stability = 0.85", "b %% 25L"
)
check(
  "inference consumers do not restate their retired policy literals",
  !any(vapply(
    retired_literals,
    function(literal) any(grepl(literal, threaded_code, fixed = TRUE)),
    logical(1)
  ))
)

rm(
  inference_control, threaded_fields, threaded_code, threaded,
  retired_literals
)
stopifnot(
  identical(
    names(boot_band(c(1, 2, 3))),
    c("median", "lower", "upper", "n")
  ),
  identical(
    boot_min_reps(200L),
    ceiling(
      200L *
        PAPER_ANALYSIS_CONTRACT$inference$minimum_valid_draw_share
    )
  )
)
