# Helper function: evaluate one positive tau using public package bounds and sampled fits
structural_inference_positive <- function(fit, arrays, tau, frame, settings, retain) {
  mean_rows <- frame$panel == "mean"
  variance_rows <- frame$panel == "variance"
  dimension <- ncol(fit$w2)
  stopifnot(
    nrow(fit$beta2r) == dimension,
    identical(rownames(fit$beta2r), colnames(fit$w2)),
    identical(colnames(fit$beta2r), names(fit$beta1r))
  )
  built <- build_pipeline_quadratic_system(fit$gamma, rep(tau, dimension), fit$moments)
  box <- NULL
  box_reason <- "no tau-zero point for the public box search"
  box_error <- NULL
  center <- if (is.null(fit$point)) NULL else fit$point$theta
  if (!is.null(center)) {
    # the box refuses a center that is not strictly feasible, so check it first
    slack <- hetid::make_system_checker(built$quadratic)(center)
    if (all(is.finite(slack)) && all(slack < 0)) {
      attempt <- structural_inference_capture(
        hetid::compute_identified_set_box(fit, tau,
          n_grid = settings$n_grid,
          null_loading_rtol = 0
        ), "mean box"
      )
      box <- attempt$value
      box_error <- attempt$error
      box_reason <- if (is.null(box_error)) "public box search completed" else box_error$message
    } else {
      box_reason <- "tau-zero point is not strictly inside this positive-tau system"
    }
  }
  points <- if (is.null(center)) NULL else matrix(center, nrow = 1L)
  if (!is.null(box)) {
    candidates <- rbind(
      box$arg_lower, box$arg_upper,
      box$beta1_arg_lower, box$beta1_arg_upper
    )
    points <- rbind(points, candidates[stats::complete.cases(candidates), , drop = FALSE])
  }
  # beta1(theta) = beta1r - beta2r' theta, so its linear part loads -beta2r
  objectives <- cbind(diag(dimension), -fit$beta2r)
  colnames(objectives) <- c(colnames(fit$w2), names(fit$beta1r))
  evidence <- hetid::compute_quadratic_set_evidence(
    built$quadratic, objectives,
    points = points
  )
  frame[mean_rows, ] <- structural_inference_mean_rows(
    frame[mean_rows, ], box, evidence, fit, box_reason
  )
  sample <- NULL
  sample_error <- NULL
  if (is.null(box)) {
    status <- if (is.null(box_error)) "unreliable" else "failed"
    frame[variance_rows, ] <- structural_inference_mark(
      frame[variance_rows, ], status,
      paste("variance sampling unavailable:", box_reason)
    )
    if (!is.null(box_error)) {
      for (side in c("lower", "upper")) {
        affected <- mean_rows & frame[[paste0(side, "_geometry")]] == "bounded"
        frame[[paste0(side, "_status")]][affected] <- "failed"
        frame[[side]][affected] <- NA_real_
      }
    }
  } else {
    # the sampler reads the residuals the box carries, one per mean row, and
    # has no row argument. PPML runs on a copy whose residuals are the drawn
    # rows with PC_R, the same fitted mean system viewed on the variance
    # sample. quadratic, bounds and witnesses stay the full fit's, and the
    # retained box is the original
    view <- box
    view$w1 <- box$w1[arrays$variance_rows]
    view$w2 <- box$w2[arrays$variance_rows, , drop = FALSE]
    attempt <- structural_inference_capture(
      hetid::sample_log_variance_set(view, arrays$x_var,
        estimator = "ppml",
        n_points = settings$n_points
      ), "variance sample"
    )
    sample <- attempt$value
    sample_error <- attempt$error
    if (!is.null(sample_error)) {
      frame[variance_rows, ] <- structural_inference_mark(
        frame[variance_rows, ], "failed", sample_error$message
      )
    } else {
      frame[variance_rows, ] <- structural_inference_sample_rows(
        frame[variance_rows, ], sample
      )
    }
  }
  diagnostics <- list(
    tau = tau, box_reason = box_reason, box_error = box_error,
    mean_geometry = evidence$summary, nonempty = evidence$nonempty,
    mean_witnesses = if (is.null(box)) {
      NULL
    } else {
      structural_inference_witness_diagnostics(box, built$quadratic)
    },
    sample_error = sample_error,
    sample = if (is.null(sample)) {
      NULL
    } else {
      list(
        reason = sample$reason, n_attempted = attr(sample$bounds, "n_attempted"),
        n_failed = attr(sample$bounds, "n_failed"), fits = sample$fits
      )
    }
  )
  list(
    frame = frame, diagnostics = diagnostics,
    retained = if (retain) {
      list(
        box = box, sample = sample, evidence = evidence,
        quadratic = built$quadratic
      )
    } else {
      NULL
    }
  )
}

# Helper function: distinguish sampled PPML eligibility from full-set boundedness
structural_inference_sample_rows <- function(frame, sample) {
  bounds <- sample$bounds
  stopifnot(identical(bounds$term, frame$term))
  attempted <- attr(bounds, "n_attempted")
  failed <- attr(bounds, "n_failed")
  frame$n_attempted <- attempted
  frame$n_failed <- failed
  frame$lower_geometry <- frame$upper_geometry <- "not established for full variance set"
  finite <- all(is.finite(bounds$lower)) && all(is.finite(bounds$upper))
  # a failed fit would silently shrink the range, so any failure makes it ineligible
  good <- identical(sample$reason, "sampled") && attempted > 0L && failed == 0L && finite
  frame <- structural_inference_mark(
    frame, if (good) "bounded" else "unreliable",
    if (good) {
      "eligible sampled statistic; full-set extrema not established"
    } else {
      paste(
        "ineligible sampled statistic:", sample$reason, "failed fits", failed,
        "of", attempted
      )
    }
  )
  # an ineligible partial range stays visible for diagnosis
  if (finite) {
    frame$lower <- bounds$lower
    frame$upper <- bounds$upper
  }
  frame
}

# Helper function: retain public-checker boundary residuals without changing eligibility
structural_inference_witness_diagnostics <- function(box, quadratic) {
  checker <- hetid::make_system_checker(quadratic)
  do.call(rbind, lapply(c("lower", "upper"), function(side) {
    points <- rbind(box[[paste0("beta1_arg_", side)]], box[[paste0("arg_", side)]])
    data.frame(
      term = c(box$beta1_bounds$coef, box$bounds$coef), side = side,
      maximum_constraint_residual = apply(points, 1L, function(point) {
        if (all(is.finite(point))) max(checker(unname(point))) else NA_real_
      }), stringsAsFactors = FALSE
    )
  }))
}
