# Helper function: evaluate both equation panels from one aligned numeric sample
structural_inference_fit <- function(prepared, settings, index = NULL,
                                     draw_id = 0L, retain = TRUE) {
  stopifnot(
    identical(settings, prepared$settings),
    length(draw_id) == 1L, is.finite(draw_id), draw_id >= 0, draw_id == floor(draw_id),
    is.logical(retain), length(retain) == 1L, !is.na(retain)
  )
  # hetid seeds its recession-direction search but keeps the caller's RNG kind,
  # so fix the kind for reproducible draws and hand the caller back its own
  # state, including an absent seed
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(
    {
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  do.call(RNGkind, as.list(paper_mbb_protocol()$rng_kind))
  if (is.null(index)) index <- seq_len(prepared$n_obs)
  arrays <- structural_inference_arrays(prepared, index)
  frame <- structural_inference_axis(prepared, settings)
  attempt <- structural_inference_capture(
    hetid::compute_tau0_system(arrays$y, arrays$y2, arrays$x, arrays$z), "mean fit"
  )
  fit <- attempt$value
  diagnostics <- list(draw_id = draw_id, mean_error = attempt$error, rng_kind = RNGkind())
  if (is.null(fit)) {
    frame <- structural_inference_mark(frame, "failed", attempt$error$message)
    return(list(frame = frame, diagnostics = diagnostics, retained = NULL))
  }
  # the mean fit keeps every drawn row in order, so the variance rows index it
  stopifnot(length(fit$w1) == length(arrays$y), nrow(fit$w2) == length(arrays$y))
  zero <- frame$tau == 0
  mean_zero <- zero & frame$panel == "mean"
  variance_zero <- zero & frame$panel == "variance"
  logvar <- NULL
  if (is.null(fit$point)) {
    frame[zero, ] <- structural_inference_mark(
      frame[zero, ],
      "unreliable", "mean system has no unique consistent tau-zero point"
    )
  } else {
    # named after the news columns so the log-variance fit's order guard is live
    theta <- stats::setNames(fit$point$theta, colnames(fit$w2))
    frame[mean_zero, ] <- structural_inference_point_rows(
      frame[mean_zero, ],
      c(fit$beta1, theta)
    )
    # the full mean fit's residuals, on the drawn rows with PC_R. an
    # overflowing squared residual is a numerical failure of this draw, not
    # the argument error hetid would raise for it
    w1 <- fit$w1[arrays$variance_rows]
    w2 <- fit$w2[arrays$variance_rows, , drop = FALSE]
    if (!all(is.finite(drop(w1 - w2 %*% theta)^2))) {
      logvar_attempt <- list(value = NULL, error = list(
        stage = "variance point", message = "Squared point residual exceeds numeric range",
        classes = "structural_inference_numeric_failure"
      ))
    } else {
      logvar_attempt <- structural_inference_capture(
        hetid::fit_log_variance_at_b(theta, w1, w2, arrays$x_var,
          estimator = "ppml"
        ), "variance point"
      )
    }
    logvar <- logvar_attempt$value
    if (structural_inference_fit_ok(logvar)) {
      frame[variance_zero, ] <- structural_inference_point_rows(
        frame[variance_zero, ], logvar$coef
      )
    } else {
      reason <- if (is.null(logvar_attempt$error)) {
        paste("tau-zero PPML fit:", logvar$diagnostics$error_class)
      } else {
        logvar_attempt$error$message
      }
      frame[variance_zero, ] <- structural_inference_mark(frame[variance_zero, ], "failed", reason)
    }
    diagnostics$variance_point_error <- logvar_attempt$error
  }
  diagnostics$tau_zero <- if (is.null(logvar)) NULL else logvar$diagnostics
  diagnostics$variance_center <- attr(arrays$x_var, "scaled:center")
  diagnostics$variance_rows <- list(
    source = arrays$variance_source,
    position = which(arrays$variance_rows)
  )
  positive <- lapply(settings$taus, function(tau) {
    structural_inference_positive(
      fit, arrays, tau, frame[frame$tau == tau, ],
      settings, retain
    )
  })
  names(positive) <- sprintf("tau=%.2f", settings$taus)
  for (result in positive) {
    row <- match(result$frame$coef, frame$coef)
    stopifnot(!anyNA(row))
    frame[row, ] <- result$frame
  }
  diagnostics$positive <- lapply(positive, `[[`, "diagnostics")
  list(
    frame = frame, diagnostics = diagnostics,
    retained = if (retain) {
      list(
        mean = fit, logvar_point = logvar, x_var = arrays$x_var,
        positive = lapply(positive, `[[`, "retained")
      )
    } else {
      NULL
    }
  )
}
