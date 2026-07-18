# The Harvey Gaussian multiplicative-heteroskedasticity estimator: the b-indexed
# map theta_hat_H(b) minimizing Q_H(theta; b) = 0.5 sum(eta + y exp(-eta)) on
# each candidate's squared residual. Holds the Jacobian, spec_id, start bundle,
# and estimator-engine estimator object (math, recession, solver sourced below).
paper_source_once(paper_path("log_variance", "estimators", "harvey", "likelihood.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "recession_certificate.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "solver.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "fit_contract.R"))

# The estimator-engine estimator object. Callable only after the driver stability
# precheck. Freezes X = cbind(1, pcr), factors X'X once and forwards it to
# every solve, builds the Lewbel-point ladder (PPML rung one, mean inside the
# solver, shifted log-OLS diagnostic), fits the point once, and exposes it.
logvar_harvey_estimator <- function(w1, w2, pcr, qtr, b_point = NULL,
                                    ppml_bundle = NULL,
                                    ppml_bundle_source_id = NULL,
                                    ppml_start_at_b = NULL,
                                    ppml_start_at_b_source_id = NULL,
                                    logols_coef = NULL,
                                    control = LOGVAR_HARVEY_CONTROL) {
  logvar_harvey_validate_policy(control)
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))
  chol_xx <- chol(crossprod(x_mat))
  ladder <- list()
  if (!is.null(ppml_bundle) && "variance_start" %in% ppml_bundle$valid_for) {
    ladder$ppml_point <- ppml_bundle$coef_original
  }
  if (!is.null(logols_coef)) {
    ladder$logols_shifted <-
      logols_coef +
      c(LOGVAR_NORMAL_LOG_SQUARE_GAP, rep(0, length(logols_coef) - 1L))
  }
  start_plan <- logvar_harvey_start_plan(ladder, control)
  start_identity <- logvar_harvey_start_identity(
    ppml_bundle,
    ppml_bundle_source_id,
    ppml_start_at_b,
    ppml_start_at_b_source_id,
    logols_coef
  )
  raw_point_fit <- NULL
  if (!is.null(b_point) && !anyNA(b_point)) {
    raw_point_fit <- logvar_harvey_fit(
      b_point, w1, w2, x_mat,
      start = start_plan$start,
      fallback_starts = start_plan$fallback_starts,
      chol_xx = chol_xx,
      auto_intercept = start_plan$auto_intercept,
      control = control
    )
  }
  accepted <- logvar_harvey_accepted(raw_point_fit)
  point_fit <- if (accepted) raw_point_fit else NULL
  # winning rung: the accepted start attempt (NA error class); NA when no point
  point_start_rung <- NA_character_
  attempts <- if (accepted) raw_point_fit$diagnostics$start_attempts else list()
  for (a in attempts) {
    if (is.na(a$error_class)) {
      point_start_rung <- as.character(a$source)
      break
    }
  }
  point_warm <- if (accepted) raw_point_fit$warm_start else NULL
  metadata <- list(
    estimator = "harvey", target_functional = "theta_var_gaussian",
    intercept_normalization = sprintf(
      paste(
        "log conditional variance (mean-log + %s under normality;",
        "absorbs 2 log|m_0|)"
      ),
      logvar_normal_gap_text(9L)
    ),
    sample_id = logvar_sample_id(qtr, w1, w2, pcr), smoothness = "smooth",
    inner_solver = "observed-Newton (Fisher fallback) + backtracking", response_scale = "variance",
    response_scale_value = 1, scale_reference = control$scaling_policy,
    spec_id = logvar_spec_id(c(
      logvar_flatten_spec(control, "control"),
      logvar_flatten_spec(start_identity, "start"),
      list(
        b_point = if (!is.null(b_point) && !anyNA(b_point)) {
          b_point
        } else {
          "null"
        }
      )
    )),
    fit_control = control,
    cold_start_rtol = control$cold_start_rtol
  )
  start_bundle <- if (accepted) {
    logvar_harvey_start_bundle(point_fit, metadata)
  } else {
    NULL
  }
  if (!is.null(start_bundle) &&
    !(identical(start_bundle$sample_id, metadata$sample_id) &&
      identical(start_bundle$spec_id, metadata$spec_id))) {
    stop("logvar_harvey_estimator: start bundle identity must match metadata")
  }
  list(
    metadata = metadata, coef_labels = colnames(x_mat), point_fit = point_fit,
    point_start_rung = point_start_rung, start_bundle = start_bundle,
    fit_at_b = function(b, start = NULL) {
      hv <- function(s, fallbacks, auto) {
        logvar_harvey_fit(b, w1, w2, x_mat,
          start = s, fallback_starts = fallbacks, chol_xx = chol_xx,
          auto_intercept = auto, control = control
        )
      }
      tagged <- function(f, lbl) {
        lapply(f$diagnostics$start_attempts, function(a) {
          a$source <- paste0(lbl, a$source)
          a
        })
      }
      # Execute the serialized stage policy exactly; each failed stage falls
      # through to the next named stage and successful stages stop the ladder.
      warm <- if (!is.null(start)) start else point_warm
      best <- hv_result(
        fit_status = "nonconvergence",
        error_class = "start_policy_exhausted"
      )
      trail <- list()
      for (stage in control$fit_stage_policy) {
        if (logvar_harvey_accepted(best)) break
        if (identical(stage, "warm") && !is.null(warm)) {
          best <- hv(warm, list(), FALSE)
          trail <- c(trail, tagged(best, "warm:"))
        } else if (identical(stage, "ppml_at_b") &&
          !is.null(ppml_start_at_b)) {
          pf <- tryCatch(ppml_start_at_b(b), error = function(cond) NULL)
          if (logvar_harvey_accepted(pf)) {
            attempt <- hv(pf$coef, list(), FALSE)
            trail <- c(trail, tagged(attempt, "ppml:"))
            if (logvar_harvey_accepted(attempt)) best <- attempt
          }
        } else if (identical(stage, "standalone")) {
          attempt <- hv(
            start_plan$start,
            start_plan$fallback_starts,
            start_plan$auto_intercept
          )
          trail <- c(trail, tagged(attempt, "standalone:"))
          best <- attempt
        }
      }
      best$diagnostics$start_attempts <- trail
      best
    },
    jacobian_at_b = function(b, fit = NULL) {
      if (is.null(fit)) {
        return(NULL)
      }
      logvar_harvey_jacobian(
        fit, b, w1, w2, x_mat, control
      )
    },
    analyze_domain = list(
      precheck = function(qs, b_tab) {
        st <- logvar_harvey_recession_self_test(x_mat, control)
        # engine contract: unresolved is a vector (empty = pass), not a logical
        flagged <- names(st$checks)[!st$checks]
        list(unresolved = flagged, n_flagged = length(flagged), info = st)
      },
      sides = function(qs, b_tab, scan) {
        nm <- function(v) {
          if (is.null(names(v))) colnames(x_mat)[seq_along(v)] else names(v)
        }
        list(
          lower_unbounded = setNames(rep(FALSE, length(scan$min)), nm(scan$min)),
          upper_unbounded = setNames(rep(FALSE, length(scan$max)), nm(scan$max)),
          unresolved_endpoints = character(0),
          closure_diagnostics = NULL,
          info = list(method = "fit_level_recession", candidate_specific = TRUE)
        )
      }
    )
  )
}
