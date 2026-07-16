# The Harvey Gaussian multiplicative-heteroskedasticity estimator: the b-indexed
# map theta_hat_H(b) minimizing Q_H(theta; b) = 0.5 sum(eta + y exp(-eta)) on
# each candidate's squared residual. Holds the Jacobian, spec_id, start bundle,
# and estimator-engine estimator object (math, recession, solver sourced below).
source(paper_path("log_variance", "estimators", "harvey", "likelihood.R"))
source(paper_path("log_variance", "estimators", "harvey", "recession_certificate.R"))
source(paper_path("log_variance", "estimators", "harvey", "solver.R"))
source(paper_path("log_variance", "estimators", "harvey", "fit_contract.R"))

# The estimator-engine estimator object. Callable only after the driver stability
# precheck. Freezes X = cbind(1, pcr), factors X'X once and forwards it to
# every solve, builds the Lewbel-point ladder (PPML rung one, mean inside the
# solver, shifted log-OLS diagnostic), fits the point once, and exposes it.
logvar_harvey_estimator <- function(w1, w2, pcr, qtr, b_point = NULL,
                                    ppml_bundle = NULL, ppml_start_at_b = NULL,
                                    logols_coef = NULL) {
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))
  chol_xx <- chol(crossprod(x_mat))
  ladder <- list()
  if (!is.null(ppml_bundle) && "variance_start" %in% ppml_bundle$valid_for) {
    ladder$ppml <- ppml_bundle$coef_original
  }
  if (!is.null(logols_coef)) {
    ladder$logols_shifted <-
      logols_coef + c(logvar_normal_lnchisq_gap, rep(0, length(logols_coef) - 1L))
  }
  point_fallbacks <- unname(ladder[setdiff(names(ladder), "ppml")])
  raw_point_fit <- NULL
  if (!is.null(b_point) && !anyNA(b_point)) {
    raw_point_fit <- logvar_harvey_fit(
      b_point, w1, w2, x_mat,
      start = ladder$ppml, fallback_starts = point_fallbacks, chol_xx = chol_xx
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
  base_ladder <- Filter(Negate(is.null), list(ladder$ppml, ladder$logols_shifted))
  metadata <- list(
    estimator = "harvey", target_functional = "theta_var_gaussian",
    intercept_normalization =
      "log conditional variance (mean-log + 1.270362845 under normality; absorbs 2 log|m_0|)",
    sample_id = logvar_sample_id(qtr, w1, w2, pcr), smoothness = "smooth",
    inner_solver = "observed-Newton (Fisher fallback) + backtracking", response_scale = "variance",
    response_scale_value = 1, scale_reference = "none",
    spec_id = logvar_spec_id(list(
      estimator_version = "harvey-v1", score_tol = 1e-8, rank_tol = 1e-8,
      rcond_tol = 1e-10,
      recession_rate_tol_rule = "1e-9*max(1,sum(abs(colSums(Z))))",
      line_search_halvings = 30L, maxit = 1000L,
      q_equality_progress = "10*eps*max(1,score)", rel_change_tol = 1e-10,
      start_policy = "ppml,mean,logols_shifted", scaling_policy = "none",
      cold_start_rtol = 1e-6,
      b_point = if (!is.null(b_point) && !anyNA(b_point)) b_point else "null",
      ppml_rung = !is.null(ppml_bundle), logols_rung = !is.null(logols_coef)
    )),
    cold_start_rtol = 1e-6
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
          auto_intercept = auto
        )
      }
      tagged <- function(f, lbl) {
        lapply(f$diagnostics$start_attempts, function(a) {
          a$source <- paste0(lbl, a$source)
          a
        })
      }
      # warm -> ppml -> standalone: the cheap warm attempt runs without the
      # auto-salvage rung so a failed chain genuinely falls through, the
      # arbitrary-b PPML rung is consulted lazily (only on that failure),
      # and the standalone ladder closes with the solver's intercept rung
      warm <- if (!is.null(start)) start else point_warm
      best <- NULL
      trail <- list()
      if (!is.null(warm)) {
        best <- hv(warm, list(), FALSE)
        trail <- tagged(best, "warm:")
      }
      if (!logvar_harvey_accepted(best) && !is.null(ppml_start_at_b)) {
        pf <- tryCatch(ppml_start_at_b(b), error = function(cond) NULL)
        if (logvar_harvey_accepted(pf)) {
          second <- hv(pf$coef, list(), FALSE)
          trail <- c(trail, tagged(second, "ppml:"))
          if (logvar_harvey_accepted(second)) best <- second
        }
      }
      if (!logvar_harvey_accepted(best)) {
        third <- hv(NULL, base_ladder, TRUE)
        trail <- c(trail, tagged(third, "standalone:"))
        best <- third
      }
      best$diagnostics$start_attempts <- trail
      best
    },
    jacobian_at_b = function(b, fit = NULL) {
      if (is.null(fit)) {
        return(NULL)
      }
      logvar_harvey_jacobian(fit, b, w1, w2, x_mat)
    },
    analyze_domain = list(
      precheck = function(qs, b_tab) {
        st <- logvar_harvey_recession_self_test(x_mat)
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
