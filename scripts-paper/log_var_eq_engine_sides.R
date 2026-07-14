# Endpoint stage of the estimator-generic set engine: the sides phase of
# analyze_domain, extra-start attained candidates, the per-coefficient
# per-side polish loop (SLSQP or COBYLA via the generalized objective seam),
# cold-start replication, and side-specific status assembly. The benchmark
# configuration reproduces the driver closure's arithmetic exactly: same
# start order (grid arg-extreme, then seed), same acceptance, same blow
# guard, same status ladder. Definitions only; sourced by
# log_var_eq_engine.R.

logvar_engine_endpoints <- function(est, qs, b_tab, b_seed, extra_starts,
                                    cold_start_check, tau, meta, bs,
                                    evaluate_fit, ctx, st, omega, scan,
                                    pre_cross, diag_of) {
  labels <- st$labels
  n_coef <- length(labels)
  ad <- est$analyze_domain
  lower_unb <- upper_unb <- rep(FALSE, n_coef)
  sides <- NULL
  if (!is.null(ad$sides)) {
    sides <- logvar_call_sides(ad$sides, qs, b_tab, scan, ctx)
    lower_unb <- sides$lower_unbounded
    upper_unb <- sides$upper_unbounded
  }
  lo_unrel <- up_unrel <- rep(FALSE, n_coef)
  for (u in if (is.null(sides)) character(0) else sides$unresolved_endpoints) {
    parts <- strsplit(u, ":", fixed = TRUE)[[1]]
    j <- match(parts[1], labels)
    if (is.na(j) || length(parts) != 2L) {
      lo_unrel[] <- TRUE
      up_unrel[] <- TRUE
    } else if (parts[2] == "min") {
      lo_unrel[j] <- TRUE
    } else {
      up_unrel[j] <- TRUE
    }
  }
  lower <- ifelse(lower_unb, -Inf, scan$min)
  upper <- ifelse(upper_unb, Inf, scan$max)
  arg_lo <- scan$arg_min
  arg_up <- scan$arg_max
  prov_lo <- rep("grid", n_coef)
  prov_up <- rep("grid", n_coef)
  extra_pool <- list()
  extra_skipped <- list()
  if (!is.null(extra_starts)) {
    ec <- logvar_extra_candidates(extra_starts, evaluate_fit, ctx$check_feasible)
    extra_skipped <- ec$skipped
    extra_pool <- ec$points
    for (i in seq_along(ec$points)) {
      v <- ec$values[[i]]
      for (j in seq_len(n_coef)) {
        if (!lower_unb[j] && v[j] < lower[j]) {
          lower[j] <- v[j]
          arg_lo[j, ] <- ec$points[[i]]
          prov_lo[j] <- "extra-start"
        }
        if (!upper_unb[j] && v[j] > upper[j]) {
          upper[j] <- v[j]
          arg_up[j, ] <- ec$points[[i]]
          prov_up[j] <- "extra-start"
        }
      }
    }
  }
  seed_start <- !is.null(b_seed) && !anyNA(b_seed)
  budget_hit <- new.env(parent = emptyenv())
  budget_hit$cond <- NULL
  fit_ok <- function(fit) {
    identical(fit$fit_status, "ok") && isTRUE(fit$converged) &&
      !is.null(fit$coef) && all(is.finite(fit$coef))
  }
  for (j in seq_len(n_coef)) {
    scan_j <- c(scan$min[j], scan$max[j])
    scale_j <- max(1, abs(scan_j[is.finite(scan_j)]))
    co <- if (is.null(est$coef_objective)) NULL else est$coef_objective(j)
    if (is.null(co)) {
      fn <- local({
        jj <- j
        function(b) {
          tryCatch(
            {
              fit <- evaluate_fit(b, phase = "polish")
              if (!fit_ok(fit)) NaN else unname(fit$coef[[jj]])
            },
            logvar_budget_exhausted = function(e) {
              budget_hit$cond <- e
              NaN
            }
          )
        }
      })
      gr <- if (is.null(est$jacobian_at_b)) {
        NULL
      } else {
        local({
          jj <- j
          function(b) est$jacobian_at_b(b)[jj, ]
        })
      }
    } else {
      fn <- co$fn
      gr <- co$gr
    }
    method <- if (identical(meta$smoothness, "smooth") && !is.null(gr)) {
      "slsqp"
    } else {
      "cobyla"
    }
    for (side in c("min", "max")) {
      if (if (side == "min") lower_unb[j] else upper_unb[j]) next
      starts <- list(if (side == "min") scan$arg_min[j, ] else scan$arg_max[j, ])
      if (seed_start) starts <- c(starts, list(b_seed))
      starts <- c(starts, extra_pool)
      accepted <- FALSE
      for (b_start in starts) {
        pol <- logvar_polish_objective(
          qs, side, b_start, scale_j,
          fn = fn, gr = gr, method = method
        )
        if (!is.null(budget_hit$cond)) stop(budget_hit$cond)
        if (pol$suspect) {
          if (side == "min") lo_unrel[j] <- TRUE else up_unrel[j] <- TRUE
        }
        if (is.null(pol$bound)) next
        accepted <- TRUE
        if (side == "min" && pol$bound < lower[j]) {
          lower[j] <- pol$bound
          arg_lo[j, ] <- pol$par
          prov_lo[j] <- "polish"
        }
        if (side == "max" && pol$bound > upper[j]) {
          upper[j] <- pol$bound
          arg_up[j, ] <- pol$par
          prov_up[j] <- "polish"
        }
      }
      if (!accepted) {
        if (side == "min") lo_unrel[j] <- TRUE else up_unrel[j] <- TRUE
      }
    }
  }
  cold_recs <- list()
  if (isTRUE(cold_start_check)) {
    rtol <- if (is.null(meta$cold_start_rtol)) 1e-8 else meta$cold_start_rtol
    for (j in seq_len(n_coef)) {
      for (side in c("min", "max")) {
        unb <- if (side == "min") lower_unb[j] else upper_unb[j]
        unrel <- if (side == "min") lo_unrel[j] else up_unrel[j]
        val <- if (side == "min") lower[j] else upper[j]
        arg <- if (side == "min") arg_lo[j, ] else arg_up[j, ]
        if (unb || unrel || !is.finite(val) || anyNA(arg)) next
        fitc <- evaluate_fit(arg, phase = "cold_start", start = NULL, use_cache = FALSE)
        vc <- if (fit_ok(fitc)) unname(fitc$coef[[j]]) else NaN
        if (!is.finite(vc) || abs(vc - val) > rtol * max(1, abs(val))) {
          if (side == "min") lo_unrel[j] <- TRUE else up_unrel[j] <- TRUE
          cold_recs[[length(cold_recs) + 1L]] <- list(
            coef = labels[j], side = side, value = val, cold_value = vc
          )
        }
      }
    }
  }
  lo_st <- ifelse(lo_unrel, "unreliable", ifelse(lower_unb, "unbounded", "bounded"))
  up_st <- ifelse(up_unrel, "unreliable", ifelse(upper_unb, "unbounded", "bounded"))
  n_cross <- if (!is.null(sides$info$cross_all)) {
    length(sides$info$cross_all)
  } else {
    pre_cross
  }
  domain_info <- list(
    cross_all = sides$info$cross_all,
    sides_info = if (is.null(sides)) NULL else sides$info,
    closure_diagnostics = if (is.null(sides)) NULL else sides$closure_diagnostics,
    claimed_failures = scan$claimed_failures,
    precheck = ctx$precheck
  )
  logvar_engine_result(
    labels, lower, upper, lo_st, up_st, prov_lo, prov_up, arg_lo, arg_up,
    meta, tau, qs, omega, st$n_fail, n_cross, st$n_feasible, domain_info,
    diag_of(list(extra_start_skipped = extra_skipped, cold_start = cold_recs))
  )
}
