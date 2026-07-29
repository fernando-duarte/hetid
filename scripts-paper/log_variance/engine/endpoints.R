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
    state <- logvar_apply_extra_candidates(
      ec,
      list(
        lower = lower, upper = upper, arg_lo = arg_lo, arg_up = arg_up,
        prov_lo = prov_lo, prov_up = prov_up
      ),
      lower_unb, upper_unb
    )
    lower <- state$lower
    upper <- state$upper
    arg_lo <- state$arg_lo
    arg_up <- state$arg_up
    prov_lo <- state$prov_lo
    prov_up <- state$prov_up
  }
  seed_start <- !is.null(b_seed) && !anyNA(b_seed)
  budget_hit <- new.env(parent = emptyenv())
  budget_hit$cond <- NULL
  polish_recs <- list()
  for (j in seq_len(n_coef)) {
    scan_j <- c(scan$min[j], scan$max[j])
    scale_j <- max(1, abs(scan_j[is.finite(scan_j)]))
    objective <- logvar_coef_objective_fns(est, j, evaluate_fit, budget_hit)
    fn <- objective$fn
    gr <- objective$gr
    method <- if (identical(meta$smoothness, "smooth") && !is.null(gr)) {
      "slsqp"
    } else {
      "cobyla"
    }
    for (side in c("min", "max")) {
      if (if (side == "min") lower_unb[j] else upper_unb[j]) next
      starts <- list(if (side == "min") scan$arg_min[j, ] else scan$arg_max[j, ])
      pool <- if (side == "min") scan$arg_min_pool else scan$arg_max_pool
      if (!is.null(pool) && length(pool) >= j) starts <- c(starts, pool[[j]])
      if (seed_start) starts <- c(starts, list(b_seed))
      starts <- c(starts, extra_pool)
      accepted <- FALSE
      opt_code <- NA_integer_
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
          opt_code <- pol$convergence
        }
        if (side == "max" && pol$bound > upper[j]) {
          upper[j] <- pol$bound
          arg_up[j, ] <- pol$par
          prov_up[j] <- "polish"
          opt_code <- pol$convergence
        }
      }
      if (!accepted) {
        if (side == "min") lo_unrel[j] <- TRUE else up_unrel[j] <- TRUE
      }
      polish_recs[[length(polish_recs) + 1L]] <- list(
        coef = labels[j], side = side, n_trials = length(starts),
        accepted = accepted, opt_code = opt_code
      )
    }
  }
  # an accepted endpoint outside the box the scan was handed proves the box is
  # not the outer screen it is contracted to be: the grid could never have
  # reached that point, so the two halves of the search disagreed about the
  # region. Report the side instead of drawing a boundary only the polish saw.
  box_escapes <- list()
  for (j in seq_len(n_coef)) {
    for (side in c("min", "max")) {
      if (if (side == "min") lower_unb[j] else upper_unb[j]) next
      excess <- logvar_box_escape(
        if (side == "min") arg_lo[j, ] else arg_up[j, ], b_tab
      )
      if (is.na(excess) ||
        excess <= PAPER_QUADRATIC_CONTROL$box_escape_rtol) {
        next
      }
      if (side == "min") lo_unrel[j] <- TRUE else up_unrel[j] <- TRUE
      box_escapes[[length(box_escapes) + 1L]] <- list(
        coef = labels[j], side = side, excess = excess
      )
    }
  }
  estimator_cold_switch <- meta$fit_control$cold_switch
  cold_enabled <- isTRUE(cold_start_check) &&
    !identical(estimator_cold_switch, FALSE)
  cold_recs <- if (cold_enabled) {
    logvar_engine_cold_check(
      meta, labels, lower, upper, arg_lo, arg_up,
      lower_unb, upper_unb, lo_unrel, up_unrel, evaluate_fit
    )
  } else {
    list(records = list(), lo_unrel = lo_unrel, up_unrel = up_unrel)
  }
  lo_unrel <- cold_recs$lo_unrel
  up_unrel <- cold_recs$up_unrel
  endpoint_status <- function(unreliable, unbounded) {
    ifelse(
      unreliable,
      PAPER_ENDPOINT_STATUS[["unreliable"]],
      ifelse(
        unbounded,
        PAPER_ENDPOINT_STATUS[["unbounded"]],
        PAPER_ENDPOINT_STATUS[["bounded"]]
      )
    )
  }
  lo_st <- endpoint_status(lo_unrel, lower_unb)
  up_st <- endpoint_status(up_unrel, upper_unb)
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
    diag_of(list(
      extra_start_skipped = extra_skipped, cold_start = cold_recs$records,
      polish = polish_recs, box_escape = box_escapes
    ))
  )
}
