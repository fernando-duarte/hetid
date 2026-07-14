# Diagnostics helpers for the median (LAD) log-variance driver: the fn
# nonuniqueness demotion, the fragility scale, the crossing-path re-derivation (the
# committed sides hook keeps only the stable-finite closure records, so the driver
# re-runs the committed probe and classifier to recover the divergent M-slopes and
# the one-sided limit values), and the closure CSV rows. The console printer lives
# in log_var_eq_lad_console.R. The committed modules are consumed, never modified.
# Definitions only; sourced by the driver before the guarded orchestration.

# NULL-coalesce for the optional classifier metrics.
.lad_or <- function(x, d) if (is.null(x)) d else x

# The deterministic fn schedule over endpoint-relevant args (bounded attaining
# points), the benchmark seed, and promoted candidates, under the nonunique cap.
# A sensitive endpoint-relevant point, or a cap that truncates a required point,
# fails the whole tau closed. Probes fit cold (never averaged), as the schedule
# requires; the cap is enforced here since these refits bypass the engine cache.
logvar_lad_nonunique_demote <- function(est, res, cfg, promoted) {
  cap <- cfg$phase_caps[["nonunique"]]
  sc <- res$schema
  args <- list()
  add <- function(a, tag, rel) {
    if (!anyNA(a)) {
      args[[length(args) + 1L]] <<- list(a = unname(a), tag = tag, rel = rel)
    }
  }
  for (j in seq_len(nrow(sc))) {
    if (identical(sc$lower_status[j], "bounded")) {
      add(sc$arg_lower[[j]], sprintf("%s:min", sc$coef[j]), TRUE)
    }
    if (identical(sc$upper_status[j], "bounded")) {
      add(sc$arg_upper[[j]], sprintf("%s:max", sc$coef[j]), TRUE)
    }
  }
  if (!is.null(cfg$b_seed)) add(cfg$b_seed, "benchmark", FALSE)
  for (p in promoted) add(p, "promoted", TRUE)
  count <- 0L
  sensitive <- list()
  truncated <- FALSE
  for (it in args) {
    if (count >= cap) {
      if (isTRUE(it$rel)) truncated <- TRUE
      next
    }
    fit <- tryCatch(est$fit_at_b(it$a), error = function(e) NULL)
    if (is.null(fit) || !identical(fit$fit_status, "ok")) next
    z <- 2 * log(abs(drop(cfg$w1 - cfg$w2 %*% it$a)))
    pr <- tryCatch(logvar_lad_nonunique_probe(fit, z, cfg$x_mat), error = function(e) NULL)
    count <- count + 1L
    if (!is.null(pr) && isTRUE(pr$multiple_solution_sensitive) && isTRUE(it$rel)) {
      sensitive[[length(sensitive) + 1L]] <- list(tag = it$tag, coef_max_diff = pr$coef_max_diff)
    }
  }
  list(
    tau_unreliable = length(sensitive) > 0L || truncated,
    n_probed = count, sensitive = sensitive, truncated = truncated, cap = cap
  )
}

# The fragility line: the smallest pointwise min_t |eps_hat_t(b)| over each bounded
# endpoint arg and the search seed -- how close the attained set gets to a crossing.
logvar_lad_min_feasible_eps <- function(schema, w1, w2, seed) {
  args <- logvar_lad_bounded_args(schema)
  if (!is.null(seed) && !anyNA(seed)) args <- c(args, list(seed))
  if (length(args) == 0L) {
    return(NA_real_)
  }
  min(vapply(args, function(a) min(abs(drop(w1 - w2 %*% a))), numeric(1)))
}

# Re-derive the per-path, per-coefficient tail classification for a tau by probing
# every verified witness cold through the committed crossing probe and classifier.
# Returns one row per (witness, path, coefficient) with the classification status,
# endpoint direction, M-span, tail slope, stability, tail-limit value, and a
# 17-digit path summary; NULL when the tau has no verified crossing paths.
logvar_lad_probe_report <- function(res, tau, est, geom, labels, qtr) {
  pc <- res$domain_info$precheck
  wits <- if (is.null(pc)) list() else .lad_or(pc$info$witnesses, list())
  fmt17 <- function(x) formatC(x, digits = 17, format = "fg", flag = "#")
  ctx <- list(
    evaluate_fit = function(b, phase, start = NULL, use_cache = TRUE) est$fit_at_b(b),
    check_feasible = geom$check_feasible, w1 = geom$w1, w2 = geom$w2,
    e_scale_ref = geom$e_scale_ref, x_mat = geom$x_mat,
    b_scales = list(delta = geom$delta, omega = geom$omega)
  )
  rows <- list()
  for (wid in names(wits)) {
    wit <- wits[[wid]]
    for (path_id in seq_along(wit$anchors)) {
      tr <- tryCatch(logvar_lad_crossing_probe(wit, path_id, ctx), error = function(e) NULL)
      cm <- if (is.null(tr)) NULL else tr$coef
      have <- !is.null(cm) && !is.null(dim(cm)) && nrow(cm) >= 1L
      cls <- if (have) logvar_lad_tail_classify(tr)$coef else list()
      side <- if (identical(wit$anchors[[path_id]]$sign, 1)) "+" else "-"
      for (j in seq_along(labels)) {
        cj <- .lad_or(cls[[labels[j]]], list(status = "unresolved"))
        lim <- if (have && j <= ncol(cm)) cm[nrow(cm), j] else NA_real_
        summ <- if (have) {
          paste(sprintf("n=%d", nrow(cm)), sprintf("m_hi=%s", fmt17(max(tr$m))),
            sprintf("c_last=%s", fmt17(lim)),
            sep = "|"
          )
        } else {
          "empty"
        }
        ep <- .lad_or(cj$endpoint, NA_character_)
        if (is.na(ep)) ep <- NA_character_
        rows[[length(rows) + 1L]] <- data.frame(
          coef = labels[j], tau = tau, crossing_row = as.integer(wit$row),
          crossing_qtr = format(qtr[wit$row], "%Y Q%q"), residual_side = side,
          path_id = as.integer(path_id), witness_status = "verified",
          status = .lad_or(cj$status, "unresolved"), endpoint = ep,
          m_span = .lad_or(cj$m_span, NA_real_), tail_slope = .lad_or(cj$slope6, NA_real_),
          tail_slope_stability = .lad_or(cj$implied_move, NA_real_),
          limit_value = lim, path_summary = summ, stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) NULL else do.call(rbind, rows)
}

# Closure CSV rows: the stable-finite one-sided limits from the path report mapped
# to the frozen schema. Never merged into the attained hull.
logvar_lad_closure_rows <- function(paths, estimator, sample_id, spec_id) {
  if (is.null(paths)) {
    return(logvar_lad_closure_schema())
  }
  keep <- paths[paths$status == "stable_finite", , drop = FALSE]
  if (nrow(keep) == 0L) {
    return(logvar_lad_closure_schema())
  }
  data.frame(
    coef = keep$coef, tau = keep$tau, estimator = estimator,
    sample_id = sample_id, spec_id = spec_id, crossing_row = keep$crossing_row,
    crossing_qtr = keep$crossing_qtr, residual_side = keep$residual_side,
    coef_side = ifelse(is.na(keep$endpoint), NA_character_, keep$endpoint),
    limit_value = keep$limit_value, path_id = keep$path_id,
    witness_status = keep$witness_status, M_span = keep$m_span,
    tail_slope = keep$tail_slope, tail_slope_stability = keep$tail_slope_stability,
    path_summary = keep$path_summary,
    provenance = "one-sided crossing-limit approximation", stringsAsFactors = FALSE
  )
}
