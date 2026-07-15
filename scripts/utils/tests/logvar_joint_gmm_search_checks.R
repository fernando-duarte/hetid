# Stage B search checks: the pinned constants block, complete candidate construction,
# deterministic separated-start selection with tie replay, full observed-sign-pattern
# accounting with the exact raw-sign gate, evidence-calibrated search status,
# tolerance and tau/delta nesting discipline, expanding-box statuses, floor-sensitivity
# audits, GMM-owned budget exhaustion, and epigraph initialization. Sourced by
# test_logvar_joint_gmm.R, which owns check(), jg_try, jg_need, and jg_fx. Every check
# asserts the not-yet-built symbol exists first, so an absent module fails closed for
# the right reason. Section numbers cite the joint-GMM research dossier.
fx <- jg_fx
w1 <- fx$w1
w2 <- fx$w2
b_fd <- fx$b_fd
e_fd <- drop(w1 - w2 %*% b_fd)

# Pinned constants and separated numerical controls (dossier section 5): four
# controls, box schedule, budgets, and the Option-B dimension counts; moment_delta is
# never one of the numerical controls.
check("the pinned constants block carries the frozen values and control separation", jg_try({
  jg_need("logvar_joint_gmm_constants")
  k <- logvar_joint_gmm_constants
  k$param_xtol_rel == 1e-9 && k$objective_tol == 1e-10 && k$constraint_tol == 1e-8 &&
    k$root_tol == 1e-8 && identical(as.numeric(k$box_half_widths), c(4, 8, 16)) &&
    k$pattern_start_cap == 256L && k$gmm_grid_cap == 4000L &&
    k$candidate_eval_cap == 60000L && k$grid_n == 41L && k$grid_floor == 100L &&
    k$n_moments_unprofiled == 10L && k$n_parameters_unprofiled == 9L &&
    k$n_moments_profiled == 8L && k$n_parameters_profiled == 7L &&
    !("moment_delta" %in% names(k))
}))

# Observed sign patterns are the literal +/- vector; near-zero rows are unavailable.
check("sign patterns are the literal +/- census with unavailable near-zero rows", jg_try({
  jg_need("logvar_joint_sign_pattern")
  sp <- logvar_joint_sign_pattern(e_fd, 1e-12)
  e2 <- e_fd
  e2[1L] <- 1e-14
  sp2 <- logvar_joint_sign_pattern(e2, 1e-12)
  identical(sp$signs, sign(e_fd)) && all(sp$available) &&
    identical(sp$id, paste(ifelse(e_fd > 0, "+", "-"), collapse = "")) &&
    isFALSE(sp2$available[1L]) && all(sp2$available[-1L])
}))
# The exact raw-sign gate rejects a wrong orthant even inside generic 1e-8 slack.
check("a wrong-orthant point within 1e-8 slack fails the exact raw-sign check", jg_try({
  jg_need("logvar_joint_accept_raw_signs")
  w1b <- w1
  w1b[1L] <- drop(w2[1L, ] %*% b_fd) - 1e-9
  signs <- sign(e_fd)
  signs[1L] <- 1
  e1 <- drop(w1b - w2 %*% b_fd)
  isTRUE(all(signs * e1 > -1e-8)) &&
    isFALSE(logvar_joint_accept_raw_signs(b_fd, w1b, w2, signs))
}))

# Candidate construction is deterministic and complete on the scanned grid (dossier
# section 6): every (b, beta) pair appears once; duplicate seeds do not inflate it.
check("candidate construction is complete on the grid and deduplicates seeds", jg_try({
  jg_need("logvar_joint_candidate_table")
  seeds <- list(c(0, 0, 0, 0), c(0.1, -0.1, 0, 0))
  tab <- logvar_joint_candidate_table(fx$grid_b, seeds)
  tab_dup <- logvar_joint_candidate_table(fx$grid_b, c(seeds, seeds[1L]))
  nrow(tab) == nrow(fx$grid_b) * 2L && nrow(tab_dup) == nrow(tab)
}))
# Deterministic separated-start selection: best per pattern, replayed identically.
check("start selection keeps the best per pattern and replays deterministically", jg_try({
  jg_need("logvar_joint_select_starts")
  coords <- rbind(c(0, 0), c(0.01, 0), c(1, 1), c(1.01, 1), c(3, 3), c(3, 3.01))
  pid <- c("A", "A", "B", "B", "C", "C")
  score <- c(2, 1, 2, 1, 2, 1)
  sel <- logvar_joint_select_starts(coords, pid, score, pattern_cap = 256L)
  all(c(2L, 4L, 6L) %in% sel$indices) && identical(sel$coverage_status, "complete") &&
    identical(sel, logvar_joint_select_starts(coords, pid, score, pattern_cap = 256L))
}))
check("start selection marks coverage incomplete when patterns exceed the cap", jg_try({
  jg_need("logvar_joint_select_starts")
  coords <- rbind(c(0, 0), c(1, 1), c(3, 3))
  pid <- c("A", "B", "C")
  sel <- logvar_joint_select_starts(coords, pid, c(1, 1, 1), pattern_cap = 2L)
  identical(sel$coverage_status, "incomplete")
}))

# Evidence-calibrated search status (dossier section 3.3, contract): constructive
# statuses only; absence never certifies emptiness; a within-delta candidate is a
# nonemptiness witness.
check("search status is constructive and never certifies emptiness", jg_try({
  jg_need("logvar_joint_classify_search")
  cs <- logvar_joint_classify_search
  identical(cs(0.3, FALSE, TRUE, FALSE), "not_found_within_search") &&
    identical(cs(1e-9, TRUE, TRUE, FALSE), "found_within_root_tol") &&
    identical(cs(0.3, FALSE, FALSE, FALSE), "unreliable") &&
    identical(cs(0.3, FALSE, TRUE, TRUE), "unreliable") &&
    !(cs(0.3, FALSE, TRUE, FALSE) %in% c("empty", "exact_root", "region_unchanged"))
}))
check("a within-delta candidate certifies nonemptiness and nests in delta", jg_try({
  jg_need("logvar_joint_delta_nonempty")
  d <- logvar_joint_delta_nonempty
  # nesting: a candidate feasible at the smaller delta is feasible at the larger one
  isTRUE(d(0.02, 0.05)) && isFALSE(d(0.2, 0.05)) && isFALSE(d(0.02, 0.01)) &&
    (!d(0.02, 0.03) || d(0.02, 0.05))
}))
# Tau nesting: the Plan 6 hard-stop-with-floor carries the prior arg-min when a
# supposedly larger set fails to improve the attained minimum.
check("nested-tau minima are nonincreasing with the prior arg-min carried as floor", jg_try({
  jg_need("logvar_joint_carry_floor")
  worse <- logvar_joint_carry_floor(0.1, c(1, 2, 3), 0.2, c(4, 5, 6))
  better <- logvar_joint_carry_floor(0.2, c(1, 2, 3), 0.1, c(4, 5, 6))
  worse$min == 0.1 && identical(worse$argmin, c(1, 2, 3)) && isTRUE(worse$floored) &&
    better$min == 0.1 && identical(better$argmin, c(4, 5, 6)) && isFALSE(better$floored)
}))

# Expanding-box statuses (contract): interior agreement is bounded; box movement is
# unreliable; unbounded requires a verified direction certificate, never finite probes.
check("box status is bounded interior, unreliable when moving, unbounded only certified", jg_try({
  jg_need("logvar_joint_box_status")
  bs <- logvar_joint_box_status
  cert <- list(certified = TRUE, direction = c(1, 0, 0), rate = 1, sign = 1)
  identical(bs(TRUE, TRUE, FALSE, FALSE), "bounded") &&
    identical(bs(FALSE, FALSE, TRUE, TRUE), "unreliable") &&
    identical(bs(FALSE, FALSE, FALSE, TRUE, direction_certificate = NULL), "unreliable") &&
    identical(bs(FALSE, FALSE, FALSE, TRUE,
      direction_certificate = list(certified = FALSE)
    ), "unreliable") &&
    identical(bs(FALSE, FALSE, FALSE, TRUE, direction_certificate = cert), "unbounded")
}))

# Floor sensitivity (contract): replay at multipliers 1, 0.1, 0.01; drift or a status
# change is unreliable; the raw-unit guard slack round-trips.
check("floor audit is stable only when status and value agree across the reductions", jg_try({
  jg_need("logvar_joint_floor_audit")
  fa <- logvar_joint_floor_audit
  stable <- fa(c(0.5, 0.5 + 1e-8, 0.5 - 1e-8), rep("found_within_root_tol", 3L))
  drift <- fa(c(0.5, 0.6, 0.5), rep("found_within_root_tol", 3L))
  flip <- fa(c(0.5, 0.5, 0.5), c("found_within_root_tol", "unreliable", "found_within_root_tol"))
  isTRUE(stable$stable) && identical(stable$status, "found_within_root_tol") &&
    isFALSE(drift$stable) && identical(drift$status, "unreliable") &&
    isFALSE(flip$stable) && identical(flip$status, "unreliable")
}))
check("the raw-unit guard slack round-trips against min(s_i e_i) - eps_floor", jg_try({
  jg_need("logvar_joint_guard_slack")
  eps_floor <- 1e-12 * stats::median(abs(fx$e_star))
  gs <- logvar_joint_guard_slack(b_fd, w1, w2, sign(e_fd), eps_floor)
  abs(gs - (min(sign(e_fd) * e_fd) - eps_floor)) < 1e-12
}))

# GMM-owned budget (contract): seven counters, cap-based exhaustion, cache hits never
# debit or exhaust; the counts are disclosed for the unreliable-block report.
check("budget state exposes the seven counters and cache hits never exhaust", jg_try({
  jg_need("logvar_joint_budget_state")
  st <- logvar_joint_budget_state(0.05, "z", "jid", "sid", caps = list(grid_eval = 3L))
  ok_names <- all(c(
    "grid_eval", "ppml_seed_fit", "candidate_eval", "stage_b_solve",
    "stage_c_solve", "fresh_verify", "cache_hit"
  ) %in% names(st$counts))
  st$debit("grid_eval", 3L)
  st$debit("cache_hit", 9L)
  ok_names && st$counts[["grid_eval"]] == 3L && isTRUE(st$is_exhausted("grid_eval")) &&
    st$counts[["cache_hit"]] == 9L && isFALSE(st$is_exhausted("cache_hit"))
}))

# Epigraph initialization (contract): fresh r0, the exact-zero short circuit, and the
# separately recorded positive-slack start.
check("epigraph start short-circuits at exact zero and slacks a positive r0", jg_try({
  jg_need("logvar_joint_epigraph_start")
  z <- logvar_joint_epigraph_start(c(0, 0, 0))
  p <- logvar_joint_epigraph_start(c(0.3, -0.5, 0.1))
  z$r0 == 0 && isTRUE(z$short_circuit) && z$r_start == 0 &&
    abs(p$r0 - 0.5) < 1e-15 && isFALSE(p$short_circuit) &&
    abs(p$r_start - (0.5 + 5e-7)) < 1e-15
}))
