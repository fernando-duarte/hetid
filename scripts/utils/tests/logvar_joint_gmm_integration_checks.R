# Integration checks: gate-branch dispatch over the committed Task 1 decision record,
# the joint_input_id canonical hash, qtr-keyed alignment, artifact dimension and schema
# fields, and the honest-language guard. The decision-layer checks exercise the
# committed helpers and may pass; the driver, identity, budget, and artifact checks are
# red until Tasks 3 and 4 land. Sourced by test_logvar_joint_gmm.R, which owns check(),
# jg_try, jg_need, and jg_fx. Section numbers cite the joint-GMM research dossier.
fx <- jg_fx

# Build a validated decision record from the committed pure default, overriding the
# scientific choices and recomputing the spec ID (Task 1 helpers only).
mk_decision <- function(z = FALSE, lp = FALSE, sc = FALSE, delta = numeric(0)) {
  rec <- logvar_joint_decision_default("2026-07-14")
  rec$enable_z <- z
  rec$enable_log_ppml <- lp
  rec$stage_c_requested <- sc
  rec$moment_delta <- delta
  rec$decision_provenance <- "user_ratified"
  rec$decision_spec_id <- logvar_joint_decision_spec_id(rec)
  rec
}

# The committed no-answer default validates, is all-false, and equals the pure default
# (a noninteractive clean checkout mutates nothing at runtime).
check("the committed no-answer default validates and is all-false", jg_try({
  logvar_joint_decision_validate(logvar_joint_gmm_decision)
  d <- logvar_joint_decision_default(logvar_joint_gmm_decision$decided_on)
  identical(logvar_joint_gmm_decision$decision_provenance, "no_answer_default") &&
    isFALSE(logvar_joint_gmm_decision$enable_z) &&
    isFALSE(logvar_joint_gmm_decision$enable_log_ppml) &&
    identical(d$decision_spec_id, logvar_joint_gmm_decision$decision_spec_id)
}))
# A gaussian_gap_restriction = TRUE record is rejected (reserved, not implemented).
check("a gaussian_gap_restriction = TRUE record is rejected by validation", jg_try({
  rec <- logvar_joint_decision_default("2026-07-14")
  rec$gaussian_gap_restriction <- TRUE
  rec$decision_spec_id <- logvar_joint_decision_spec_id(rec)
  res <- tryCatch(
    {
      logvar_joint_decision_validate(rec)
      "ok"
    },
    error = function(e) paste(class(e), collapse = " ")
  )
  grepl("unsupported_gaussian_gap_restriction", res)
}))
# A tampered spec ID is rejected as stale (schema/spec-ID tampering guard).
check("a tampered decision_spec_id is rejected as stale", jg_try({
  rec <- logvar_joint_decision_default("2026-07-14")
  rec$decision_spec_id <- "deadbeefdeadbeefdeadbeefdeadbeef"
  identical(
    tryCatch(
      {
        logvar_joint_decision_validate(rec)
        "ok"
      },
      error = function(e) "stopped"
    ),
    "stopped"
  )
}))

# Gate branch matrix (dossier section 11): enable flags map to moment blocks; Stage C
# needs a request and a nonempty ratified delta.
check("the gate branch matrix maps enable flags to moment blocks", jg_try({
  jg_need("logvar_joint_gmm_enabled_blocks")
  eb <- logvar_joint_gmm_enabled_blocks
  length(eb(mk_decision())) == 0L &&
    identical(eb(mk_decision(z = TRUE)), "z") &&
    identical(eb(mk_decision(lp = TRUE)), "log_ppml") &&
    setequal(eb(mk_decision(z = TRUE, lp = TRUE)), c("z", "log_ppml"))
}))
check("stage C is enabled only with a request and a nonempty ratified delta", jg_try({
  jg_need("logvar_joint_gmm_stage_c_enabled")
  sc <- logvar_joint_gmm_stage_c_enabled
  isFALSE(sc(mk_decision(sc = TRUE, delta = numeric(0)))) &&
    isTRUE(sc(mk_decision(sc = TRUE, delta = c(0.05, 0.1)))) &&
    isFALSE(sc(mk_decision(sc = FALSE, delta = 0.05)))
}))
# An all-redundant z block fails closed and is never reported as overidentification.
check("an all-redundant z block fails closed, never overidentification", jg_try({
  jg_need("logvar_residualize_moment_basis")
  bs <- logvar_residualize_moment_basis(fx$z_redundant, fx$x_mat)
  identical(bs$status, "unreliable") && bs$rank == 0L
}))

# Canonical joint_input_id (contract): deterministic, validates finite inputs, and is
# sensitive to z, gamma, tau, and the mean-system geometry.
check("joint_input_id is deterministic and sensitive to every substantive input", jg_try({
  jg_need("logvar_joint_input_id")
  jid <- function(z, g, tau, ms) {
    logvar_joint_input_id(fx$sample_id, fx$qtr, z, g, tau, ms)
  }
  base <- jid(fx$z_multi, fx$gamma, fx$tau, fx$mean_systems)
  z2 <- fx$z_multi
  z2[1L, 1L] <- z2[1L, 1L] + 1e-6
  ms2 <- list(jg_ball(fx$b_star, 0.6))
  identical(base, jid(fx$z_multi, fx$gamma, fx$tau, fx$mean_systems)) &&
    base != jid(z2, fx$gamma, fx$tau, fx$mean_systems) &&
    base != jid(fx$z_multi, fx$gamma * 2, fx$tau, fx$mean_systems) &&
    base != jid(fx$z_multi, fx$gamma, c(fx$tau, 0.5), fx$mean_systems) &&
    base != jid(fx$z_multi, fx$gamma, fx$tau, ms2)
}))
check("joint_input_id validates finite inputs before hashing", jg_try({
  jg_need("logvar_joint_input_id")
  z_bad <- fx$z_multi
  z_bad[1L, 1L] <- NA
  identical(
    tryCatch(
      {
        logvar_joint_input_id(fx$sample_id, fx$qtr, z_bad, fx$gamma, fx$tau, fx$mean_systems)
        "ok"
      },
      error = function(e) "stopped"
    ),
    "stopped"
  )
}))
# Key alignment (dossier section 12): a qtr join is position-independent.
check("qtr joins are position-independent: shuffled z realigns identically", jg_try({
  jg_need("logvar_joint_align_inputs")
  set.seed(7L)
  ord <- sample.int(length(fx$qtr))
  aligned <- logvar_joint_align_inputs(fx$qtr, fx$qtr[ord], fx$z_multi[ord, , drop = FALSE])
  max(abs(aligned - fx$z_multi)) < 1e-12
}))

# Artifact fields (contract): the representation-specific dimension counts with typed
# inapplicable values, and the CSV schema including the required and excluded fields.
check("Option A and Option B carry the pinned dimension fields with typed NAs", jg_try({
  jg_need("logvar_joint_gmm_dim_fields")
  a <- logvar_joint_gmm_dim_fields("z", q = 2L, k = 3L)
  b <- logvar_joint_gmm_dim_fields("log_ppml")
  a$n_added_moments == 2L && a$n_search_parameters == 3L &&
    identical(a$n_moments_unprofiled, NA_integer_) && identical(b$n_added_moments, NA_integer_) &&
    b$n_moments_unprofiled == 10L && b$n_parameters_unprofiled == 9L &&
    b$n_moments_profiled == 8L && b$n_parameters_profiled == 7L
}))
check("the CSV schema names the provenance/status/scale fields and excludes df claims", jg_try({
  jg_need("logvar_joint_gmm_csv_fields")
  f <- logvar_joint_gmm_csv_fields
  all(c(
    "schema_version", "diagnostic", "sample_id", "joint_input_id", "decision_spec_id",
    "spec_id", "inference_status", "moment_block", "numerical_status", "search_status",
    "coverage_status", "rho_scaled_linf", "eps_floor", "guard_slack", "floor_active",
    "root_tol", "moment_delta", "algebraic_moment_excess"
  ) %in% f) &&
    !any(c("n_overid", "chi_square_df", "p_value") %in% f)
}))
check("a CSV row carries every schema column even on a skipped branch", jg_try({
  jg_need("logvar_joint_gmm_csv_row", "logvar_joint_gmm_csv_fields")
  row <- logvar_joint_gmm_csv_row(list(moment_block = "none", skip_reason = "gate_declined"))
  all(logvar_joint_gmm_csv_fields %in% names(row))
}))

# Language guard (dossier sections 2, 10; Plan 6 allowlisted grep): the comparison note
# never conflates the delta region with an identified set or a rejection, and names the
# residualized instrument correctly.
check("the comparison note is language-guarded and names the residualized instrument", jg_try({
  jg_need("build_joint_gmm_comparison")
  txt <- paste(build_joint_gmm_comparison(
    list(moment_block = "z", search_status = "not_found_within_search")
  ), collapse = " ")
  forbidden <- paste0(
    "identified set|smaller set|\\brejected\\b|\\bexcluded\\b|exact_root|",
    "region_unchanged|n_overid|chi-?square|degrees of freedom|\\bempty\\b"
  )
  !grepl(forbidden, txt, ignore.case = TRUE) &&
    grepl("orthogonal to the static variance regressors", txt)
}))
# Stage C is gated: an empty ratified delta returns the disabled reason, never a region.
check("stage C projection returns the disabled reason without a ratified delta", jg_try({
  jg_need("logvar_joint_project_set")
  out <- logvar_joint_project_set(list(), numeric(0))
  grepl("projection disabled", paste(unlist(out), collapse = " "), ignore.case = TRUE)
}))
