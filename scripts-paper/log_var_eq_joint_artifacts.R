# Fixed artifact schemas and the language-guarded comparison note for the joint
# moment-compatibility layer (Plan 4, logvar-joint-gmm). Definitions only: the
# representation-specific dimension fields with typed inapplicable values, the
# frozen CSV schema (provenance/status/scale fields, never a chi-square or
# overidentification claim), the typed-NA row assembler that carries every
# schema column on any branch, and the honest-language panels-note builder. The
# versioned CSV/RDS writer with its typed round-trip guards lives in the sourced
# companion so this file stays below the repository line cap. Sourced by
# test_logvar_joint_gmm.R and the Stage B/C driver.

# Representation-specific raw dimensions (dossier section 4): Option A (the z
# block) stores its added-moment and search-parameter counts with typed NA for
# the profiled/unprofiled log/PPML geometry; Option B stores the pinned ten-
# versus-nine and eight-versus-seven counts with typed NA for the z counts.
logvar_joint_gmm_dim_fields <- function(block, q = NA_integer_, k = NA_integer_) {
  if (identical(block, "z")) {
    list(
      n_added_moments = as.integer(q), n_search_parameters = as.integer(k),
      n_moments_unprofiled = NA_integer_, n_parameters_unprofiled = NA_integer_,
      n_moments_profiled = NA_integer_, n_parameters_profiled = NA_integer_
    )
  } else if (identical(block, "log_ppml")) {
    kk <- logvar_joint_gmm_constants
    list(
      n_added_moments = NA_integer_, n_search_parameters = NA_integer_,
      n_moments_unprofiled = kk$n_moments_unprofiled,
      n_parameters_unprofiled = kk$n_parameters_unprofiled,
      n_moments_profiled = kk$n_moments_profiled,
      n_parameters_profiled = kk$n_parameters_profiled
    )
  } else {
    stop(sprintf("logvar_joint_gmm_dim_fields: unknown block '%s'", block))
  }
}

# The frozen CSV row template as typed NA defaults: identifiers/enums character,
# counts/ranks integer, flags logical, coefficients/distances/tolerances double.
# The schema version, diagnostic label, and deferred inference status are fixed;
# every other field is a typed NA the driver fills. There is deliberately no
# n_overid, chi_square_df, or p_value column -- an attained sample distance is
# never a reference-law test statistic.
.jg_csv_template <- function() {
  list(
    schema_version = "2.0.0", diagnostic = "joint_gmm",
    sample_id = NA_character_, joint_input_id = NA_character_,
    decision_spec_id = NA_character_, spec_id = NA_character_,
    inference_status = "deferred", moment_block = NA_character_,
    tau_order = NA_integer_, tau_display = NA_real_, tau_internal = NA_real_,
    numerical_status = NA_character_, search_status = NA_character_,
    coverage_status = NA_character_, rho_scaled_linf = NA_real_,
    b1 = NA_real_, b2 = NA_real_, b3 = NA_real_,
    theta0 = NA_real_, thetaR1 = NA_real_, thetaR2 = NA_real_,
    thetaR3 = NA_real_, thetaR4 = NA_real_, a_l = NA_real_, a_p = NA_real_,
    gap_a_p_a_l = NA_real_, beta1 = NA_real_, beta2 = NA_real_,
    beta3 = NA_real_, beta4 = NA_real_, fresh_moment_residual = NA_real_,
    fresh_constraint_residual = NA_real_, crossing_count = NA_integer_,
    min_abs_eps = NA_real_, eps_floor = NA_real_, guard_slack = NA_real_,
    floor_active = NA, floor_sensitivity_status = NA_character_,
    patterns_observed = NA_integer_, patterns_polished = NA_integer_,
    argmin_source = NA_character_, winning_start_type = NA_character_,
    optimizer_status = NA_character_, box_half_width = NA_real_,
    box_active = NA, box_audit_status = NA_character_, n_obs = NA_integer_,
    n_added_moments = NA_integer_, n_search_parameters = NA_integer_,
    n_moments_unprofiled = NA_integer_, n_parameters_unprofiled = NA_integer_,
    n_moments_profiled = NA_integer_, n_parameters_profiled = NA_integer_,
    jac_rank_unprofiled = NA_integer_, jac_rank_profiled = NA_integer_,
    jac_min_sv_unprofiled = NA_real_, jac_min_sv_profiled = NA_real_,
    jac_status_unprofiled = NA_character_, jac_status_profiled = NA_character_,
    active_constraint_count = NA_integer_, active_constraint_rank = NA_integer_,
    moment_active_stacked_rank = NA_integer_,
    algebraic_moment_excess = NA_integer_, param_xtol_rel = NA_real_,
    objective_tol = NA_real_, constraint_tol = NA_real_, root_tol = NA_real_,
    moment_delta = NA_real_, skip_reason = NA_character_
  )
}

# The frozen column names, the single source of truth for the CSV schema and the
# row assembler's completeness contract.
logvar_joint_gmm_csv_fields <- names(.jg_csv_template())

# One typed-NA-filled CSV row: start from the template, overlay each scalar the
# result supplies (ignoring RDS-only fields and any non-scalar entry so a skipped
# or partial branch still yields a complete one-row frame), and coerce to a
# single-row data.frame carrying every schema column.
logvar_joint_gmm_csv_row <- function(result) {
  row <- .jg_csv_template()
  for (nm in names(row)) {
    v <- result[[nm]]
    if (!is.null(v) && length(v) == 1L) row[[nm]] <- v
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

# The honest-language comparison note (dossier sections 2, 10): it states the
# software-invariance graph result, names the residualized-z instrument as the
# components orthogonal to the static variance regressors, and reports the
# evidence-calibrated search outcome as an attained upper bound. It never calls
# the tolerance region an identified or smaller set, never claims a rejection or
# an exact root, and carries no reference-law degrees-of-freedom language.
build_joint_gmm_comparison <- function(result) {
  block <- if (is.null(result$moment_block)) "none" else result$moment_block
  search <- if (is.null(result$search_status)) NA_character_ else result$search_status
  lines <- c(
    "Joint moment-compatibility diagnostic (diagnostic = joint_gmm).",
    paste(
      "Just-identified stacking of the Lewbel mean moments with the variance",
      "moments reproduces the benchmark two-step graph: a software-invariance",
      "check, not new identification."
    )
  )
  if (identical(block, "z")) {
    lines <- c(lines, paste(
      "The z block adds only the components of the Lewbel instrument orthogonal",
      "to the static variance regressors; columns already in the span of the",
      "regressors are redundant, not extra information."
    ))
  }
  if (identical(block, "log_ppml")) {
    lines <- c(lines, paste(
      "The log/PPML block shares one innovation-shape slope across the two",
      "variance links with separate intercepts and an estimated Jensen gap."
    ))
  }
  if (!is.na(search)) {
    lines <- c(lines, paste(
      sprintf("Search outcome for the %s block: %s.", block, search),
      "An attained scaled distance is only an upper bound on the global minimum."
    ))
  }
  lines
}

# The typed CSV/RDS writer and its round-trip guards.
source("scripts-paper/log_var_eq_joint_artifacts_io.R")
