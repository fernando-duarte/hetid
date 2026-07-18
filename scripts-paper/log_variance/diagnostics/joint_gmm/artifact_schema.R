# Fixed artifact schemas and the language-guarded comparison note for the joint
# moment-compatibility layer (joint-GMM, logvar-joint-gmm). Definitions only: the
# representation-specific dimension fields with typed inapplicable values, the
# frozen CSV schema (provenance/status/scale fields, never a chi-square or
# overidentification claim), the typed-NA row assembler that carries every
# schema column on any branch, and the honest-language panels-note builder. The
# versioned CSV/RDS writer with its typed round-trip guards lives in the sourced
# companion so this file stays below the repository line cap. Sourced by
# the joint-GMM test entrypoint and the search/projection driver.

paper_source_once(paper_path("config", "analysis_contract.R"))

LOGVAR_JOINT_GMM_SCHEMA <- list(
  schema_version = "3.0.0",
  diagnostic = "joint_gmm",
  inference_status = "deferred"
)

logvar_joint_gmm_schema_header <- function() {
  LOGVAR_JOINT_GMM_SCHEMA
}

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

paper_source_once(paper_path(
  "log_variance", "diagnostics", "joint_gmm", "artifact_row_schema.R"
))

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
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "artifact_io.R"))
