BOOTSTRAP_STAGE_CACHE_SCHEMA <- 1L
BOOTSTRAP_STAGE_CACHE_FIELDS <- c(
  "anchor", "mean", "volatility_primary",
  "volatility_primary_n_failed", "volatility_sensitivity",
  "volatility_sensitivity_n_failed", "provenance"
)
BOOTSTRAP_STAGE_CANDIDATE_FIELDS <- BOOTSTRAP_STAGE_CACHE_FIELDS[seq_len(6L)]
BOOTSTRAP_STAGE_PROVENANCE_FIELDS <- c(
  "resampler", "sample_size", "b_reps", "block", "seed", "rng_kind",
  "block_rule", "index_sha256", "post_index_rng_sha256",
  "sens_block", "sens_reps", "sens_index_sha256",
  "sens_post_index_rng_sha256", "axes", "input_sha", "draw_spec_sha",
  "code_sha", "runtime_sha", "cache_schema_version"
)
BOOTSTRAP_STAGE_SHA_FIELDS <- c(
  "index_sha256", "post_index_rng_sha256", "sens_index_sha256",
  "sens_post_index_rng_sha256", "input_sha", "draw_spec_sha",
  "code_sha", "runtime_sha"
)
BOOTSTRAP_STAGE_SHA_PATTERN <- "^[0-9a-f]{64}$"
BOOTSTRAP_STAGE_LEGACY_CACHE_BASENAMES <- c(
  "set_id_boot_draws.rds",
  "log_var_eq_set_boot_draws.rds"
)

paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_provenance.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_execution.R"
))

bootstrap_stage_cache_validators <- function(stage_spec, current) {
  spec <- stage_spec
  control <- spec$design$failure_control
  logvar_validate <- function(value, family, primary) {
    logvar_boot_collection_validate(
      value[[paste0("volatility_", family)]],
      value[[paste0("volatility_", family, "_n_failed")]],
      spec$log_variance, spec$tau$union,
      if (primary) current$b_reps else current$sens_reps,
      control, primary
    )
  }
  list(
    provenance = function(value) {
      bootstrap_stage_provenance_validate(
        value$provenance, spec, current
      )
    },
    mean = function(value) {
      mean_boot_collection_validate(
        value$mean, spec$mean, spec$tau$display,
        current$b_reps, control
      )
    },
    anchor = function(value) {
      logvar_boot_anchor_validate(
        value$anchor, spec$log_variance, spec$tau$union
      )
    },
    primary = function(value) logvar_validate(value, "primary", TRUE),
    sensitivity = function(value) {
      logvar_validate(value, "sensitivity", FALSE)
    }
  )
}

bootstrap_stage_cache_validate <- function(value, stage_spec, current) {
  valid <- bootstrap_stage_provenance_validate(
    current, stage_spec, current
  )
  if (!isTRUE(valid)) stop("invalid current provenance: ", valid)
  bootstrap_stage_payload_validate(
    value, BOOTSTRAP_STAGE_CACHE_FIELDS,
    bootstrap_stage_cache_validators(stage_spec, current)
  )
}

bootstrap_stage_cache_payload <- function(candidate, provenance, stage_spec) {
  valid <- bootstrap_stage_payload_validate(
    candidate, BOOTSTRAP_STAGE_CANDIDATE_FIELDS, list()
  )
  if (!isTRUE(valid)) stop("bootstrap stage cache: ", valid, call. = FALSE)
  value <- c(candidate, list(provenance = provenance))
  valid <- bootstrap_stage_cache_validate(value, stage_spec, provenance)
  if (!isTRUE(valid)) stop("bootstrap stage cache: ", valid, call. = FALSE)
  value
}

bootstrap_stage_cached_or_run <- function(
  path, mode, stage_spec, provenance, run_fn, reader = readRDS,
  writer = function(value, destination) {
    paper_write_exact_rds(value, destination, "bootstrap stage cache")
  },
  promoter = file.rename
) {
  stopifnot(
    mode %in% c("reuse", "rerun"),
    length(mode) == 1L,
    is.function(run_fn)
  )
  provenance_fn <- if (is.function(provenance)) {
    provenance
  } else {
    function() provenance
  }
  initial <- provenance_fn()
  validator <- function(value) {
    current <- provenance_fn()
    bootstrap_stage_cache_validate(
      value, stage_spec, current
    )
  }
  if (identical(mode, "reuse")) {
    cached <- if (file.exists(path)) {
      bootstrap_stage_read_validated(path, validator, reader)
    } else {
      list(value = NULL, reason = "no cache file")
    }
    if (is.null(cached$reason)) {
      return(list(
        stage = cached$value,
        source = "reuse",
        recovery_backup = NULL
      ))
    }
    warning(
      "bootstrap stage: ", cached$reason, "; rerunning",
      call. = FALSE
    )
    source <- "fallback-rerun"
  } else {
    source <- "rerun"
  }
  candidate <- run_fn()
  current <- provenance_fn()
  if (!identical(current, initial)) {
    stop("bootstrap stage: provenance changed during execution", call. = FALSE)
  }
  payload <- bootstrap_stage_cache_payload(
    candidate, current, stage_spec
  )
  installed <- paper_boot_transactional_replace(
    payload, path, validator, reader, writer, promoter
  )
  list(
    stage = installed$value,
    source = source,
    recovery_backup = installed$recovery_backup
  )
}

bootstrap_stage_remove_legacy_caches <- function(cache_path, validator) {
  legacy_paths <- file.path(
    dirname(cache_path),
    BOOTSTRAP_STAGE_LEGACY_CACHE_BASENAMES
  )
  bootstrap_stage_remove_validated(
    legacy_paths, cache_path, validator
  )
}
