# Fixture checks for the vol-equation set-endpoint bootstrap's reuse machinery:
# logvar_boot_freshness/logvar_boot_provenance recompute their fields without a
# resample, logvar_boot_code_manifest resolves to real files on the draw path,
# and logvar_boot_cache_validate's hardened reps_ok rejects a missing count
# target. Run from root:
#   Rscript scripts-paper/tests/inference/set_bootstrap_reuse_checks.R

sbr_n <- 256L
sbr_block <- 5L
sbr_sens_block <- 10L
sbr_reps <- 40L
sbr_seed <- 20260722L
sbr_prep <- list(data = data.frame(qtr = seq_len(sbr_n), x = rnorm(sbr_n)))
sbr_spec <- list(
  coefs = c("(Intercept)", "l.pc1"), gamma = matrix(1, 1, 2),
  taus = c(0, 0.05), x_cols = "x", y1_col = "y1", y2_cols = c("w2a", "w2b"),
  z_col = "z", impose_null = TRUE, pc_cols = "l.pc1",
  grid_cap = 5L, fit_budget = 300, estimator_ids = c("ppml", "harvey")
)
sbr_scale <- 1.5
sbr_logols <- c(`(Intercept)` = 0.1, l.pc1 = 0.2)

sbr_fresh <- logvar_boot_freshness(
  sbr_prep, sbr_spec, sbr_scale, sbr_logols,
  sbr_n, sbr_block, sbr_sens_block, sbr_reps, sbr_reps, sbr_seed
)
sbr_matched <- c(
  "index_sha", "sens_index_sha", "input_sha", "draw_spec_sha",
  "code_sha", "runtime_sha", "cache_schema_version"
)
check(
  "freshness carries all seven matched fields as non-NA scalars",
  paper_boot_cache_freshness_ok(sbr_fresh, sbr_matched)
)
check(
  "freshness carries the informational reps/block/seed/sample_size fields",
  identical(sbr_fresh$b_reps, sbr_reps) && identical(sbr_fresh$sens_reps, sbr_reps) &&
    identical(sbr_fresh$block, sbr_block) && identical(sbr_fresh$seed, sbr_seed) &&
    identical(sbr_fresh$sample_size, sbr_n)
)
check(
  "sens_index_sha differs from index_sha at the doubled block",
  !identical(sbr_fresh$index_sha, sbr_fresh$sens_index_sha)
)
sbr_fresh_wide <- logvar_boot_freshness(
  sbr_prep, sbr_spec, sbr_scale, sbr_logols,
  sbr_n, sbr_block, sbr_block * 3L, sbr_reps, sbr_reps, sbr_seed
)
check(
  "sens_index_sha changes when sens_block changes",
  !identical(sbr_fresh$sens_index_sha, sbr_fresh_wide$sens_index_sha)
)

sbr_prov <- logvar_boot_provenance(
  sbr_n, sbr_reps, sbr_block, sbr_sens_block, sbr_reps, sbr_seed
)
check(
  "provenance's index_sha256 matches a direct paper_boot_index_sha call",
  identical(
    sbr_prov$index_sha256,
    paper_boot_index_sha(sbr_n, sbr_block, sbr_reps, sbr_seed)
  )
)
check(
  "provenance carries sens_block and sens_reps",
  identical(sbr_prov$sens_block, sbr_sens_block) && identical(sbr_prov$sens_reps, sbr_reps)
)

sbr_manifest <- logvar_boot_code_manifest()
check(
  "code manifest includes the core/estimator files logvar_set_boot_draw executes",
  all(c(
    "log_variance/inference/set_bootstrap_core.R",
    "log_variance/estimators/ppml/estimator.R",
    "log_variance/estimators/harvey/estimator.R"
  ) %in% sbr_manifest)
)
check(
  "every code-manifest path resolves to an existing file",
  all(file.exists(vapply(sbr_manifest, function(p) {
    do.call(paper_path, as.list(strsplit(p, "/", fixed = TRUE)[[1]]))
  }, character(1))))
)

sbr_cache_cell <- function() {
  list(
    lower = matrix(0, sbr_reps, 1),
    lower_status = matrix("bounded", sbr_reps, 1),
    upper_status = matrix("bounded", sbr_reps, 1)
  )
}
sbr_cache_ok <- list(
  collected = list(ppml = list(sbr_cache_cell())),
  sens_collected = list(ppml = list(sbr_cache_cell())),
  provenance = list(b_reps = sbr_reps, sens_reps = sbr_reps)
)
check(
  "a well-formed payload validates before the b_reps hardening check",
  isTRUE(logvar_boot_cache_validate(sbr_cache_ok))
)
sbr_cache_null_b <- sbr_cache_ok
sbr_cache_null_b$provenance$b_reps <- NULL
check(
  "a NULL provenance$b_reps returns a reason string, not TRUE",
  is.character(logvar_boot_cache_validate(sbr_cache_null_b))
)

rm(
  sbr_n, sbr_block, sbr_sens_block, sbr_reps, sbr_seed, sbr_prep, sbr_spec,
  sbr_scale, sbr_logols, sbr_fresh, sbr_matched, sbr_fresh_wide,
  sbr_prov, sbr_manifest, sbr_cache_cell, sbr_cache_ok, sbr_cache_null_b
)
