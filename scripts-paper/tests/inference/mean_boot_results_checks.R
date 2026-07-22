# Fixture checks for the mean-equation bootstrap's cheap results layer:
# mean_boot_results assembles set_id_boot from a collected draw set without
# touching the resample, and mean_boot_cache_validate gates a cached payload
# before it can stand in for one. Run from root:
#   Rscript scripts-paper/tests/inference/mean_boot_results_checks.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "identification", "identified_set_inference.R"))
paper_source_once(paper_path("mean_equation", "inference", "boot_results.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# a minimal two-coefficient, single-tau collected draw set: the shape
# set_id_boot_collect returns for B = 20 draws, all finite
mbr_b <- 20L
mbr_coefs <- c("b1", "th1")
set.seed(20260722L)
mbr_point_draws <- matrix(
  stats::rnorm(mbr_b * 2L), mbr_b, 2L,
  dimnames = list(NULL, mbr_coefs)
)
mbr_lower <- matrix(
  -abs(stats::rnorm(mbr_b * 2L)), mbr_b, 2L,
  dimnames = list(NULL, mbr_coefs)
)
mbr_upper <- matrix(
  abs(stats::rnorm(mbr_b * 2L)), mbr_b, 2L,
  dimnames = list(NULL, mbr_coefs)
)
mbr_collected <- list(
  point_draws = mbr_point_draws,
  n_point_deficient = 0L,
  endpoint_draws = list(
    list(
      lower = mbr_lower, upper = mbr_upper,
      status = matrix("bounded", mbr_b, 2L, dimnames = list(NULL, mbr_coefs))
    )
  ),
  tau_star_draws = stats::runif(mbr_b, 0, 0.1),
  n_capped = 1L,
  n_failed = 0L,
  failure_causes = NULL
)

mbr_set_tables <- list(tau_005 = list(
  beta1 = data.frame(
    coef = "b1", set_lower = -1, set_upper = 1, status = "bounded",
    stringsAsFactors = FALSE
  ),
  theta = data.frame(
    coef = "th1", set_lower = -0.5, set_upper = 0.5, status = "bounded",
    stringsAsFactors = FALSE
  )
))
mbr_set_id_mean_eq <- list(
  set_tables = mbr_set_tables,
  tau_display = 0.05,
  tau_baseline = 0.05,
  beta1_table = data.frame(coef = "b1", point = 0.1, stringsAsFactors = FALSE),
  theta_table = data.frame(coef = "th1", point = 0.2, stringsAsFactors = FALSE)
)
mbr_provenance <- list(
  resampler = "circular_mbb", sample_size = 100L,
  b_reps = mbr_b, block = 5L, seed = 999L,
  rng_kind = "Mersenne-Twister", block_rule = "ceiling(1.5*T^(1/3))",
  index_sha256 = "deadbeef"
)

mbr_out <- mean_boot_results(
  mbr_collected, mbr_set_id_mean_eq,
  PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
  PAPER_INFERENCE_SEARCH_CONTROL, mbr_provenance
)

check(
  "mean_boot_results returns a point_ci frame with both coefficients",
  identical(mbr_out$point_ci$coef, mbr_coefs)
)
check(
  "mean_boot_results returns one inference table per tau, named by set_tables",
  identical(names(mbr_out$inference), names(mbr_set_tables))
)
check(
  "mean_boot_results returns a tau_star_band with a finite median",
  is.finite(mbr_out$tau_star_band[["median"]])
)
check(
  "mean_boot_results reports the collected n_failed unchanged",
  identical(mbr_out$n_failed, 0L)
)
check(
  "mean_boot_results stamps cache_schema_version 1L",
  identical(mbr_out$cache_schema_version, 1L)
)
check(
  "mean_boot_results carries b_reps/block/seed from provenance, not stray locals",
  mbr_out$b_reps == mbr_provenance$b_reps &&
    mbr_out$block == mbr_provenance$block &&
    mbr_out$seed == mbr_provenance$seed
)

mbr_complete <- mbr_out[c(
  "point_draws", "endpoint_draws", "tau_star_draws", "n_failed",
  "n_capped", "n_point_deficient", "cache_schema_version", "provenance"
)]
check(
  "a complete cache payload validates TRUE",
  isTRUE(mean_boot_cache_validate(mbr_complete))
)
mbr_missing <- mbr_complete
mbr_missing$endpoint_draws <- NULL
check(
  "a payload missing endpoint_draws returns a reason string naming it",
  is.character(mean_boot_cache_validate(mbr_missing)) &&
    grepl("endpoint_draws", mean_boot_cache_validate(mbr_missing))
)
mbr_stale_schema <- mbr_complete
mbr_stale_schema$cache_schema_version <- 2L
check(
  "a schema-version mismatch returns a reason string",
  is.character(mean_boot_cache_validate(mbr_stale_schema))
)
mbr_bad_rows <- mbr_complete
mbr_bad_rows$point_draws <- mbr_bad_rows$point_draws[1:5, , drop = FALSE]
check(
  "a point_draws row count mismatched to b_reps returns a reason string",
  is.character(mean_boot_cache_validate(mbr_bad_rows))
)

.test$finish()
