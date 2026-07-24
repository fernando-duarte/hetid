mbr_bad_rows <- mbr_complete
mbr_bad_rows$point_draws <- mbr_bad_rows$point_draws[1:5, , drop = FALSE]
check(
  "a point_draws row count mismatched to b_reps returns a reason string",
  is.character(mean_boot_cache_validate(mbr_bad_rows))
)

check(
  "mean_boot_code_manifest includes the draw path and excludes cheap inference",
  {
    files <- basename(mean_boot_code_manifest())
    all(c("identified_set_bootstrap.R", "tau_star.R") %in% files) &&
      !any(c("identified_set_inference.R", "inference_calibration.R") %in% files)
  }
)

# a minimal fixture with only the fields mean_boot_freshness and
# mean_boot_provenance actually read: sample size and the raw data/spec they hash
mbr_fresh_eq <- list(sample = list(n = 256L), data = data.frame(x = 1:256))
mbr_fresh_spec <- list(
  coefs = c("b1", "th1"), gamma = matrix(1, 1, 1), taus = 0.05,
  tau_grid = c(0, 0.05), y1_col = "y1", x_cols = "x1", y2_cols = "y2",
  z_col = "z", impose_null = TRUE
)
mbr_fresh_reps <- 200L
mbr_fresh_block <- 5L
mbr_fresh_seed <- 999L

mbr_freshness <- mean_boot_freshness(
  mbr_fresh_eq, mbr_fresh_spec, mbr_fresh_reps, mbr_fresh_block, mbr_fresh_seed
)
check(
  "mean_boot_freshness returns all six matched fields as non-NA scalars",
  paper_boot_cache_freshness_ok(
    mbr_freshness,
    c(
      "index_sha", "input_sha", "draw_spec_sha", "code_sha",
      "runtime_sha", "cache_schema_version"
    )
  )
)

mbr_prov <- mean_boot_provenance(
  mbr_fresh_eq, mbr_fresh_reps, mbr_fresh_block, mbr_fresh_seed
)
check(
  "mean_boot_provenance's index_sha256 matches a direct paper_boot_index_sha call",
  identical(
    mbr_prov$index_sha256,
    paper_boot_index_sha(
      mbr_fresh_eq$sample$n, mbr_fresh_block, mbr_fresh_reps, mbr_fresh_seed
    )
  )
)
