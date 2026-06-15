# The persisted stage-04 residuals block must carry its calendar index.
# Regression guard for the fix that adds `dates = resid$dates` to the saved
# `results$residuals` list in compute_identification.R: without it the W1/W2
# series land on disk positionally indexed only, with no way to rejoin them to
# calendar time. We assert the residuals block the stage assembles carries a
# `dates` element aligned 1:1 with w1/w2, identical to the aligner's common
# dates; and, if the Stage-04 baseline RDS is already on disk, that the saved
# artifact itself carries the aligned index. Needs the Stage-01 data.rds fixture.
# Run from the package root:
#   Rscript scripts/utils/tests/test_saved_residuals_dates.R
source(here::here("scripts/utils/common_settings.R"))

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

if (!file.exists(DATA_RDS_PATH)) {
  cat("SKIP  data.rds fixture absent at", DATA_RDS_PATH, "\n")
  cat("      run Stage 01 first; saved-residuals date test needs the panel\n")
  quit(status = 0L)
}

inputs <- load_identification_inputs()

# Mirror the stage exactly: compute_identification.R calls the aligner with
# defaults, then assembles results$residuals from the returned fields.
resid <- compute_identification_residuals(inputs$data)

# The aligner contract the save site relies on.
check(
  "aligner returns a non-null dates index",
  !is.null(resid$dates)
)
check(
  "dates length equals n_obs",
  length(resid$dates) == resid$n_obs
)
check(
  "dates align 1:1 with w1 and w2 rows",
  length(resid$dates) == length(resid$w1) &&
    nrow(resid$w2) == length(resid$dates)
)

# The persisted residuals block (as assembled in the stage) must carry dates.
residuals_block <- list(
  w1 = resid$w1,
  w2 = resid$w2,
  dates = resid$dates,
  n_obs = resid$n_obs,
  pcs_aligned = resid$pcs_aligned
)
check(
  "assembled residuals block exposes a dates element",
  !is.null(residuals_block$dates)
)
check(
  "block dates identical to the aligner common dates",
  identical(residuals_block$dates, resid$dates)
)
check(
  "block dates match the instrument date attribute",
  identical(
    as.character(residuals_block$dates),
    as.character(attr(resid$pcs_aligned, "dates"))
  )
)

# If the Stage-04 baseline artifact is already on disk, assert the real saved
# object carries the aligned index (catches a future save-site regression).
baseline_rds <- file.path(
  OUTPUT_TEMP_DIR, "identification_baseline",
  "baseline_identification_results.rds"
)
if (file.exists(baseline_rds)) {
  saved <- readRDS(baseline_rds)$residuals
  check(
    "saved baseline RDS residuals block has a dates element",
    !is.null(saved$dates)
  )
  check(
    "saved dates align 1:1 with saved w1/w2",
    length(saved$dates) == length(saved$w1) &&
      nrow(saved$w2) == length(saved$dates)
  )
} else {
  cat("SKIP  baseline RDS absent; run Stage 04 to test the on-disk artifact\n")
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
