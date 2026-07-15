# Offline checks for the vol-equation set-endpoint bootstrap inference layer.
# A thin entry point mirroring test_logvar_ppml.R: source the envelope module
# (which itself sources set_id_inference.R for robust_scale/boot_min_reps),
# define the shared check() counter, and source the focused check files. Run
# from the worktree root:
#   Rscript scripts/utils/tests/test_logvar_set_boot.R

source(here::here("scripts/utils/logvar_set_envelope.R"))

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

source(here::here("scripts/utils/tests/logvar_set_envelope_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
