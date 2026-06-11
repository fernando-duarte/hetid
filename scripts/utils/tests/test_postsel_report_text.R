# Post-selection report text tests (headline gating, vocabulary,
# caveats, verdict wording). Run from package root:
#   Rscript scripts/utils/tests/test_postsel_report_text.R
suppressMessages(source("scripts/utils/common_settings.R"))
source("scripts/utils/postsel_split_utils.R")
source("scripts/post_selection/postsel_report_stats.R")
source("scripts/post_selection/postsel_report_text.R")
source("scripts/post_selection/postsel_report_caveats.R")
source("scripts/utils/tests/postsel_report_fixtures.R")

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

mini_study <- postsel_mini_study()
acc_good <- sim_acceptance(
  aggregate_sim_coverage(postsel_good_results())
)
acc_bad <- sim_acceptance(
  aggregate_sim_coverage(postsel_bad_results())
)

txt_nosim <- paste(
  build_postsel_summary_lines(mini_study, NULL, NULL),
  collapse = "\n"
)
check(
  "summary keeps the three-state vocabulary verbatim",
  grepl("no-certified-bound", txt_nosim, fixed = TRUE) &&
    grepl("unbounded", txt_nosim, fixed = TRUE) &&
    grepl("solver-certified finite bound", txt_nosim, fixed = TRUE)
)
check(
  "no-sim summary refuses the honest headline and validated wording",
  grepl("NOT validated", txt_nosim, fixed = TRUE) &&
    !grepl("interpretation VALIDATED", txt_nosim, fixed = TRUE) &&
    !grepl("Selection-honest split panel", txt_nosim, fixed = TRUE)
)
check(
  "summary carries the boundary and half-sample caveats",
  grepl(
    "Serial dependence across the block boundary", txt_nosim,
    fixed = TRUE
  ) &&
    grepl("mechanically wider", txt_nosim, fixed = TRUE) &&
    grepl("not a confidence statement", txt_nosim, fixed = TRUE)
)

sim_quick <- list(
  results = postsel_good_results(),
  settings = list(
    quick = TRUE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 8L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04
  )
)
txt_quick <- paste(
  build_postsel_summary_lines(mini_study, sim_quick, NULL),
  collapse = "\n"
)
check(
  "smoke-grid simulation never produces an acceptance verdict",
  grepl("NOT EVALUATED", txt_quick, fixed = TRUE) &&
    !grepl("Verdict: PASS", txt_quick, fixed = TRUE) &&
    !grepl("interpretation VALIDATED", txt_quick, fixed = TRUE)
)

sim_full <- list(
  results = postsel_good_results(),
  settings = list(
    quick = FALSE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 50L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04
  )
)
txt_pass <- paste(
  build_postsel_summary_lines(mini_study, sim_full, acc_good),
  collapse = "\n"
)
check(
  "passing full simulation prints the validated verdict",
  grepl("interpretation VALIDATED", txt_pass, fixed = TRUE) &&
    grepl("Verdict: PASS", txt_pass, fixed = TRUE)
)

sim_full_bad <- list(
  results = postsel_bad_results(),
  settings = list(
    quick = FALSE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 50L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04
  )
)
txt_fail <- paste(
  build_postsel_summary_lines(mini_study, sim_full_bad, acc_bad),
  collapse = "\n"
)
check(
  "failing full simulation prints FAIL and withholds validation",
  grepl("Verdict: FAIL", txt_fail, fixed = TRUE) &&
    !grepl("interpretation VALIDATED", txt_fail, fixed = TRUE)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
