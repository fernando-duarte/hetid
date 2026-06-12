# Post-selection report text tests (headline gating, K-scope gating,
# vocabulary, caveats, verdict wording). Exact whole-vector byte pins
# for the fail/absent surfaces live in
# test_postsel_report_wording.R. Run from package root:
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

# Gating-assertion strings, built with the same paste0 construction
# as production (whole-vector identical() pins live in the wording
# test file)
headline_not_validated <- paste0(
  "Status: selection-honest interpretation NOT validated in",
  " this artifact set (see simulation section)."
)
verdict_fail_line <- paste0(
  "Verdict: FAIL -- the split procedure did NOT deliver its",
  " promised coverage behavior on the synthetic DGP. Do not",
  " quote the split rows as selection-honest; per the plan's",
  " honesty-precedence rule, report this and stop."
)
# Grid-exact scope phrase expected for the registered grid {2, 4};
# production derives it from the verdict artifact via
# postsel_k_scope_phrase()
scoped_phrase <- paste0(
  "on the registered K grid {2, 4} (application-parity scope up",
  " to K = 4; no claim for untested K, e.g. K = 3, or for K > 4)"
)
headline_validated <- sprintf(
  paste0(
    "Status: selection-honest interpretation VALIDATED %s by",
    " the full simulation (verdict below)."
  ),
  scoped_phrase
)
rider_validated <- sprintf(
  paste0(
    " Validated as selection-honest %s by the full simulation",
    " (see verdict)."
  ),
  scoped_phrase
)

mini_study <- postsel_mini_study()
acc_good_k4 <- sim_acceptance(
  aggregate_sim_coverage(postsel_good_results_k4())
)
acc_good_k8 <- sim_acceptance(
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
  "no-sim summary refuses the validated wording",
  grepl(headline_not_validated, txt_nosim, fixed = TRUE) &&
    !grepl("interpretation VALIDATED", txt_nosim, fixed = TRUE)
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

sim_pass_k4 <- list(
  results = postsel_good_results_k4(),
  settings = postsel_full_settings(c(2L, 4L))
)
txt_pass <- paste(
  build_postsel_summary_lines(mini_study, sim_pass_k4, acc_good_k4),
  collapse = "\n"
)
check(
  "in-scope passing full simulation prints the grid-exact headline",
  grepl(headline_validated, txt_pass, fixed = TRUE) &&
    grepl("Verdict: PASS", txt_pass, fixed = TRUE) &&
    grepl(
      paste0("The claim is validated ", scoped_phrase),
      txt_pass,
      fixed = TRUE
    )
)
check(
  "grid-exact split-panel rider is derived from the verdict grid",
  grepl(rider_validated, txt_pass, fixed = TRUE) &&
    !grepl("VALIDATED for K <=", txt_pass, fixed = TRUE)
)

sim_pass_oos <- list(
  results = postsel_good_results(),
  settings = postsel_full_settings(c(2L, 8L))
)
txt_oos <- paste(
  build_postsel_summary_lines(mini_study, sim_pass_oos, acc_good_k8),
  collapse = "\n"
)
check(
  "out-of-scope passing verdict never yields affirmative wording",
  !grepl("interpretation VALIDATED", txt_oos, fixed = TRUE) &&
    grepl(headline_not_validated, txt_oos, fixed = TRUE) &&
    grepl("outside the registered scope", txt_oos, fixed = TRUE) &&
    grepl("margins PASS", txt_oos, fixed = TRUE)
)

sim_pass_legacy <- list(
  results = postsel_good_results_k4(),
  settings = list(
    quick = FALSE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 50L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04
  )
)
txt_legacy <- paste(
  build_postsel_summary_lines(
    mini_study, sim_pass_legacy, acc_good_k4
  ),
  collapse = "\n"
)
check(
  "verdict artifacts without a recorded K grid fail closed",
  !grepl("interpretation VALIDATED", txt_legacy, fixed = TRUE) &&
    grepl(headline_not_validated, txt_legacy, fixed = TRUE)
)

sim_full_bad <- list(
  results = postsel_bad_results(),
  settings = postsel_full_settings(c(2L, 8L))
)
txt_fail <- paste(
  build_postsel_summary_lines(mini_study, sim_full_bad, acc_bad),
  collapse = "\n"
)
check(
  "failing full simulation withholds validation",
  grepl(verdict_fail_line, txt_fail, fixed = TRUE) &&
    grepl(headline_not_validated, txt_fail, fixed = TRUE) &&
    !grepl("interpretation VALIDATED", txt_fail, fixed = TRUE)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
