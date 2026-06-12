# Post-selection report wording byte pins: the fail/absent surfaces
# must stay byte-identical across the K4 rescope round (registered
# requirement). Whole-vector identical() comparisons, not substring
# greps -- an accidental rewording fails here even if every keyword
# survives. Run from package root:
#   Rscript scripts/utils/tests/test_postsel_report_wording.R
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

check(
  "refusal headline block is byte-identical to the shipped wording",
  identical(
    postsel_headline(FALSE),
    c(
      "Post-selection split study: split-selected weights",
      "====================================================",
      paste0(
        "Status: selection-honest interpretation NOT validated in",
        " this artifact set (see simulation section)."
      )
    )
  )
)

check(
  "no-sim simulation section is byte-identical to the shipped wording",
  identical(
    postsel_sim_lines(NULL, NULL),
    c(
      "Simulation validation: NOT FOUND.",
      paste0(
        "No simulation artifacts present. The split rows above are",
        " solver outputs only; the selection-honest interpretation",
        " is NOT validated in this artifact set. Run",
        " scripts/post_selection/split_simulation.R."
      ),
      ""
    )
  )
)

sim_quick <- list(
  results = postsel_good_results(),
  settings = list(
    quick = TRUE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 8L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04
  )
)
quick_lines <- postsel_sim_lines(sim_quick, NULL)
check(
  "quick-path refusal line is byte-identical to the shipped wording",
  identical(
    quick_lines[length(quick_lines) - 1L],
    paste0(
      "Acceptance: NOT EVALUATED on the smoke grid (too few",
      " replications). The selection-honest interpretation is NOT",
      " validated by this artifact; run the full simulation."
    )
  )
)

sim_full_bad <- list(
  results = postsel_bad_results(),
  settings = list(
    quick = FALSE, t_obs = 240L, tau = 0.35, phi = 0.5, reps = 50L,
    shock_dist = "uniform", kappa_eta = 1.8, rho_target = 0.04,
    k_grid = c(2L, 8L)
  )
)
acc_bad <- sim_acceptance(
  aggregate_sim_coverage(postsel_bad_results())
)
fail_lines <- postsel_sim_lines(sim_full_bad, acc_bad)
# unname(): the acceptance-check vapply gives the vector names; the
# pin compares the line's BYTES, not the subsetting artifact
check(
  "FAIL verdict line is byte-identical to the shipped wording",
  identical(
    unname(fail_lines[length(fail_lines) - 1L]),
    paste0(
      "Verdict: FAIL -- the split procedure did NOT deliver its",
      " promised coverage behavior on the synthetic DGP. Do not",
      " quote the split rows as selection-honest; per the plan's",
      " honesty-precedence rule, report this and stop."
    )
  )
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
