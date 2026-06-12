# Post-selection report stats tests (aggregation, acceptance,
# K-scope guard, exit-status mechanics). Run from package root:
#   Rscript scripts/utils/tests/test_postsel_report_stats.R
suppressMessages(source("scripts/utils/common_settings.R"))
source("scripts/utils/postsel_split_utils.R")
source("scripts/post_selection/postsel_report_stats.R")
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

good <- postsel_good_results()
cov_good <- aggregate_sim_coverage(good)
check(
  "coverage aggregation counts applicable cells per arm",
  nrow(cov_good) == 10L && all(cov_good$n == 50L) &&
    abs(cov_good$covered[
      cov_good$k_inst == 8 & cov_good$arm == "opt_full"
    ] - 0.60) < 1e-12
)
acc_good <- sim_acceptance(cov_good)
check(
  "acceptance verdict passes on a clearly repaired grid",
  all(unlist(acc_good$checks))
)
acc_bad <- sim_acceptance(
  aggregate_sim_coverage(postsel_bad_results())
)
check(
  "acceptance fails when the split misses the fixed benchmark",
  !acc_bad$checks$split_matches_fixed_benchmark
)
acc_k4 <- sim_acceptance(
  aggregate_sim_coverage(postsel_good_results_k4())
)
check(
  "frozen margins bind at the largest registered K cell",
  all(unlist(acc_k4$checks)) &&
    acc_k4$wide$k_inst[nrow(acc_k4$wide)] == 4L &&
    acc_k4$wide$k_inst[1] == 2L
)
res_k4 <- postsel_good_results_k4()
res_k8 <- postsel_good_results()
check(
  "scope guard accepts exactly the registered K grid",
  sim_k_scope_ok(
    list(settings = list(k_grid = c(2L, 4L)), results = res_k4)
  ) &&
    sim_k_scope_ok(
      list(settings = list(k_grid = c(4L, 2L)), results = res_k4)
    ) &&
    !sim_k_scope_ok(
      list(settings = list(k_grid = c(2L, 8L)), results = res_k8)
    ) &&
    !sim_k_scope_ok(
      list(
        settings = list(k_grid = c(2L, 4L, 8L)), results = res_k4
      )
    ) &&
    !sim_k_scope_ok(
      list(settings = list(k_grid = 2L), results = res_k4)
    ) &&
    !sim_k_scope_ok(list(settings = list(), results = res_k4)) &&
    !sim_k_scope_ok(NULL)
)
check(
  "scope guard rejects grid and cell mismatches in both directions",
  !sim_k_scope_ok(
    list(settings = list(k_grid = c(2L, 4L)), results = res_k8)
  ) &&
    !sim_k_scope_ok(
      list(settings = list(k_grid = c(2L, 8L)), results = res_k4)
    ) &&
    !sim_k_scope_ok(list(settings = list(k_grid = c(2L, 4L))))
)
check(
  "scope phrase is grid-exact and derived from the artifact grid",
  identical(
    postsel_k_scope_phrase(c(4L, 2L)),
    paste0(
      "on the registered K grid {2, 4} (application-parity scope",
      " up to K = 4; no claim for untested K, e.g. K = 3, or for",
      " K > 4)"
    )
  )
)
check(
  "exit status is nonzero only for a failing full-simulation verdict",
  postsel_exit_status(acc_good) == 0L &&
    postsel_exit_status(acc_bad) == 1L &&
    postsel_exit_status(NULL) == 0L
)

# End-to-end: a failing full-simulation acceptance must yield a
# nonzero process exit through the same function the report driver
# calls (system2 inherits R_LIBS_USER, so this is hermetic in the
# worktree)
bad_rds <- tempfile(fileext = ".rds")
saveRDS(postsel_bad_results(), bad_rds)
snippet <- tempfile(fileext = ".R")
writeLines(c(
  "suppressMessages(source(\"scripts/utils/common_settings.R\"))",
  "source(\"scripts/post_selection/postsel_report_stats.R\")",
  sprintf("bad <- readRDS(\"%s\")", bad_rds),
  "acc <- sim_acceptance(aggregate_sim_coverage(bad))",
  "quit(save = \"no\", status = postsel_exit_status(acc))"
), snippet)
status <- system2("Rscript", snippet, stdout = FALSE, stderr = FALSE)
check(
  "failing full simulation produces a nonzero exit",
  status != 0L
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
