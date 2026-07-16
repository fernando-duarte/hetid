#!/usr/bin/env Rscript
# Run every scripts/utils test file in a fresh R process.
# Run from the package root: Rscript scripts/utils/tests/run_tests.R
#
# Each test file sources what it needs, prints its own "N passed, M failed"
# summary and quits non-zero on failure, so this runner only has to launch
# them and pass the failures on. The directory is flat and every suite is
# named test_*.R, so the set is discovered rather than listed -- a new test
# file is picked up without being registered here.

test_dir <- file.path("scripts", "utils", "tests")
paths <- sort(list.files(test_dir, pattern = "^test_.*\\.R$", full.names = TRUE))
# An empty glob would otherwise report success without running anything.
stopifnot(dir.exists(test_dir), length(paths) > 0L)

rscript <- file.path(R.home("bin"), "Rscript")
statuses <- integer(length(paths))
for (i in seq_along(paths)) {
  cat(sprintf("\n=== %s ===\n", basename(paths[[i]])))
  status <- system2(rscript, paths[[i]])
  if (is.null(status)) status <- 0L
  statuses[[i]] <- as.integer(status)
}

failed <- basename(paths[statuses != 0L])
cat(sprintf("\nfiles run: %d   files failing: %d\n", length(paths), length(failed)))
if (length(failed)) {
  cat("Failed test files:", paste(failed, collapse = ", "), "\n")
  quit(status = 1L)
}
cat("All scripts/utils test files passed.\n")
