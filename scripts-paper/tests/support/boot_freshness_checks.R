# Contract checks for the bootstrap freshness-fingerprint helpers. Run from root:
# Rscript scripts-paper/tests/support/boot_freshness_checks.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("support", "runtime", "core.R"))

check(
  "index sha reproduces the committed B=10,000 production hash",
  identical(
    paper_boot_index_sha(256L, 10L, 10000L, 20260708L),
    "5701196733f917351bcb22d111bff0d01ed8638daabf18df7561b5857b2a225e"
  )
)
check(
  "index sha reproduces the committed doubled-block B=10,000 hash",
  identical(
    paper_boot_index_sha(256L, 20L, 10000L, 20260708L),
    "d2550c39daa842fb3b14b19909bbfab1e26e4f3446c44bd80ce7874baab180ce"
  )
)
check(
  "index sha changes with the seed",
  !identical(
    paper_boot_index_sha(256L, 10L, 200L, 20260708L),
    paper_boot_index_sha(256L, 10L, 200L, 1L)
  )
)
check(
  "runtime sha is stable within a session",
  identical(paper_boot_runtime_sha(), paper_boot_runtime_sha())
)
check("code sha changes when a hashed file's content differs", {
  a <- paper_boot_code_sha(c("support/statistics/mbb_runner.R"))
  b <- paper_boot_code_sha(c("support/statistics/bootstrap_and_stationarity.R"))
  is.character(a) && nchar(a) == 64L && !identical(a, b)
})
check("freshness match returns TRUE when all fields agree", {
  p <- list(a = "x", b = "y")
  isTRUE(paper_boot_freshness_matches(p, p, c("a", "b")))
})
check(
  "freshness match names the first diverging field",
  identical(
    paper_boot_freshness_matches(list(a = "x", b = "y"), list(a = "x", b = "z"), c("a", "b")),
    "b"
  )
)

.test$finish()
