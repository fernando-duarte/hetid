# Contract checks for the reuse-or-run bootstrap cache dispatcher. Run from root:
# Rscript scripts-paper/tests/support/boot_cache_checks.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
paper_source_once(paper_path("support", "statistics", "api.R"))

make_case <- function() {
  dir <- tempfile("bootcache")
  dir.create(dir)
  list(
    dir = dir,
    fresh = list(
      index_sha = "h", input_sha = "d", code_sha = "c",
      runtime_sha = "r", schema = 1L
    ),
    fields = c("index_sha", "input_sha", "code_sha", "runtime_sha", "schema")
  )
}

# rerun always runs and writes
local({
  cc <- make_case()
  calls <- 0L
  path <- file.path(cc$dir, "m.rds")
  writer <- function(obj, prov) saveRDS(c(obj, list(provenance = prov)), path)
  out <- paper_boot_cached_or_run_at(path, "rerun", cc$fresh, cc$fields,
    run_fn = function() {
      calls <<- calls + 1L
      list(x = 1L)
    },
    validate_fn = function(c) TRUE, warn_label = "t", writer = writer,
    reader = function() readRDS(path)
  )
  check(
    "rerun runs run_fn and writes",
    calls == 1L && file.exists(path) &&
      identical(out$source, "rerun")
  )
})

# reuse hit: matching freshness + valid → no run
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  saveRDS(list(x = 1L, provenance = cc$fresh), path)
  calls <- 0L
  out <- paper_boot_cached_or_run_at(path, "reuse", cc$fresh, cc$fields,
    run_fn = function() {
      calls <<- calls + 1L
      list(x = 2L)
    },
    validate_fn = function(c) TRUE, warn_label = "t",
    writer = function(o, p) saveRDS(c(o, list(provenance = p)), path),
    reader = function() readRDS(path)
  )
  check(
    "reuse hit returns cached draws without running",
    calls == 0L && identical(out$source, "reuse") && out$draws$x == 1L
  )
})

# reuse hit: draws shape matches run_fn's return, no provenance leak
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  saveRDS(c(list(x = 1L, y = 2L), list(provenance = cc$fresh)), path)
  out <- paper_boot_cached_or_run_at(path, "reuse", cc$fresh, cc$fields,
    run_fn = function() list(x = 1L, y = 2L),
    validate_fn = function(c) TRUE, warn_label = "t",
    writer = function(o, p) saveRDS(c(o, list(provenance = p)), path),
    reader = function() readRDS(path)
  )
  check(
    "reuse hit draws carry no provenance leak and match run_fn's shape",
    identical(sort(names(out$draws)), c("x", "y")) && is.null(out$draws$provenance)
  )
})

# reuse miss: stale field → warn + rerun
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  stale <- cc$fresh
  stale$input_sha <- "OLD"
  saveRDS(list(x = 1L, provenance = stale), path)
  calls <- 0L
  warned <- FALSE
  out <- withCallingHandlers(
    paper_boot_cached_or_run_at(path, "reuse", cc$fresh, cc$fields,
      run_fn = function() {
        calls <<- calls + 1L
        list(x = 2L)
      },
      validate_fn = function(c) TRUE, warn_label = "t",
      writer = function(o, p) saveRDS(c(o, list(provenance = p)), path),
      reader = function() readRDS(path)
    ),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  check(
    "reuse with a stale field warns and reruns",
    calls == 1L && warned && identical(out$source, "fallback-rerun")
  )
})

# validate_fn rejection → warn + rerun
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  saveRDS(list(x = 1L, provenance = cc$fresh), path)
  calls <- 0L
  out <- suppressWarnings(paper_boot_cached_or_run_at(path, "reuse", cc$fresh, cc$fields,
    run_fn = function() {
      calls <<- calls + 1L
      list(x = 2L)
    },
    validate_fn = function(c) "bad shape", warn_label = "t",
    writer = function(o, p) saveRDS(c(o, list(provenance = p)), path),
    reader = function() readRDS(path)
  ))
  check("reuse with an invalid cache reruns", calls == 1L && out$source == "fallback-rerun")
})

# run_fn errors propagate (not swallowed as a fallback)
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  res <- tryCatch(
    paper_boot_cached_or_run_at(path, "rerun", cc$fresh, cc$fields,
      run_fn = function() stop("gate failed"),
      validate_fn = function(c) TRUE, warn_label = "t",
      writer = function(o, p) saveRDS(o, path), reader = function() readRDS(path)
    ),
    error = conditionMessage
  )
  check("run_fn failures propagate", identical(res, "gate failed"))
})

# atomic write: a failed write leaves the prior good cache intact
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  saveRDS(list(x = 99L, provenance = cc$fresh), path)
  tryCatch(
    paper_boot_cached_or_run_at(path, "rerun", cc$fresh, cc$fields,
      run_fn = function() list(x = 2L), validate_fn = function(c) TRUE, warn_label = "t",
      writer = function(o, p) stop("disk full"), reader = function() readRDS(path)
    ),
    error = function(e) NULL
  )
  check("a failed write preserves the prior cache", readRDS(path)$x == 99L)
})

# incomplete freshness fingerprint hard-stops before any run_fn/writer call
local({
  cc <- make_case()
  path <- file.path(cc$dir, "m.rds")
  incomplete <- cc$fresh
  incomplete$schema <- NA
  calls <- 0L
  res <- tryCatch(
    paper_boot_cached_or_run_at(path, "rerun", incomplete, cc$fields,
      run_fn = function() {
        calls <<- calls + 1L
        list(x = 1L)
      },
      validate_fn = function(c) TRUE, warn_label = "t",
      writer = function(o, p) saveRDS(o, path), reader = function() readRDS(path)
    ),
    error = conditionMessage
  )
  check(
    "an incomplete freshness fingerprint hard-stops without running",
    calls == 0L && !file.exists(path) &&
      is.character(res) && grepl("freshness fingerprint", res)
  )
})

.test$finish()
