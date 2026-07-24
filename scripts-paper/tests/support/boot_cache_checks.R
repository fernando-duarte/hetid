#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
paper_source_once(paper_path("support", "statistics", "api.R"))
.test <- paper_test_harness()
check <- .test$check

cache_case <- function() {
  directory <- tempfile("bootstrap-cache-")
  dir.create(directory)
  list(
    path = file.path(directory, "stage.rds"),
    validator = function(value) {
      if (is.list(value) && identical(names(value), "version")) {
        TRUE
      } else {
        "bad payload"
      }
    },
    reader = readRDS,
    writer = function(value, path) saveRDS(value, path, version = 3L)
  )
}

local({
  case <- cache_case()
  payload <- list(version = "new")
  installed <- paper_boot_transactional_replace(
    payload, case$path, case$validator,
    case$reader, case$writer
  )
  check(
    "transaction installs the validated round-tripped payload",
    identical(installed$value, payload) &&
      identical(readRDS(case$path), payload)
  )
})

local({
  case <- cache_case()
  old <- list(version = "old")
  saveRDS(old, case$path, version = 3L)
  failure <- tryCatch(
    paper_boot_transactional_replace(
      list(version = "new"), case$path, case$validator,
      case$reader,
      function(value, path) stop("disk full")
    ),
    error = conditionMessage
  )
  check(
    "failed temporary write preserves the prior cache",
    identical(failure, "disk full") &&
      identical(readRDS(case$path), old)
  )
})

local({
  case <- cache_case()
  old <- list(version = "old")
  saveRDS(old, case$path, version = 3L)
  failure <- tryCatch(
    paper_boot_transactional_replace(
      list(version = "new"), case$path, case$validator,
      case$reader, case$writer,
      promoter = function(from, to) FALSE
    ),
    error = conditionMessage
  )
  check(
    "failed promotion preserves the prior cache",
    identical(failure, "atomic cache promotion failed") &&
      identical(readRDS(case$path), old)
  )
})

local({
  case <- cache_case()
  old <- list(version = "old")
  saveRDS(old, case$path, version = 3L)
  installed_read <- FALSE
  reader <- function(path) {
    if (identical(path, case$path) && installed_read) {
      installed_read <<- FALSE
      stop("post-promotion read failed")
    }
    readRDS(path)
  }
  promoter <- function(from, to) {
    promoted <- file.rename(from, to)
    installed_read <<- isTRUE(promoted)
    promoted
  }
  failure <- tryCatch(
    paper_boot_transactional_replace(
      list(version = "new"), case$path, case$validator,
      reader, case$writer, promoter
    ),
    error = conditionMessage
  )
  check(
    "post-promotion failure restores the prior valid cache",
    identical(failure, "post-promotion read failed") &&
      identical(readRDS(case$path), old)
  )
})

.test$finish()
