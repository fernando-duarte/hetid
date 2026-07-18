# Minimal counter and reporting harness for fresh-process paper tests.

paper_source_once(paper_path(
  "tests", "support", "logvar_fixtures.R"
))

paper_test_harness <- function() {
  state <- new.env(parent = emptyenv())
  state$passed <- 0L
  state$failed <- 0L
  state$skipped <- 0L

  safe <- function(condition) {
    isTRUE(tryCatch(
      condition,
      error = function(error) FALSE
    ))
  }

  check <- function(label, condition) {
    ok <- safe(condition)
    field <- if (ok) "passed" else "failed"
    state[[field]] <- state[[field]] + 1L
    cat(
      sprintf(
        "%s  %s\n",
        if (ok) "PASS" else "FAIL",
        label
      )
    )
    invisible(ok)
  }

  skip <- function(label, reason) {
    state$skipped <- state$skipped + 1L
    cat(sprintf("SKIP  %s (%s)\n", label, reason))
    invisible(NULL)
  }

  optional_check <- function(available, reason) {
    force(available)
    force(reason)
    function(label, condition) {
      if (!isTRUE(available)) {
        return(skip(label, reason))
      }
      check(label, condition)
    }
  }

  finish <- function() {
    suffix <- if (state$skipped) {
      sprintf(", %d skipped", state$skipped)
    } else {
      ""
    }
    cat(sprintf(
      "\n%d passed, %d failed%s\n",
      state$passed,
      state$failed,
      suffix
    ))
    if (state$failed > 0L) {
      quit(status = 1L)
    }
    invisible(as.list(state))
  }

  list(
    check = check,
    safe = safe,
    skip = skip,
    optional_check = optional_check,
    finish = finish,
    counts = function() as.list(state)
  )
}
