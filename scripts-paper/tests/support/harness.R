# Minimal counter and reporting harness for fresh-process paper tests.

paper_test_harness <- function() {
  state <- new.env(parent = emptyenv())
  state$passed <- 0L
  state$failed <- 0L
  state$skipped <- 0L

  check <- function(label, condition) {
    ok <- isTRUE(tryCatch(
      condition,
      error = function(error) FALSE
    ))
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
    skip = skip,
    finish = finish,
    counts = function() as.list(state)
  )
}
