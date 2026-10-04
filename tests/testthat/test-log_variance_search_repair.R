lv_repair_schema <- function(tau, lower, upper, id) {
  schema <- data.frame(
    tau = tau, coef = "b1", lower = lower, upper = upper,
    lower_status = "bounded", upper_status = "bounded"
  )
  schema$arg_lower <- I(list(c(id, -1)))
  schema$arg_upper <- I(list(c(id, 1)))
  schema
}

lv_repair_results <- function() {
  taus <- c(0.1, 0.2, 0.3)
  stats::setNames(list(
    list(schema = lv_repair_schema(0.1, -1, 1, 1)),
    list(schema = lv_repair_schema(0.2, -2, 0.5, 2)),
    list(schema = lv_repair_schema(0.3, -3, 3, 3))
  ), profile_tau_key(taus))
}

test_that("nesting checks flag shrinking intervals only", {
  rows <- data.frame(
    tau = c(0.1, 0.2), coef = "b1", lower = c(-1, -2), upper = c(1, 2),
    lower_status = "bounded", upper_status = "bounded"
  )
  expect_identical(nrow(lv_set_check_nesting(rows, 1e-6)), 0L)
  # a lower bound that rises with tau shrinks the set
  rows$lower <- c(-2, -1)
  violations <- lv_set_check_nesting(rows, 1e-6)
  expect_identical(violations$coef, "b1")
  expect_identical(violations$side, "lower")
  expect_identical(violations$tau, 0.2)
  expect_identical(violations$violation, 1)
})

test_that("nesting repair reruns the offending tau from neighbouring arguments", {
  results <- lv_repair_results()
  calls <- list()
  rerun <- function(tau, near) {
    calls[[length(calls) + 1L]] <<- list(tau = tau, near = near)
    list(schema = lv_repair_schema(tau, -2, 2, 2))
  }
  repaired <- lv_set_nesting_repair(results, rerun, 1e-6)
  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$tau, 0.2)
  expect_identical(calls[[1L]]$near, list(
    c(1, -1), c(1, 1), c(2, -1), c(2, 1), c(3, -1), c(3, 1)
  ))
  expect_identical(names(repaired$results), names(results))
  expect_identical(repaired$rows$tau, c(0.1, 0.2, 0.3))
  expect_identical(repaired$rows$upper, c(1, 2, 3))
  expect_identical(nrow(repaired$violations), 0L)
  expect_true(all(repaired$rows$upper_status == "bounded"))
})

test_that("nesting repair marks a violation the rerun cannot remove", {
  results <- lv_repair_results()
  repaired <- lv_set_nesting_repair(
    results, function(tau, near) results[[profile_tau_key(tau)]], 1e-6
  )
  expect_identical(repaired$rows$upper_status, c("bounded", "unreliable", "bounded"))
  expect_identical(repaired$rows$lower_status, rep("bounded", 3L))
  expect_identical(repaired$rows$upper, c(1, 0.5, 3))
  expect_identical(repaired$violations$side, "upper")
})

test_that("endpoint arguments outside the box are flagged in place", {
  ends <- list2env(list(
    labels = c("b1", "b2"), lower_open = c(FALSE, FALSE), upper_open = c(FALSE, FALSE),
    lower_bad = c(FALSE, FALSE), upper_bad = c(FALSE, FALSE),
    arg_lower = matrix(c(-3, 0), 2L), arg_upper = matrix(c(NA_real_, 3), 2L)
  ), parent = emptyenv())
  bounds <- list(lower = -1, upper = 1)
  escapes <- lv_set_check_endpoint_box(ends, bounds, log_variance_search_control())
  expect_identical(escapes, list(
    list(coef = "b1", side = "min", excess = 1),
    list(coef = "b2", side = "max", excess = 1)
  ))
  # a missing argument has no measurable escape and is left unflagged
  expect_identical(ends$lower_bad, c(TRUE, FALSE))
  expect_identical(ends$upper_bad, c(FALSE, TRUE))
  expect_identical(lv_set_box_escape(NA_real_, bounds), NA_real_)
  expect_identical(lv_set_box_escape(0, list(lower = -Inf, upper = 1)), NA_real_)
})

test_that("audits must have chosen their grid with the requested selector", {
  audit_entry <- function(selector_id, n_raw = 1L, ok = TRUE) {
    list(ok = ok, result = list(diagnostics = list(
      n_raw_feasible = n_raw, selector = list(selector_id = selector_id)
    )))
  }
  keys <- profile_tau_key(c(0.1, 0.2, 0.3))
  skipped <- stats::setNames(list(
    audit_entry("other", ok = FALSE),
    audit_entry("other", n_raw = 0L),
    audit_entry("other", n_raw = NA_integer_)
  ), keys)
  expect_identical(lv_set_audit_selector(skipped, "expected"), "expected")
  expect_invisible(lv_set_audit_selector(skipped, "expected"))
  matching <- stats::setNames(list(audit_entry("expected")), keys[1L])
  expect_identical(lv_set_audit_selector(matching, "expected"), "expected")
  wrong <- stats::setNames(list(audit_entry("other")), keys[1L])
  expect_error(lv_set_audit_selector(wrong, "expected"),
    "chose its grid with other",
    class = "hetid_error"
  )
})

test_that("extra starts skip infeasible points and failed fits", {
  evaluated <- list()
  check_feasible <- function(b) {
    list(feasible = b[1L] != 1, max_violation = if (b[1L] == 1) 0.5 else 0)
  }
  evaluate <- function(b, phase) {
    evaluated[[length(evaluated) + 1L]] <<- b
    if (b[1L] == 2) {
      lv_set_fit_result(c(a = NA_real_), "nonconvergence", FALSE)
    } else {
      lv_set_fit_result(c(a = b[1L] * 10), "ok", TRUE)
    }
  }
  starts <- list(c(1, 0), list(c(2, 0), list(c(3, 0))))
  out <- lv_set_extra_candidates(starts, evaluate, check_feasible)
  expect_identical(evaluated, list(c(2, 0), c(3, 0)))
  expect_identical(out$points, list(c(3, 0)))
  expect_identical(out$values, list(30))
  expect_identical(
    vapply(out$skipped, `[[`, character(1), "reason"),
    c("infeasible", "fit_failure")
  )
  expect_identical(out$skipped[[1L]]$max_violation, 0.5)
})
