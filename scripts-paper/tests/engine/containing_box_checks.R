# Containing-bound contract: producer columns from evidence and the strict
# consumer accessor. Needs `check` and the integrated loader.

cb_error <- function(expr) inherits(tryCatch(expr, error = identity), "error")
cb_bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
cb_tab <- function(lower = c(-1, -2), upper = c(1, 2), outer_lower = c(-1.5, -2),
                   outer_upper = c(1, 2.5), status = rep(cb_bounded, 2)) {
  data.frame(
    coef = c("a", "b"), set_lower = lower, set_upper = upper,
    status = status, lower_status = status, upper_status = status,
    outer_lower = outer_lower, outer_upper = outer_upper, stringsAsFactors = FALSE
  )
}

check("containing box returns the outer columns, never the attained ones", {
  box <- paper_containing_box(cb_tab())
  identical(box$lower, c(-1.5, -2)) && identical(box$upper, c(1, 2.5))
})
check(
  "a table without containing columns is a contract breach",
  cb_error(paper_containing_box(cb_tab()[c("coef", "set_lower", "set_upper", "status")]))
)
check("a table without status is a contract breach", {
  tab <- cb_tab()
  tab$status <- NULL
  cb_error(paper_containing_box(tab))
})
check("an NA status is a contract breach", {
  tab <- cb_tab(status = c(cb_bounded, NA_character_))
  cb_error(paper_containing_box(tab))
})
check(
  "a bounded row without finite containing bounds is a contract breach",
  cb_error(paper_containing_box(cb_tab(outer_lower = c(NA, -2))))
)
check(
  "an attained endpoint outside its containing bound is an explicit error",
  cb_error(paper_containing_box(cb_tab(upper = c(1.2, 2))))
)
check("a search domain refuses a table with an unbounded row", {
  status <- c(cb_bounded, PAPER_ENDPOINT_STATUS[["unbounded"]])
  tab <- cb_tab(upper = c(1, Inf), outer_upper = c(1, Inf), status = status)
  cb_error(paper_containing_box(tab)) &&
    identical(paper_containing_box(tab, require_bounded = FALSE)$upper, c(1, Inf))
})

cb_ball <- list(
  A_i = list(diag(3), diag(3)), b_i = list(numeric(3), numeric(3)),
  c_i = c(-1, -4)
)
check("producer columns contain the unit ball and equal its extremes", {
  out <- profile_containing_bounds(paper_profile_evidence(cb_ball, diag(3)), 3L)
  all(out$outer_lower <= -1) && all(out$outer_upper >= 1) &&
    all(abs(out$outer_upper - 1) < 1e-9) && all(abs(out$outer_lower + 1) < 1e-9)
})
check("producer columns are infinite on strictly unbounded sides", {
  outside <- list(A_i = list(-diag(3)), b_i = list(numeric(3)), c_i = 1)
  evidence <- paper_profile_evidence(outside, diag(3),
    directions = matrix(c(1, 0, 0), 1)
  )
  out <- profile_containing_bounds(evidence, 3L)
  all(out$outer_lower == -Inf) && all(out$outer_upper == Inf)
})
check("producer columns are unknown without a positive definite certificate", {
  cylinder <- list(A_i = list(diag(c(1, 1, 0))), b_i = list(numeric(3)), c_i = -1)
  evidence <- paper_profile_evidence(cylinder, diag(3))
  out <- profile_containing_bounds(evidence, 3L)
  is.null(evidence$boundedness) && all(is.na(out$outer_lower[1:2])) &&
    identical(out$outer_upper[3], Inf)
})
check("the patched table producer adds containing columns to theta only", {
  tables <- coef_interval_tables_from_quadratic(
    cb_ball, c(x = 0.5),
    matrix(c(1, 0, 0), 3, dimnames = list(c("t1", "t2", "t3"), "x"))
  )
  all(c("outer_lower", "outer_upper") %in% names(tables$theta)) &&
    !any(c("outer_lower", "outer_upper") %in% names(tables$beta1)) &&
    !is.null(paper_containing_box(tables$theta))
})
