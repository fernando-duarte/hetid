# No-point and tolerance contracts are distinct from invalid regression inputs.
for (null in c(TRUE, FALSE)) {
  spec <- adapter_spec
  spec$impose_null <- null
  fit <- estimate_set_id_system(adapter_data, spec)
  check(
    sprintf("unnamed unit gamma: null=%s preserves a solved point and the recipe", null),
    !is.null(fit$point0) &&
      mean_system_equal(mean_system_reference(adapter_data, spec, adapter_tol), fit)
  )
  for (case in c("constant instrument", "duplicated news")) {
    dat <- adapter_data
    if (case == "constant instrument") dat$z <- 0 else dat$news_b <- dat$news_a
    fit <- estimate_set_id_system(dat, spec)
    positive <- build_pipeline_quadratic_system(spec$gamma, c(0.1, 0.1), fit$moments)
    check(
      sprintf("%s: null=%s retains moments and positive-tau geometry", case, null),
      is.null(fit$point0) && inherits(fit$moments, "hetid_moments") &&
        length(positive$quadratic$A_i) == 2L &&
        mean_system_equal(mean_system_reference(dat, spec, adapter_tol), fit)
    )
  }
}

local({
  original <- PAPER_QUADRATIC_CONTROL$point_identification_tolerance
  on.exit(PAPER_QUADRATIC_CONTROL$point_identification_tolerance <<- original)
  PAPER_QUADRATIC_CONTROL$point_identification_tolerance <<- 2
  expected <- mean_system_reference(adapter_data, adapter_spec, 2)
  check(
    "the paper point tolerance controls rank decisions",
    !is.null(mean_system_reference(adapter_data, adapter_spec, original)$point0) &&
      is.null(expected$point0) &&
      mean_system_equal(expected, estimate_set_id_system(adapter_data, adapter_spec))
  )
})
check(
  "the nondefault tolerance check restores the paper setting",
  identical(PAPER_QUADRATIC_CONTROL$point_identification_tolerance, adapter_tol)
)

# Reject invalid aligned systems without independently dropping observations.
for (null in c(TRUE, FALSE)) {
  spec <- adapter_spec
  spec$impose_null <- null
  for (column in c("y1", "news_a", "x", "z")) {
    for (value in c(NA_real_, NaN, Inf)) {
      dat <- adapter_data
      dat[[column]][1L] <- value
      error <- tryCatch(estimate_set_id_system(dat, spec), error = identity)
      check(
        sprintf("null=%s rejects %s in %s before estimation", null, value, column),
        inherits(error, "hetid_error_bad_argument")
      )
    }
  }
  dat <- adapter_data
  dat$x2 <- dat$x
  error <- tryCatch(estimate_set_id_system(dat, spec), error = identity)
  check(
    sprintf("null=%s rejects an aliased regression design", null),
    inherits(error, "hetid_error")
  )
  short <- tryCatch(estimate_set_id_system(adapter_data[1:3, ], spec), error = identity)
  check(
    sprintf("null=%s enforces minimum observations (observed %s)", null, class(short)[1L]),
    inherits(short, "hetid_error_insufficient_data")
  )
  bad_axes <- list(
    column_only = list(NULL, spec$y2_cols), row_only = list("z", NULL),
    wrong_row = list("wrong", spec$y2_cols), wrong_column = list("z", rev(spec$y2_cols))
  )
  for (case in names(bad_axes)) {
    named <- spec
    dimnames(named$gamma) <- bad_axes[[case]]
    error <- tryCatch(estimate_set_id_system(adapter_data, named), error = identity)
    check(
      sprintf("null=%s rejects %s gamma names", null, case),
      inherits(error, "hetid_error_bad_argument")
    )
  }
}
