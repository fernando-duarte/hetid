# Real primary-stage branch boundaries with one fixed indexed draw.

bsf_index <- bsr_family$indices[[1L]]
bsf_input <- bootstrap_stage_indexed_input(bsf_index, 1L, bsr_stage_spec)
bsf_dependencies <- bootstrap_stage_draw_dependencies()
bsf_control <- bootstrap_stage_primary_draw(
  bsf_input$data,
  bsr_stage_spec,
  bsf_input$context,
  bsf_dependencies
)
bsf_shared_cases <- list(
  estimate = function(dependencies) {
    dependencies$estimate <- function(...) stop("shared estimate fixture")
    dependencies
  },
  geometry = function(dependencies) {
    dependencies$geometry <- function(...) stop("shared geometry fixture")
    dependencies
  }
)
bsf_shared_ok <- vapply(names(bsf_shared_cases), function(name) {
  dependencies <- bsf_shared_cases[[name]](bsf_dependencies)
  result <- bootstrap_stage_primary_draw(
    bsf_input$data,
    bsr_stage_spec,
    bsf_input$context,
    dependencies
  )
  expected <- if (identical(name, "estimate")) {
    "shared estimate fixture"
  } else {
    "shared geometry fixture"
  }
  identical(result, list(mean = expected, volatility = expected))
}, logical(1))
check(
  "shared estimation and geometry errors reach both real primary branches identically",
  all(bsf_shared_ok)
)
bsf_mean_dependencies <- bsf_dependencies
bsf_mean_dependencies$mean <- function(...) stop("mean evaluator fixture")
bsf_mean <- bootstrap_stage_primary_draw(
  bsf_input$data, bsr_stage_spec,
  bsf_input$context, bsf_mean_dependencies
)
check(
  "real mean evaluator errors stay mean-only",
  identical(bsf_mean$mean, "mean evaluator fixture") &&
    identical(bsf_mean$volatility, bsf_control$volatility)
)
bsf_vol_dependencies <- bsf_dependencies
bsf_vol_dependencies$volatility <- function(...) {
  stop("volatility tau-zero fixture")
}
bsf_volatility <- bootstrap_stage_primary_draw(
  bsf_input$data, bsr_stage_spec,
  bsf_input$context, bsf_vol_dependencies
)
check(
  "real volatility evaluator errors stay volatility-only",
  identical(bsf_volatility$mean, bsf_control$mean) &&
    identical(bsf_volatility$volatility, "volatility tau-zero fixture")
)

# Preserve indexed row/key identity while exercising genuine shared-fit failures.
for (case in c("collinear", "missing outcome")) {
  invalid <- bsf_input$data
  if (case == "collinear") invalid$x <- 1 else invalid$y1[1L] <- NA_real_
  error <- tryCatch(
    estimate_set_id_system(invalid, bsr_stage_spec$system),
    error = identity
  )
  expected_class <- if (case == "collinear") "hetid_error" else "hetid_error_bad_argument"
  result <- bootstrap_stage_primary_draw(invalid, bsr_stage_spec, bsf_input$context)
  check(
    paste(case, "raises a structured error at the shared estimator"),
    inherits(error, expected_class)
  )
  check(
    paste(case, "reaches both real primary branches identically"),
    inherits(error, "error") && identical(
      result, list(mean = conditionMessage(error), volatility = conditionMessage(error))
    )
  )
}
