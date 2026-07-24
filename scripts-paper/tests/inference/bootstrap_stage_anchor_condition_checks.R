source(file.path(
  "scripts-paper",
  "tests",
  "inference",
  "bootstrap_stage_draw_contract_checks.R"
))

anchor_condition <- structure(
  list(
    message = "anchor condition fixture",
    call = quote(anchor_fixture_call()),
    token = "preserve-me"
  ),
  class = c("anchor_fixture_error", "error", "condition")
)
dependencies <- bootstrap_stage_draw_dependencies()
dependencies$estimate <- function(...) stop(anchor_condition)
stage_error <- tryCatch(
  bootstrap_stage_volatility_anchor(
    dat,
    stage_spec,
    dependencies = dependencies
  ),
  error = identity
)
captured <- bootstrap_stage_volatility_draw(
  dat,
  stage_spec,
  dependencies = dependencies
)
stopifnot(
  identical(class(stage_error), class(anchor_condition)),
  identical(conditionMessage(stage_error), conditionMessage(anchor_condition)),
  identical(stage_error$call, anchor_condition$call),
  identical(stage_error$token, anchor_condition$token),
  identical(captured, conditionMessage(anchor_condition))
)
cat("bootstrap_stage_anchor_condition_checks: PASS\n")
