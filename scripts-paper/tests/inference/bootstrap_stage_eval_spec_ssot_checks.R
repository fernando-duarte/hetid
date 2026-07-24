source(file.path(
  "scripts-paper",
  "tests",
  "inference",
  "bootstrap_stage_draw_contract_checks.R"
))

effects <- 0L
guarded <- deps
guarded$estimate <- function(...) {
  effects <<- effects + 1L
  est
}
mutations <- list(
  mean_gamma = function(value) {
    value$mean$gamma <- value$system$gamma
    value
  },
  mean_taus = function(value) {
    value$mean$taus <- value$tau$display
    value
  },
  volatility_key = function(value) {
    value$log_variance$key_col <- value$frame$key_col
    value
  },
  volatility_taus = function(value) {
    value$log_variance$taus <- value$tau$union
    value
  }
)
rejected <- vapply(mutations, function(mutate) {
  inherits(
    try(
      bootstrap_stage_primary_draw(
        dat,
        mutate(stage_spec),
        context,
        guarded
      ),
      silent = TRUE
    ),
    "try-error"
  )
}, logical(1))
stopifnot(
  all(rejected),
  identical(effects, 0L),
  !"eval_specs" %in% names(formals(bootstrap_stage_primary_indexed_draw)),
  !"eval_specs" %in% names(formals(bootstrap_stage_volatility_indexed_draw))
)
cat("bootstrap_stage_eval_spec_ssot_checks: PASS\n")
