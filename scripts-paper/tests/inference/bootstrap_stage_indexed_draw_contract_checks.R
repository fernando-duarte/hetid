source(file.path(
  "scripts-paper", "tests", "inference",
  "bootstrap_stage_draw_contract_checks.R"
))
seen_context <- NULL
seen_anchor <- NULL
set_id_boot_draw_from_est <- function(est, geometry, spec) {
  seen_context <<- geometry$draw_context
  list(branch = "mean")
}
logvar_set_boot_draw_from_est <- function(dat, est, geometry, spec) {
  seen_anchor <<- geometry
  list(branch = "volatility")
}
indexed <- bootstrap_stage_primary_indexed_draw(
  c(3L, 1L, 2L), 2L, stage_spec
)
stopifnot(
  identical(indexed$mean, list(branch = "mean")),
  identical(seen_context, list(
    index = c(3L, 1L, 2L), draw_id = 2L, row_order = c("c", "a", "b"),
    quarter_keys = c(3L, 1L, 2L)
  ))
)
anchor <- bootstrap_stage_volatility_draw(dat, stage_spec)
stopifnot(identical(anchor, list(branch = "volatility")), is.null(seen_anchor$draw_context))
cat("bootstrap_stage_indexed_draw_contract_checks: PASS\n")
