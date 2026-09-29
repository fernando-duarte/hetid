#!/usr/bin/env Rscript
# Structural-inference settings, prepared inputs and the OLS reference column,
# rebuilt from the paper's frozen inputs and pinned to the macro_dynamics
# production run. Run from root:
#   Rscript scripts-paper/tests/inference/test_structural_inference_inputs.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis.R"))
paper_source_once(paper_path("support", "statistics", "mbb_protocol_authority.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("log_variance", "engine", "contracts.R"))
for (name in c("failures", "settings", "inputs", "arrays", "reference")) {
  paper_source_once(paper_path("support", "structural_inference", paste0(name, ".R")))
}
paper_source_once(paper_path("support", "data", "acm_inputs.R"))
paper_source_once(paper_path("support", "data", "frozen_inputs.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

paper_verify_frozen_inputs()
quarterly_acm_inputs <- suppressWarnings(paper_load_quarterly_acm(all_mats))
for (name in c(
  "build_sdf_series", "build_consumption_growth", "build_yield_volatility",
  "build_asset_return_pcs", "build_sdf_pcs"
)) {
  paper_source_once(paper_path("data_preparation", paste0(name, ".R")))
}

# Settings ------------------------------------------------------------------
settings <- structural_inference_settings(n_grid = 41L, n_points = 20L)
check("settings take the paper's draws, seed and MBB block rule", {
  identical(settings$n_draws, boot_reps) && identical(settings$seed, boot_seed) &&
    identical(settings$block_length, 10L) &&
    identical(settings$min_reps, as.integer(ceiling(boot_reps / 2)))
})
check("settings window is the paper window one quarter earlier", {
  identical(c(settings$date_begin, settings$date_end), c("1961 Q4", "2025 Q3"))
})
check("settings name the active instrument, led to the origin", {
  identical(settings$z, paste0("f", paper_instrument_choice()$column)) &&
    identical(settings$inputs[[settings$z]], paper_instrument_choice()$column)
})
check("settings honour pilot overrides", {
  pilot <- structural_inference_settings(
    n_draws = 20L, seed = 7L, block_length = 4L, n_grid = 41L, n_points = 20L
  )
  identical(
    unlist(pilot[c("n_draws", "seed", "block_length", "n_grid", "n_points", "min_reps")]),
    c(n_draws = 20L, seed = 7L, block_length = 4L, n_grid = 41L, n_points = 20L, min_reps = 10L)
  )
})
check("settings refuse a published mean or volatility spec other than B", local({
  original <- PAPER_SPEC_PLAN
  on.exit(assign("PAPER_SPEC_PLAN", original, envir = globalenv()))
  plans <- list(list(mean = c("A", "B"), volatility = "B"), list(mean = "B", volatility = "A"))
  all(vapply(plans, function(plan) {
    assign("PAPER_SPEC_PLAN", plan, envir = globalenv())
    error <- tryCatch(structural_inference_settings(), error = identity)
    inherits(error, "error") && grepl("spec B", conditionMessage(error), fixed = TRUE)
  }, TRUE))
}))
check("settings reject an instrument column that is not a paper choice", {
  inherits(tryCatch(structural_inference_settings(z_col = "vfci"), error = identity), "error")
})

# Prepared inputs -----------------------------------------------------------
prepared <- paper_structural_inference_prepare(settings)
check("prepared samples are 256 mean and 255 variance origins", {
  prepared$n_obs == 256L && prepared$variance_n_obs == 255L && !prepared$variance[1L] &&
    identical(format(range(prepared$qtr)), c("1961 Q4", "2025 Q3"))
})
check("origins are the paper's response quarters one quarter earlier", {
  identical(format(prepared$qtr[c(1L, 256L)] + 1L), c(date_begin, date_end))
})
check("prepared arrays keep their pinned values", {
  path <- tempfile()
  writeLines(sprintf("%.17g", unlist(prepared[c("y", "x", "y2", "z", "x_var")])), path)
  identical(unname(tools::md5sum(path)), "6372d26181fdd2cb1212841f45f9c992")
})
check("identity draw centres PC_R over the variance sample", {
  arrays <- structural_inference_arrays(prepared, seq_len(prepared$n_obs))
  max(abs(colMeans(arrays$x_var))) < 1e-15 && identical(arrays$y, prepared$y)
})

# Reference column ----------------------------------------------------------
reference <- structural_inference_reference(prepared, settings)
frame <- reference$frame
check("reference frame keeps the macro schema", {
  identical(names(frame), c(
    "panel", "term", "estimate", "se", "statistic", "p_value", "reference_distribution",
    "df", "n_obs", "r_squared", "available", "reason"
  )) &&
    identical(frame$reference_distribution, rep(c("t", "normal"), c(7L, 5L)))
})
check("reference estimates and statistics match the production run", {
  estimate <- c(
    0.79574249641165562, 0.00041754827279746827, -0.45756134040327251,
    0.92660417514833804, 0.015748208807755088, 0.028026843484397958,
    -0.18021601606482046, -1.3023541743052764, 0.18178273080045901,
    -0.0070395026933464111, 0.79152230353382957, 1.3459093908383499
  )
  statistic <- c(
    14.524940385561752, 0.074752550140300467, -4.4861218139120078,
    3.1628397031811191, 4.1139752677576427, 1.0166934717954825,
    -1.6900669365243863, -5.6495736005829649, 1.7768251253436453,
    -0.029256816877253629, 4.1693235866504912, 3.7310711269516683
  )
  isTRUE(all.equal(frame$estimate, estimate, tolerance = 1e-12)) &&
    isTRUE(all.equal(frame$statistic, statistic, tolerance = 1e-12))
})
check("reference tails are t on OLS df for the mean and normal for PPML", {
  mean <- frame$panel == "mean"
  all(frame$df[mean] == prepared$n_obs - 7L) && all(is.na(frame$df[!mean])) &&
    isTRUE(all.equal(
      frame$p_value[!mean], 2 * stats::pnorm(-abs(frame$statistic[!mean]))
    ))
})

.test$finish()
