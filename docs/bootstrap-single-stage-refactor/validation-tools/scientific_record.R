bootstrap_validation_fully_failed_rows <- function(collected) {
  first <- collected[[1L]][[1L]]$lower_status
  failed <- PAPER_ENDPOINT_STATUS[["failed"]]
  fully_failed <- rep(TRUE, nrow(first))
  for (estimator in collected) {
    for (cell in estimator) {
      cell_failed <- cell$lower_status == failed &
        cell$upper_status == failed
      fully_failed <- fully_failed & apply(cell_failed, 1L, all)
    }
  }
  as.integer(sum(fully_failed))
}

bootstrap_validation_cache_projection <- function(output_root) {
  state_root <- file.path(output_root, "state")
  unified <- file.path(state_root, "bootstrap_stage_draws.rds")
  if (file.exists(unified)) {
    cache <- readRDS(unified)
    return(list(
      mean = cache$mean,
      volatility_primary = cache$volatility_primary,
      volatility_primary_n_failed =
        cache$volatility_primary_n_failed,
      volatility_sensitivity = cache$volatility_sensitivity,
      volatility_sensitivity_n_failed =
        cache$volatility_sensitivity_n_failed
    ))
  }
  mean <- readRDS(file.path(state_root, "set_id_boot_draws.rds"))
  volatility <- readRDS(file.path(
    state_root,
    "log_var_eq_set_boot_draws.rds"
  ))
  mean$provenance <- NULL
  list(
    mean = mean,
    volatility_primary = volatility$collected,
    volatility_primary_n_failed = volatility$n_failed,
    volatility_sensitivity = volatility$sens_collected,
    volatility_sensitivity_n_failed =
      bootstrap_validation_fully_failed_rows(
        volatility$sens_collected
      )
  )
}

bootstrap_validation_numeric_text <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  pattern <- paste0(
    "[-+]?(?:",
    "(?:[0-9]+(?:[.][0-9]*)?)|(?:[.][0-9]+)",
    ")(?:[eE][-+]?[0-9]+)?"
  )
  matches <- gregexpr(pattern, text, perl = TRUE)
  tokens <- regmatches(text, matches)[[1L]]
  list(
    text = gsub(pattern, "<number>", text, perl = TRUE),
    numbers = if (identical(tokens, character())) {
      double()
    } else {
      as.numeric(tokens)
    }
  )
}

bootstrap_validation_artifact_projection <- function(output_root) {
  csv_paths <- c(
    mean = "diagnostics/set_id_inference_diagnostics.csv",
    volatility =
      "diagnostics/log_var_eq_set_inference_diagnostics.csv"
  )
  tex_paths <- c(
    structural_mean = "tables/structural_eq_inference.tex",
    structural_variance = "tables/structural_var_inference.tex",
    volatility = "tables/log_var_eq_panels_inference.tex"
  )
  list(
    diagnostics = lapply(
      file.path(output_root, csv_paths),
      utils::read.csv,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    tables = lapply(
      file.path(output_root, tex_paths),
      bootstrap_validation_numeric_text
    )
  )
}

bootstrap_validation_record <- function(output_root) {
  stopifnot(
    exists("set_id_boot", inherits = TRUE),
    exists("log_var_eq_set_boot", inherits = TRUE)
  )
  list(
    schema_version = 1L,
    set_id_boot = get("set_id_boot", inherits = TRUE),
    log_var_eq_set_boot = get("log_var_eq_set_boot", inherits = TRUE),
    bootstrap_collections =
      bootstrap_validation_cache_projection(output_root),
    published_inference =
      bootstrap_validation_artifact_projection(output_root)
  )
}
