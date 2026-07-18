# Canonical search controls and the production consumers that must not restate
# them. The containing engine suite supplies check() and loads controls.R.

check(
  "specialized search controls preserve their configured values",
  identical(
    unname(unlist(LOGVAR_SEARCH_CONTROL[c(
      "fitted_lad_grid_n",
      "fitted_lad_grid_cap",
      "fitted_vol_starts_per_side",
      "logols_full_grid_safety_cap"
    )])),
    c(31, 3000, 1, 1e6)
  )
)

paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "envelope.R"
))
starts_default <- formals(logvar_fitted_vol_envelope)$starts_per_side
check(
  "fitted-volatility envelope defaults to its named search control",
  identical(
    paste(deparse(starts_default), collapse = ""),
    "LOGVAR_SEARCH_CONTROL$fitted_vol_starts_per_side"
  )
)

consumer_files <- c(
  lad = paper_path(
    "log_variance", "figures", "fitted_volatility", "run_lad.R"
  ),
  fitted = paper_path(
    "log_variance", "figures", "fitted_volatility", "run.R"
  ),
  logols = paper_path(
    "log_variance", "estimators", "log_ols", "run.R"
  ),
  analysis = paper_path("config", "analysis.R")
)
consumer_code <- vapply(
  consumer_files,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
)
check(
  "specialized search consumers reference the canonical fields",
  grepl("fitted_lad_grid_n", consumer_code[["lad"]], fixed = TRUE) &&
    grepl("fitted_lad_grid_cap", consumer_code[["lad"]], fixed = TRUE) &&
    grepl(
      "fitted_vol_starts_per_side",
      consumer_code[["fitted"]],
      fixed = TRUE
    ) &&
    grepl(
      "logols_full_grid_safety_cap",
      consumer_code[["logols"]],
      fixed = TRUE
    )
)
check(
  "specialized search consumers do not restate their control literals",
  !grepl(", 31L", consumer_code[["lad"]], fixed = TRUE) &&
    !grepl("3000L", consumer_code[["lad"]], fixed = TRUE) &&
    !grepl("<= 1e6", consumer_code[["logols"]], fixed = TRUE) &&
    !grepl(
      "logvar_fitted_vol_starts_per_side <-",
      consumer_code[["analysis"]],
      fixed = TRUE
    )
)

rm(starts_default, consumer_files, consumer_code)
