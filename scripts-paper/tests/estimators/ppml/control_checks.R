# PPML execution identity is threaded from LOGVAR_PPML_CONTROL.

ppml_acceptance_code <- paste(readLines(paper_path(
  "log_variance",
  "estimators",
  "ppml",
  "acceptance.R"
), warn = FALSE), collapse = "\n")

check(
  "PPML family and link execute through the estimator control",
  grepl("control$link", ppml_acceptance_code, fixed = TRUE) &&
    grepl("control$family", ppml_acceptance_code, fixed = TRUE) &&
    grepl("control$fit_function", ppml_acceptance_code, fixed = TRUE)
)

rm(ppml_acceptance_code)
