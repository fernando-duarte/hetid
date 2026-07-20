#!/usr/bin/env Rscript
# Static ownership boundaries for scientific and protocol literals.

source(file.path("scripts-paper", "config", "paths.R"))

paper_root <- paper_path()
production <- list.files(
  paper_root,
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)
production <- production[
  !grepl("/tests/", production, fixed = TRUE)
]

deparsed <- function(path) {
  paste(
    vapply(
      parse(path),
      function(expression) {
        paste(deparse(expression), collapse = " ")
      },
      character(1)
    ),
    collapse = "\n"
  )
}
code <- stats::setNames(
  lapply(production, deparsed),
  production
)
relative <- function(path) {
  substring(path, nchar(paper_root) + 2L)
}

violations <- character(0)
forbid <- function(label, pattern, allow = character(0)) {
  hit <- names(code)[vapply(
    code,
    function(text) grepl(pattern, text, perl = TRUE),
    logical(1)
  )]
  hit <- hit[!relative(hit) %in% allow]
  if (length(hit)) {
    violations <<- c(
      violations,
      sprintf(
        "%s: %s",
        label,
        paste(relative(hit), collapse = ", ")
      )
    )
  }
}
source(
  file.path(
    "scripts-paper",
    "tests",
    "support",
    "contract_ownership_rules.R"
  )
)

if (length(violations)) {
  cat("Contract ownership checks failed:\n")
  cat(paste0("- ", violations, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("Contract ownership checks passed.\n")
production_r <- list.files(
  paper_path(),
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)
production_r <- production_r[
  !grepl("/tests/", production_r, fixed = TRUE)
]
paste_read <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
production_text <- paste(
  vapply(production_r, paste_read, character(1)),
  collapse = "\n"
)
stopifnot(
  !grepl("\\bp05\\b|\\bp95\\b", production_text, perl = TRUE),
  !grepl("moved no endpoint", production_text, fixed = TRUE),
  !grepl("Results are nearly identical with 15- or 8-quarter", production_text,
    fixed = TRUE
  )
)
fit_predicate_sites <- grep(
  'fit_status[^\\n]*(==|identical\\()[^\\n]*"ok"',
  strsplit(production_text, "\n", fixed = TRUE)[[1L]],
  value = TRUE,
  perl = TRUE
)
stopifnot(all(grepl("LOGVAR_FIT_STATUS", fit_predicate_sites, fixed = TRUE)))
# the forbid rule above is negative and file-granular, so it cannot notice the
# owner losing its definition; assert the single owner positively
owner_defs <- grep(
  "^quadratic_point_feasible <- function",
  strsplit(production_text, "\n", fixed = TRUE)[[1L]],
  perl = TRUE
)
stopifnot(length(owner_defs) == 1L)
# the engine oracle is a frozen verbatim copy: routing it through the shared
# helper would make its engine-equivalence comparisons tautological. Tests are
# excluded from the scan above, so guard the oracle explicitly. Deparsed, not
# read, so the guard tests code and ignores comments about the helper.
oracle_code <- deparsed(paper_path("tests", "engine", "oracle_checks.R"))
stopifnot(
  !grepl("quadratic_point_feasible", oracle_code, fixed = TRUE),
  grepl(
    "\\.feasibility_residual\\(qs,\\s*b_point,\\s*rep\\(1,\\s*length\\(",
    oracle_code,
    perl = TRUE
  )
)
