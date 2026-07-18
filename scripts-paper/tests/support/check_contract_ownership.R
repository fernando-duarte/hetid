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
