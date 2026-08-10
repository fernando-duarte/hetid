# Removed-reference, reachability, and ignored-state checks.

text_files <- list.files(
  paper_root,
  pattern = "[.](R|md)$",
  recursive = TRUE,
  full.names = TRUE
)
removed_patterns <- c(
  "run_all[.]R",
  "scripts/utils/tests/(test_logvar|logvar)",
  "scripts-paper/(?!(run_pipeline|reset_pipeline_state)[.]R)[a-z0-9_]+[.]R"
)
for (file in text_files) {
  text <- readLines(file, warn = FALSE)
  for (pattern in removed_patterns) {
    if (any(grepl(pattern, text, perl = TRUE))) {
      record_problem(
        "Removed path reference in %s: %s",
        relative_paper(file),
        pattern
      )
    }
  }
}

known_r_basenames <- basename(list.files(
  repo_root,
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
))
# quality-check.R is a real, current command (see the package CLAUDE.md) that
# lives under the permanently gitignored docs/ tree, so no checkout ever has
# it on disk; exempt its basename from the stale-reference scan below.
known_r_basenames <- c(known_r_basenames, "quality-check.R")
for (file in text_files) {
  text <- readLines(file, warn = FALSE)
  if (grepl("[.]R$", file)) {
    text <- text[grepl("^[[:space:]]*#", text)]
  }
  matches <- regmatches(
    text,
    gregexpr("[A-Za-z0-9_./*-]+[.]R\\b", text)
  )
  references <- as.character(unique(unlist(
    matches,
    use.names = FALSE
  )))
  missing <- references[
    !basename(references) %in% known_r_basenames
  ]
  if (length(missing)) {
    record_problem(
      "Stale source basename in %s: %s",
      relative_paper(file),
      paste(missing, collapse = ", ")
    )
  }
}

entrypoint <- paper_path("run_pipeline.R")
reachable <- entrypoint
frontier <- entrypoint
while (length(frontier)) {
  children <- unique(unlist(
    source_map[frontier],
    use.names = FALSE
  ))
  children <- children[startsWith(children, paper_prefix)]
  children <- intersect(children, r_files)
  frontier <- setdiff(children, reachable)
  reachable <- unique(c(reachable, frontier))
}
# test-only definitions, the standalone reset entrypoint, and the standalone
# quoted-numbers regenerator, none of which run_pipeline.R ever reaches
inactive <- c(
  paper_path("reset_pipeline_state.R"),
  paper_path(
    "log_variance",
    "tables",
    "legacy_log_ols_caption.R"
  ),
  paper_path(
    "log_variance",
    "estimators",
    "lad",
    "offline_refinement.R"
  ),
  paper_path(
    "log_variance",
    "figures",
    "bounds_by_tau_test_support.R"
  )
)
standalone_validation <- list.files(
  paper_path("validation"),
  pattern = "[.]R$",
  full.names = TRUE
)
# standalone dev tools (e.g. the EGARCH decision-record regenerator), which
# run_pipeline.R never reaches either
standalone_tools <- list.files(
  paper_path("tools"),
  pattern = "[.]R$",
  full.names = TRUE
)
production <- r_files[
  !grepl("/tests/", r_files) &
    !r_files %in% inactive &
    !r_files %in% standalone_validation &
    !r_files %in% standalone_tools
]
unreachable <- setdiff(production, reachable)
if (length(unreachable)) {
  record_problem(
    "Unreachable production modules: %s",
    paste(
      vapply(unreachable, relative_paper, character(1)),
      collapse = ", "
    )
  )
}

production_text <- unlist(
  lapply(production, readLines, warn = FALSE),
  use.names = FALSE
)
if (any(grepl(
  "docs/(execution-ledgers|superpowers/plans)",
  production_text
))) {
  record_problem("Production routing refers to ignored docs state")
}
