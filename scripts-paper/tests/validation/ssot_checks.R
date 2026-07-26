# Semantic SSOT scan for active table-acceptance definitions.

definition_files <- list.files(
  repo_root,
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)
definition_exclusions <- c(
  "/scripts-paper/validation/",
  "/archives/",
  "/generated/",
  "/snapshots/",
  "/snapshot/",
  "/scripts-paper/output/",
  "/tests/testthat/",
  "/fixtures/",
  "_checks[.]R$",
  "/test-[^/]+[.]R$",
  "/test_[^/]+[.]R$"
)
definition_active <- vapply(definition_files, function(path) {
  !any(vapply(
    definition_exclusions,
    grepl,
    logical(1),
    x = path,
    perl = TRUE
  ))
}, logical(1))
definition_files <- definition_files[definition_active]
definition_fixture <- paper_path(
  "tests",
  "validation",
  "fixtures",
  "renamed_acceptance_duplicate.R"
)
definition_patterns <- list(
  numeric_token = c(
    "gregexpr\\s*\\(",
    "regmatches\\s*\\(",
    "as.numeric\\s*\\("
  ),
  quantum = c(
    "sub\\s*\\(",
    "regexpr\\s*\\(",
    "nchar\\s*\\(",
    "10\\s*\\^"
  ),
  schema_validation = c(
    "\\$schema_version",
    "\\$published_tables",
    "identical\\s*\\(",
    "stop\\s*\\("
  ),
  schema_record = c(
    "list\\s*\\(",
    "schema_version\\s*=",
    "published_tables\\s*="
  ),
  rounding_overlap = c(
    "\\$quantum",
    "abs\\s*\\(",
    "/\\s*2",
    "[.]Machine\\$double[.]eps"
  )
)
definition_function_bodies <- function(path) {
  bodies <- character()
  visit <- function(node) {
    if (!is.call(node)) {
      return(invisible(NULL))
    }
    if (identical(node[[1L]], as.name("function"))) {
      bodies <<- c(bodies, paste(deparse(node), collapse = "\n"))
    }
    invisible(lapply(as.list(node), visit))
  }
  invisible(lapply(parse(path), visit))
  bodies
}
definition_scan <- function(paths) {
  unlist(lapply(paths, function(path) {
    bodies <- definition_function_bodies(path)
    matched <- vapply(definition_patterns, function(patterns) {
      any(vapply(bodies, function(body) {
        all(vapply(patterns, grepl, logical(1), x = body, perl = TRUE))
      }, logical(1)))
    }, logical(1))
    if (!any(matched)) {
      return(character())
    }
    paste0(path, ": ", names(definition_patterns)[matched])
  }))
}

fixture_hits <- definition_scan(definition_fixture)
stopifnot(
  any(grepl("numeric_token", fixture_hits, fixed = TRUE)),
  any(grepl("quantum", fixture_hits, fixed = TRUE)),
  any(grepl("schema_validation", fixture_hits, fixed = TRUE)),
  any(grepl("schema_record", fixture_hits, fixed = TRUE)),
  any(grepl("rounding_overlap", fixture_hits, fixed = TRUE))
)
definition_hits <- definition_scan(definition_files)
if (length(definition_hits)) {
  stop(
    "duplicate table-acceptance definitions outside scripts-paper/validation: ",
    paste(definition_hits, collapse = "; "),
    call. = FALSE
  )
}

rm(
  definition_files,
  definition_exclusions,
  definition_active,
  definition_fixture,
  definition_patterns,
  definition_function_bodies,
  definition_scan,
  fixture_hits,
  definition_hits
)
