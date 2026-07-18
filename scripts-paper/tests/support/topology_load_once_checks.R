# Fresh temporary-module checks for load-once and circular-source behavior.

local({
  once_path <- tempfile(fileext = ".R")
  independent_path <- tempfile(fileext = ".R")
  first_path <- tempfile(fileext = ".R")
  second_path <- tempfile(fileext = ".R")
  on.exit(
    {
      unlink(c(
        once_path,
        independent_path,
        first_path,
        second_path
      ))
      if (exists(
        ".paper_topology_once_count",
        envir = .GlobalEnv,
        inherits = FALSE
      )) {
        rm(
          ".paper_topology_once_count",
          envir = .GlobalEnv
        )
      }
      if (exists(
        ".paper_topology_independent_count",
        envir = .GlobalEnv,
        inherits = FALSE
      )) {
        rm(
          ".paper_topology_independent_count",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  writeLines(c(
    ".paper_topology_once_count <- get0(",
    "  \".paper_topology_once_count\",",
    "  ifnotfound = 0L",
    ") + 1L"
  ), once_path)
  paper_source_once(once_path)
  paper_source_once(once_path)
  if (!identical(.paper_topology_once_count, 1L)) {
    record_problem(
      "paper_source_once evaluated a module more than once"
    )
  }

  writeLines(c(
    ".paper_topology_independent_count <- get0(",
    "  \".paper_topology_independent_count\",",
    "  ifnotfound = 0L",
    ") + 1L"
  ), independent_path)
  paper_source_once(independent_path)
  paper_source_once(independent_path)
  if (!identical(.paper_topology_independent_count, 1L)) {
    record_problem(
      "paper_source_once did not isolate a second module counter"
    )
  }

  writeLines(
    sprintf(
      "paper_source_once(%s)",
      encodeString(second_path, quote = "\"")
    ),
    first_path
  )
  writeLines(
    sprintf(
      "paper_source_once(%s)",
      encodeString(first_path, quote = "\"")
    ),
    second_path
  )
  circular <- tryCatch(
    {
      paper_source_once(first_path)
      ""
    },
    error = conditionMessage
  )
  if (!grepl(
    "Circular paper source dependency",
    circular,
    fixed = TRUE
  )) {
    record_problem(
      "paper_source_once did not reject a circular dependency"
    )
  }
})
