# Shared artifact, table, and console helper boundary checks.
paper_source_once(paper_path(
  "support", "artifacts", "diagnostic_schema.R"
))
diagnostic_schema <- paper_diagnostic_schema(
  list(
    version = "1",
    value = NA_real_,
    status = NA_character_
  ),
  "version",
  "fixture-diagnostic"
)
diagnostic_row <- paper_diagnostic_row(
  diagnostic_schema,
  list(value = 2, status = "ok")
)
check(
  "diagnostic schema owns fixed fields and typed row order",
  identical(
    diagnostic_row,
    list(version = "1", value = 2, status = "ok")
  ) &&
    inherits(
      try(
        paper_diagnostic_row(
          diagnostic_schema,
          list(version = "stale")
        ),
        silent = TRUE
      ),
      "try-error"
    )
)
rm(diagnostic_schema, diagnostic_row)
local({
  csv_path <- tempfile(fileext = ".csv")
  collision_path <- tempfile(fileext = ".csv")
  empty_path <- tempfile(fileext = ".csv")
  rds_path <- tempfile(fileext = ".rds")
  on.exit(
    unlink(c(csv_path, collision_path, empty_path, rds_path)),
    add = TRUE
  )
  frame <- data.frame(
    logical = c(TRUE, FALSE, NA, TRUE, FALSE),
    integer = c(1L, NA_integer_, 3L, 4L, 5L),
    double = c(1.25, NA_real_, NaN, Inf, -Inf),
    character = c("plain", "with,comma", NA, "NaN", "Inf"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  returned <- paper_write_typed_csv(
    frame,
    csv_path,
    "typed-fixture"
  )
  check(
    "typed CSV supports all owned scalar types and nonfinite values",
    identical(returned, frame)
  )
  check(
    "typed CSV preserves exact column order and quoted commas",
    identical(
      names(utils::read.csv(
        csv_path,
        nrows = 0L,
        check.names = FALSE
      )),
      names(frame)
    ) &&
      any(grepl(
        "\"with,comma\"",
        readLines(csv_path, warn = FALSE),
        fixed = TRUE
      ))
  )
  collision <- data.frame(
    character = c(NA_character_, "NA", "\\NA", "\\leading"),
    stringsAsFactors = FALSE
  )
  encoded_collision <- paper_encode_typed_frame(collision)
  check(
    "typed CSV distinguishes literal NA text from missing character",
    !anyDuplicated(encoded_collision$character) &&
      identical(
        paper_write_typed_csv(
          collision,
          collision_path,
          "character-collision-fixture"
        ),
        collision
      )
  )
  empty <- frame[0L, , drop = FALSE]
  check(
    "typed CSV supports a header-only frame",
    identical(
      paper_write_typed_csv(
        empty,
        empty_path,
        "empty-fixture"
      ),
      empty
    )
  )
  mismatch <- tryCatch(
    {
      paper_check_typed_roundtrip(
        frame,
        frame[rev(names(frame))],
        "typed-prefix"
      )
      ""
    },
    error = conditionMessage
  )
  check(
    "typed round-trip errors retain their caller prefix",
    grepl("typed-prefix", mismatch, fixed = TRUE)
  )
  exact <- list(
    doubles = c(NA_real_, NaN, Inf, -Inf),
    nested = list(flag = TRUE, text = "fixture")
  )
  check(
    "exact RDS writer preserves object identity",
    identical(
      paper_write_exact_rds(
        exact,
        rds_path,
        "rds-fixture"
      ),
      exact
    ) &&
      identical(readRDS(rds_path), exact)
  )
  check(
    "year-quarter validator accepts its owned syntax",
    identical(
      paper_validate_yearquarter(
        c("2025 Q1", NA_character_),
        "quarter",
        "quarter-fixture"
      ),
      c("2025 Q1", NA_character_)
    )
  )
  check(
    "year-quarter validator rejects invalid quarters",
    inherits(
      try(
        paper_validate_yearquarter(
          "2025 Q5",
          "quarter",
          "quarter-fixture"
        ),
        silent = TRUE
      ),
      "try-error"
    )
  )
})
source(file.path(
  "scripts-paper",
  "tests",
  "support",
  "publication_helper_checks.R"
))
