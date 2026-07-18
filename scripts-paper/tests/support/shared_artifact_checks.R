# Shared artifact, table, and console helper boundary checks.

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

table_lines <- latex_table_environment(
  tabular_lines = c("\\begin{tabular}{c}", "x", "\\end{tabular}"),
  caption = "Fixture",
  label = "tab:fixture",
  notes = c("first", "second"),
  notes_label = "\\textit{Notes:}"
)
check(
  "table environment helper owns one notes wrapper",
  sum(table_lines == "\\begin{table}[!htbp]") == 1L &&
    sum(table_lines == "\\begin{tablenotes}[flushleft]") == 1L &&
    any(grepl("first second", table_lines, fixed = TRUE))
)

panel_lines <- logvar_panel_block(
  c(
    "\\begin{threeparttable}",
    "body",
    "\\end{threeparttable}"
  ),
  "note",
  "fixture"
)
check(
  "panel helper owns stable panel and notes markers",
  identical(panel_lines[[1L]], "% BEGIN LOGVAR PANEL fixture") &&
    identical(
      panel_lines[[length(panel_lines)]],
      "% END LOGVAR PANEL fixture"
    ) &&
    sum(grepl("LOGVAR NOTES fixture", panel_lines, fixed = TRUE)) == 2L
)

hull <- logvar_hull_text(data.frame(
  set_lower = c(-1, NA_real_),
  set_upper = c(2, NA_real_),
  status = c("bounded", "unreliable"),
  stringsAsFactors = FALSE
))
check(
  "console hull helper formats bounded and status rows",
  identical(hull, c("[-1,2]", "unreliable"))
)
