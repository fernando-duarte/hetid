# Output-preservation checks for shared publication cell formatters.

paper_source_once(paper_path("support", "reporting", "cells.R"))

check(
  "number formatter preserves distinct NA and nonfinite policies",
  identical(
    paper_format_number(c(NA, Inf, 1.25), 2L, "na"),
    c("--", "Inf", "1.25")
  ) &&
    identical(
      paper_format_number(c(NA, Inf, 1.25), 2L, "nonfinite"),
      c("--", "--", "1.25")
    )
)
check(
  "a value rounding to zero prints unsigned, in cells and in interval bounds",
  identical(
    paper_format_number(c(-1e-9, 1e-9, -0.4e-3, -0.6e-3, -1.5), 3L, "na"),
    c("0.000", "0.000", "0.000", "-0.001", "-1.500")
  ) &&
    identical(
      paper_format_confidence_interval(-1e-9, -2e-9, 3L, brackets = "open"),
      "$(0.000,\\,0.000)$"
    ) &&
    identical(
      paper_format_number(-1e-9, 0L, "na"),
      "0"
    )
)
check(
  "set formatter preserves status, degeneracy, and infinite bounds",
  identical(
    paper_format_set_interval(
      c(0, -Inf, 0),
      c(0, 2, 1),
      c("bounded", "bounded", "unreliable"),
      digits = 3L,
      status_mode = "unreliable",
      na_as_status = TRUE,
      infinite_bounds = TRUE
    ),
    c("", "$(-\\infty,\\,2.000]$", "unreliable")
  )
)
check(
  "confidence formatter preserves open and closed intervals",
  identical(
    paper_format_confidence_interval(1, 2, 3L, brackets = "open"),
    "$(1.000,\\,2.000)$"
  ) &&
    identical(
      paper_format_confidence_interval(1, 2, 3L, brackets = "closed"),
      "$[1.000,\\,2.000]$"
    )
)
