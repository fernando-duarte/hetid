# Shared, policy-driven rendering primitives for publication-table cells.

paper_source_once(paper_path("config", "reporting.R"))

.paper_fixed_format <- function(digits) {
  paste0("%.", as.integer(digits), "f")
}

paper_format_number <- function(
  value,
  digits,
  missing = c("nonfinite", "na"),
  missing_token = "--"
) {
  missing <- match.arg(missing)
  unavailable <- if (missing == "nonfinite") {
    !is.finite(value)
  } else {
    is.na(value)
  }
  ifelse(
    unavailable,
    missing_token,
    sprintf(.paper_fixed_format(digits), value)
  )
}

paper_format_set_interval <- function(
  lower,
  upper,
  status,
  digits,
  status_mode = c("not_bounded", "unreliable"),
  na_as_status = FALSE,
  infinite_bounds = FALSE,
  degenerate_rtol = 0
) {
  status_mode <- match.arg(status_mode)
  status_cell <- if (status_mode == "not_bounded") {
    status != PAPER_ENDPOINT_STATUS[["bounded"]]
  } else {
    status == PAPER_ENDPOINT_STATUS[["unreliable"]]
  }
  status_cell <- status_cell |
    (isTRUE(na_as_status) & (is.na(lower) | is.na(upper)))
  degenerate <- if (degenerate_rtol == 0) {
    lower == upper
  } else {
    abs(upper - lower) <= degenerate_rtol * (1 + abs(upper))
  }
  fixed <- sprintf(
    paste0(
      "$[", .paper_fixed_format(digits), ",\\,",
      .paper_fixed_format(digits), "]$"
    ),
    lower,
    upper
  )
  interval <- if (isTRUE(infinite_bounds)) {
    ifelse(
      is.infinite(lower) & is.infinite(upper),
      PAPER_ENDPOINT_STATUS[["unbounded"]],
      ifelse(
        is.infinite(lower),
        sprintf(
          paste0("$(-\\infty,\\,", .paper_fixed_format(digits), "]$"),
          upper
        ),
        ifelse(
          is.infinite(upper),
          sprintf(
            paste0("$[", .paper_fixed_format(digits), ",\\,\\infty)$"),
            lower
          ),
          fixed
        )
      )
    )
  } else {
    fixed
  }
  ifelse(status_cell, status, ifelse(degenerate, "", interval))
}

paper_format_confidence_interval <- function(
  lower,
  upper,
  digits,
  blank = FALSE,
  brackets = c("open", "closed")
) {
  brackets <- match.arg(brackets)
  marks <- if (brackets == "open") c("(", ")") else c("[", "]")
  value <- sprintf(
    paste0(
      "$", marks[[1L]], .paper_fixed_format(digits), ",\\,",
      .paper_fixed_format(digits), marks[[2L]], "$"
    ),
    lower,
    upper
  )
  ifelse(blank | !is.finite(lower) | !is.finite(upper), "", value)
}

paper_format_endpoint_envelope <- function(lower, upper, side, digits) {
  side <- ifelse(is.na(side), "none", side)
  ifelse(
    side == "none" | is.na(lower) | is.na(upper),
    "",
    ifelse(
      side == "upper",
      sprintf(
        paste0("$(-\\infty,\\,", .paper_fixed_format(digits), "]$"),
        upper
      ),
      ifelse(
        side == "lower",
        sprintf(
          paste0("$[", .paper_fixed_format(digits), ",\\,\\infty)$"),
          lower
        ),
        sprintf(
          paste0(
            "$[", .paper_fixed_format(digits), ",\\,",
            .paper_fixed_format(digits), "]$"
          ),
          lower,
          upper
        )
      )
    )
  )
}
