# The combined table has a wider, decimal-aligned layout than the other
# inference tables. Its TeX scaffold is kept in the adjacent template; only
# cells derived from the current inference results are generated here.

paper_structural_var_decimal <- function(value, scale = FALSE) {
  stopifnot(grepl("^-?[0-9]+\\.[0-9]+$", value))
  if (scale) value <- sprintf("%.2f", 10 * as.numeric(value))
  strsplit(value, ".", fixed = TRUE)[[1L]]
}

paper_structural_var_estimate <- function(cell, last = FALSE,
                                          statistic = FALSE, scale = TRUE) {
  macro <- if (last) "shiftedlastestimate" else "shiftedestimate"
  clean <- gsub("$", "", cell, fixed = TRUE)
  if (statistic) {
    stopifnot(grepl("^\\(-?[0-9]+\\.[0-9]{2}\\)$", clean))
    value <- substr(clean, 2L, nchar(clean) - 1L)
    pieces <- paper_structural_var_decimal(value)
    pieces[[1L]] <- paste0("(", pieces[[1L]])
    pieces[[2L]] <- paste0(pieces[[2L]], ")")
  } else {
    star_match <- regexpr("[*]{1,3}$", clean)
    stars <- if (star_match[[1L]] < 0L) "" else regmatches(clean, star_match)
    value <- sub("[*]{1,3}$", "", clean)
    pieces <- paper_structural_var_decimal(value, scale = scale)
    if (nzchar(stars)) {
      pieces[[2L]] <- paste0(pieces[[2L]], "\\text{", stars, "}")
    }
  }
  sprintf("\\%s{%s}{%s}", macro, pieces[[1L]], pieces[[2L]])
}

paper_structural_var_interval <- function(cell) {
  if (!nzchar(cell)) {
    return(c("", ""))
  }
  clean <- gsub("$", "", cell, fixed = TRUE)
  clean <- gsub("\\,", "", clean, fixed = TRUE)
  opening <- substr(clean, 1L, 1L)
  closing <- substr(clean, nchar(clean), nchar(clean))
  stopifnot(opening %in% c("[", "("), closing %in% c("]", ")"))
  values <- strsplit(substr(clean, 2L, nchar(clean) - 1L), ",", fixed = TRUE)[[1L]]
  stopifnot(length(values) == 2L)
  left <- paper_structural_var_decimal(values[[1L]], scale = TRUE)
  right <- paper_structural_var_decimal(values[[2L]], scale = TRUE)
  c(
    sprintf("\\llap{$%s$}{%s}.%s\\intervalcomma", opening, left[[1L]], left[[2L]]),
    sprintf("{%s}.%s\\rlap{$%s$}", right[[1L]], right[[2L]], closing)
  )
}

paper_structural_var_row <- function(label, cells, statistic = FALSE) {
  stopifnot(length(cells) == 5L)
  points <- vapply(seq_len(2L), function(i) {
    paper_structural_var_estimate(cells[[i]], i == 2L, statistic)
  }, character(1))
  intervals <- unlist(lapply(cells[-c(1L, 2L)], paper_structural_var_interval))
  paste0(
    label, " & ", paste(c(points, intervals), collapse = " & "),
    if (statistic) " \\\\" else " \\\\[-2pt]"
  )
}

paper_structural_var_summary <- function(label, cells) {
  stopifnot(length(cells) == 5L)
  points <- if (identical(label, "$R^2$") && cells[[1L]] != "--") {
    c(
      paper_structural_var_estimate(cells[[1L]], scale = FALSE),
      sprintf("\\estimatesummary{%s}", cells[[2L]])
    )
  } else {
    sprintf("\\estimatesummary{%s}", cells[1:2])
  }
  intervals <- sprintf("\\intervalsummary{%s}", cells[3:5])
  paste0(label, " & ", paste(c(points, intervals), collapse = " & "), " \\\\")
}

paper_structural_var_panel_rows <- function(labels, columns) {
  stopifnot(
    length(columns) == 5L,
    length(labels) %% 2L == 0L,
    identical(tail(labels, 2L), c("$R^2$", "$N$")),
    all(vapply(columns, length, integer(1)) == length(labels))
  )
  n_coef <- (length(labels) - 2L) / 2L
  lines <- character()
  for (i in seq_len(n_coef)) {
    row <- 2L * i - 1L
    lines <- c(
      lines,
      "\\addlinespace[2pt]",
      paper_structural_var_row(
        labels[[row]], vapply(columns, `[[`, character(1), row)
      ),
      paper_structural_var_row(
        "", vapply(columns, `[[`, character(1), row + 1L),
        statistic = TRUE
      )
    )
  }
  c(
    lines,
    "\\cmidrule(l{-3pt}r{-3pt}){1-9}",
    paper_structural_var_summary(
      "$R^2$", vapply(columns, `[[`, character(1), length(labels) - 1L)
    ),
    paper_structural_var_summary(
      "$N$", vapply(columns, `[[`, character(1), length(labels))
    )
  )
}

paper_structural_var_inference_table <- function(panel_a, panel_b) {
  headers <- c(
    "OLS", "$\\tau{=}0$", "$\\tau{=}0.05$", "$\\tau{=}0.1$", "$\\tau{=}0.2$"
  )
  stopifnot(
    identical(panel_a$headers, headers),
    identical(panel_b$headers, headers)
  )
  template <- readLines(paper_path(
    "support", "latex", "structural_var_inference_template.tex"
  ), warn = FALSE)
  slots <- list(
    "{{PANEL_A_ROWS}}" = paper_structural_var_panel_rows(
      panel_a$row_labels, panel_a$columns
    ),
    "{{PANEL_B_ROWS}}" = paper_structural_var_panel_rows(
      panel_b$rows, panel_b$columns
    )
  )
  stopifnot(all(vapply(
    names(slots), function(key) sum(template == key) == 1L,
    logical(1)
  )))
  unlist(lapply(template, function(line) {
    if (line %in% names(slots)) slots[[line]] else line
  }), use.names = FALSE)
}
