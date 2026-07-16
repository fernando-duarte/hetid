# Statistical reporting and data-validation functions.

#' Create formatted gt table with consistent styling
#' @param data data frame
#' @param title table title
#' @param subtitle optional subtitle
#' @param highlight_rows rows to highlight
#' @param highlight_color color for highlighting
#' @return gt table object
create_formatted_table <- function(data, title, subtitle = NULL,
                                   highlight_rows = NULL,
                                   highlight_color = "lightblue") {
  tbl <- data |>
    gt() |>
    tab_header(
      title = title,
      subtitle = subtitle
    ) |>
    tab_options(
      table.font.size = 12,
      data_row.padding = px(2)
    )

  # Add highlighting if specified
  if (!is.null(highlight_rows)) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = highlight_color),
        locations = cells_body(rows = highlight_rows)
      )
  }

  tbl
}

#' Create interactive datatable with consistent settings
#' @param data data frame
#' @param page_length number of rows per page
#' @param round_digits digits to round numeric columns
#' @return DT datatable object
create_interactive_table <- function(data, page_length = 10, round_digits = 4) {
  # Find numeric columns
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

  dt <- datatable(
    data,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    ),
    extensions = "Buttons",
    rownames = FALSE
  )

  # Round numeric columns
  if (length(numeric_cols) > 0) {
    dt <- dt |>
      formatRound(columns = numeric_cols, digits = round_digits)
  }

  dt
}

#' Test data for missing values and report
#' @param data data frame or list
#' @param stop_on_na whether to stop execution if NA found
#' @return logical indicating if data is complete
check_data_completeness <- function(data, stop_on_na = TRUE) {
  if (is.data.frame(data)) {
    na_check <- colSums(is.na(data))
    has_na <- any(na_check > 0)
  } else if (is.list(data)) {
    na_check <- vapply(
      data, function(x) sum(is.na(x)), integer(1)
    )
    has_na <- any(na_check > 0)
  } else {
    has_na <- any(is.na(data))
  }

  if (has_na) {
    if (is.data.frame(data) || is.list(data)) {
      na_vars <- names(na_check)[na_check > 0]
      message <- paste("Missing values found in:", paste(na_vars, collapse = ", "))
    } else {
      message <- "Missing values found in data"
    }

    if (stop_on_na) {
      stop(message)
    } else {
      cli::cli_alert_warning(message)
    }
  }

  !has_na
}
