# Helper function: turn the tidy structural-inference rows into the two panels
# paper_structural_var_inference_table() fills, each a set of row labels, the
# shared column headers and five columns of formatted cells. The cells use the
# paper's reporting formatters, and the renderer applies the x10 scaling and the
# decimal alignment as it did before.

paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))

# Helper function: the display rows that cannot be printed
# the renderer prints finite numbers only, so a withheld statistic, an
# unbounded side or a gated interval stops the table before anything publishes
structural_inference_unpublishable <- function(rows) {
  point <- rows$column %in% c("reference", "tau0")
  star_p <- ifelse(rows$column == "tau0", structural_inference_star_p(rows), rows$p_value)
  ok <- ifelse(point,
    is.finite(rows$estimate) & is.finite(rows$statistic) & is.finite(star_p),
    is.finite(rows$lower) & is.finite(rows$upper) &
      is.finite(rows$ci_lower) & is.finite(rows$ci_upper) &
      rows$lower_status %in% "bounded" & rows$upper_status %in% "bounded"
  )
  # the mean panel prints its OLS R^2; the variance panel prints none
  r_squared <- rows$panel != "mean" | rows$column != "reference" | is.finite(rows$r_squared)
  ok <- ok & r_squared & is.finite(rows$n_obs)
  paste(rows$panel, rows$term, rows$column, sep = "|")[!ok]
}

# Helper function: the tau = 0 p-value the stars read, on the paper's star basis
structural_inference_star_p <- function(rows) {
  point_star_p(data.frame(p_value_normal = rows$p_value, p_value = rows$p_value_empirical))
}

# Helper function: one panel's labels and five cell columns
structural_inference_panel <- function(rows, terms, labels, taus, policy) {
  columns <- c("reference", "tau0", sprintf("tau%.2f", taus))
  stopifnot(
    length(terms) == length(labels),
    nrow(rows) == length(terms) * length(columns)
  )
  statistic_digits <- PAPER_REPORTING_CONTROL$cells$statistic_digits
  number <- function(x) {
    paper_math_negative(paper_format_number(x, policy$digits, policy$numeric_missing))
  }
  column_cells <- function(column) {
    r <- rows[rows$column == column, , drop = FALSE]
    r <- r[match(terms, r$term), , drop = FALSE]
    n_obs <- unique(r$n_obs)
    stopifnot(identical(r$term, terms), length(n_obs) == 1L)
    if (column %in% c("reference", "tau0")) {
      p <- if (column == "tau0") structural_inference_star_p(r) else r$p_value
      top <- paste0(number(r$estimate), sig_stars(p))
      bottom <- sprintf(
        "(%s)",
        paper_math_negative(paper_format_number(r$statistic, statistic_digits, "na"))
      )
      r_squared <- if (column == "reference") {
        paper_format_number(r$r_squared[[1L]], statistic_digits, "na")
      } else {
        PAPER_NA_TOKEN
      }
    } else {
      # both sides are bounded once the rows pass the publication check
      top <- paper_format_set_interval(
        r$lower, r$upper, r$lower_status,
        digits = policy$digits, status_mode = policy$status_mode,
        na_as_status = policy$na_as_status, infinite_bounds = policy$infinite_bounds,
        degenerate_rtol = policy$degenerate_rtol
      )
      # a degenerate (blank) set carries no interval beneath it
      bottom <- paper_format_confidence_interval(
        r$ci_lower, r$ci_upper, policy$digits,
        blank = top == "",
        brackets = PAPER_REPORTING_CONTROL$cells$structural$confidence_brackets
      )
      r_squared <- PAPER_NA_TOKEN
    }
    c(interleave(top, bottom), r_squared, sprintf("%d", as.integer(n_obs)))
  }
  row_labels <- c(interleave(labels, ""), "$R^2$", "$N$")
  # the renderer reads the mean panel's labels as row_labels and the variance
  # panel's as rows, the names the two former panel builders used
  list(
    headers = paper_tau_col_headers(taus), row_labels = row_labels, rows = row_labels,
    columns = unname(lapply(columns, column_cells))
  )
}

# Helper function: the mean (Panel A) and variance (Panel B) panels from the rows
structural_inference_panels <- function(rows, settings) {
  unpublishable <- structural_inference_unpublishable(rows)
  if (length(unpublishable)) {
    stop("Structural inference cells are not publishable: ",
      paste(unpublishable, collapse = ", "),
      call. = FALSE
    )
  }
  intercept <- PAPER_ANALYSIS_CONTRACT$model$intercept_col
  cells <- PAPER_REPORTING_CONTROL$cells
  list(
    mean = structural_inference_panel(
      rows[rows$panel == "mean", , drop = FALSE],
      c(intercept, settings$x, settings$y2),
      c(
        "$b_0$", sprintf("$b_{%d,E}$", seq_along(settings$x)),
        sprintf("$b_{%d,N}$", seq_along(settings$y2))
      ),
      settings$taus, cells$structural
    ),
    variance = structural_inference_panel(
      rows[rows$panel == "variance", , drop = FALSE],
      c(intercept, settings$x_var),
      c("$\\theta_0$", sprintf("$\\theta_{%d,R}$", seq_along(settings$x_var))),
      settings$taus, cells$log_variance
    )
  )
}
