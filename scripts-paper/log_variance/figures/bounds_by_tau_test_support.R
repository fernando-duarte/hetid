# Test-only plot-data builder for the log-variance bounds-by-tau figures. This
# pure function turns an estimator's per-tau schema and legacy set tables into
# one versioned, fixed-type, deterministically ordered regression fixture.
# Definitions only; sourced by the owning PPML suite, never by production.

# full-precision serialization of an endpoint b vector (fixed character
# type in the data frame; insensitive to options(digits))
logvar_plot_b_key <- function(b) {
  if (is.null(b) || anyNA(b)) {
    return(NA_character_)
  }
  paste(formatC(unname(b), digits = 17, format = "fg", flag = "#"),
    collapse = "|"
  )
}

# schema_list / sets: named per-tau lists from an estimator driver; the
# returned $data frame is ordered by numeric tau, the declared coefficient
# order, then side (lower before upper), with character/double columns of
# fixed type throughout
logvar_bounds_plot_data <- function(schema_list, sets, sample_id, spec_id,
                                    output_path,
                                    upstream_hash = NA_character_) {
  stopifnot(length(schema_list) > 0L)
  coef_order <- as.character(schema_list[[1L]]$coef)
  side_rows <- function(s, side) {
    arg_col <- if (side == "lower") s$arg_lower else s$arg_upper
    data.frame(
      tau = as.double(s$tau),
      coef = as.character(s$coef),
      side = rep(side, nrow(s)),
      value = as.double(if (side == "lower") s$lower else s$upper),
      status = as.character(
        if (side == "lower") s$lower_status else s$upper_status
      ),
      provenance = as.character(
        if (side == "lower") s$lower_provenance else s$upper_provenance
      ),
      endpoint_b = vapply(arg_col, logvar_plot_b_key, character(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  data <- do.call(rbind, unlist(
    lapply(schema_list, function(s) list(side_rows(s, "lower"), side_rows(s, "upper"))),
    recursive = FALSE
  ))
  data <- data[order(
    data$tau, match(data$coef, coef_order), match(data$side, c("lower", "upper"))
  ), , drop = FALSE]
  row.names(data) <- NULL
  legacy <- do.call(rbind, lapply(names(sets), function(nm) {
    tab <- sets[[nm]]
    data.frame(
      set = as.character(nm), coef = as.character(tab$coef),
      set_lower = as.double(tab$set_lower), set_upper = as.double(tab$set_upper),
      status = as.character(tab$status), stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))
  row.names(legacy) <- NULL
  list(
    version = "1.0.0",
    sample_id = as.character(sample_id),
    spec_id = as.character(spec_id),
    output_path = as.character(output_path),
    upstream_hash = as.character(upstream_hash),
    coef_order = coef_order,
    data = data,
    legacy = legacy
  )
}
