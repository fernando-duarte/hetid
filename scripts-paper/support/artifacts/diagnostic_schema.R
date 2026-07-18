# Generic typed-row and artifact protocol for diagnostic outputs.

paper_diagnostic_schema <- function(template, fixed_fields, label) {
  stopifnot(
    is.list(template),
    length(template) > 0L,
    !is.null(names(template)),
    !anyDuplicated(names(template)),
    all(fixed_fields %in% names(template)),
    is.character(label),
    length(label) == 1L,
    nzchar(label)
  )
  structure(
    list(
      template = template,
      fixed_fields = fixed_fields,
      label = label
    ),
    class = "paper_diagnostic_schema"
  )
}

paper_diagnostic_row <- function(
  schema,
  values,
  unknown = c("error", "ignore"),
  as_frame = FALSE
) {
  stopifnot(
    inherits(schema, "paper_diagnostic_schema"),
    is.list(values)
  )
  unknown <- match.arg(unknown)
  attempted <- intersect(names(values), schema$fixed_fields)
  if (length(attempted)) {
    stop(
      sprintf(
        "%s row cannot override fixed fields: %s",
        schema$label,
        paste(attempted, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  extra <- setdiff(names(values), names(schema$template))
  if (unknown == "error" && length(extra)) {
    stop(
      sprintf(
        "%s row has unknown fields: %s",
        schema$label,
        paste(extra, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  row <- schema$template
  assignable <- intersect(
    names(values),
    setdiff(names(row), schema$fixed_fields)
  )
  for (field in assignable) {
    value <- values[[field]]
    if (!is.null(value) && length(value) == 1L) {
      row[[field]] <- unname(value)
    }
  }
  if (!isTRUE(as_frame)) {
    return(row)
  }
  as.data.frame(
    row,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

paper_diagnostic_frame <- function(
  schema,
  rows,
  assembled = FALSE,
  unknown = c("error", "ignore")
) {
  stopifnot(
    inherits(schema, "paper_diagnostic_schema"),
    length(rows) > 0L
  )
  unknown <- match.arg(unknown)
  frames <- lapply(rows, function(row) {
    if (!isTRUE(assembled)) {
      return(paper_diagnostic_row(
        schema,
        row,
        unknown = unknown,
        as_frame = TRUE
      ))
    }
    stopifnot(identical(names(row), names(schema$template)))
    as.data.frame(
      row,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

paper_write_diagnostic_artifacts <- function(
  object,
  schema,
  csv_path,
  rds_path,
  artifact_name,
  assembled = FALSE,
  unknown = c("error", "ignore"),
  validate_frame = NULL
) {
  unknown <- match.arg(unknown)
  paper_write_exact_rds(object, rds_path, artifact_name)
  frame <- paper_diagnostic_frame(
    schema,
    object$rows,
    assembled = assembled,
    unknown = unknown
  )
  if (!is.null(validate_frame)) {
    validate_frame(frame)
  }
  paper_write_typed_csv(frame, csv_path, artifact_name)
  invisible(object)
}
