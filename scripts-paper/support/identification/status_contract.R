# Closed endpoint-state vocabulary and precedence.

PAPER_ENDPOINT_STATUS <- c(
  bounded = "bounded",
  unbounded = "unbounded",
  unreliable = "unreliable",
  failed = "failed"
)

paper_endpoint_status_validate <- function(
  status,
  allow_failed = TRUE
) {
  allowed <- if (isTRUE(allow_failed)) {
    unname(PAPER_ENDPOINT_STATUS)
  } else {
    unname(PAPER_ENDPOINT_STATUS[c(
      "bounded", "unbounded", "unreliable"
    )])
  }
  stopifnot(
    is.character(status),
    !anyNA(status),
    all(status %in% allowed)
  )
  invisible(status)
}

paper_endpoint_status_worst <- function(
  status,
  allow_failed = FALSE
) {
  paper_endpoint_status_validate(
    status,
    allow_failed = allow_failed
  )
  order <- unname(PAPER_ENDPOINT_STATUS[c(
    "bounded", "unbounded", "unreliable", "failed"
  )])
  order[[max(match(status, order))]]
}

paper_endpoint_status_reduce <- function(...) {
  status <- list(...)
  stopifnot(
    length(status) > 0L,
    length(unique(lengths(status))) == 1L
  )
  vapply(seq_len(length(status[[1L]])), function(index) {
    paper_endpoint_status_worst(vapply(
      status,
      `[[`,
      character(1),
      index
    ))
  }, character(1))
}

paper_endpoint_status_from_flags <- function(bounded, valid) {
  stopifnot(
    length(bounded) == length(valid),
    !anyNA(bounded),
    !anyNA(valid)
  )
  ifelse(
    !valid,
    PAPER_ENDPOINT_STATUS[["unreliable"]],
    ifelse(
      bounded,
      PAPER_ENDPOINT_STATUS[["bounded"]],
      PAPER_ENDPOINT_STATUS[["unbounded"]]
    )
  )
}
