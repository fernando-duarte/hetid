# Helper function: recognize only documented package numerical errors
structural_inference_numeric_error <- function(condition, stage) {
  # hetid sources of these messages: run_pc_regression.R, identified_set_search.R,
  # line_quadratic_roots.R and quadratic_relative_feasibility.R
  if (!identical(class(condition), c("hetid_error", "error", "condition"))) {
    return(FALSE)
  }
  message <- conditionMessage(condition)
  if (stage == "mean fit") {
    return(startsWith(message, "Rank-deficient regression design: aliased coefficient(s) ") &&
      endsWith(message, ". The conditioning columns are collinear."))
  }
  if (stage == "mean box" &&
    startsWith(message, "the Q stack is singular, so no search frame exists: ")) {
    return(TRUE)
  }
  allowed <- c(
    "Line constraint coefficients exceed the numeric range",
    "A finite line constraint root is outside the numeric range",
    "Line constraint root scaling exceeds the numeric range",
    "Constraint-relative feasibility arithmetic exceeds the numeric range"
  )
  stage %in% c("mean box", "variance sample") && message %in% allowed
}

# Helper function: retain recognized numerical failures and rethrow contract errors
structural_inference_capture <- function(expression, stage) {
  tryCatch(list(value = force(expression), error = NULL), hetid_error = function(condition) {
    if (!structural_inference_numeric_error(condition, stage)) stop(condition)
    list(value = NULL, error = list(
      stage = stage, message = conditionMessage(condition), classes = class(condition)
    ))
  })
}

# Helper function: mark unavailable endpoints without changing their identities
structural_inference_mark <- function(frame, status, reason) {
  frame$lower <- frame$upper <- NA_real_
  frame$lower_status <- frame$upper_status <- status
  frame$lower_reason <- frame$upper_reason <- reason
  frame
}

# Helper function: check whether a log-variance fit is usable, via the paper owner
structural_inference_fit_ok <- function(fit) logvar_fit_ok(fit)

# Helper function: represent a point estimate as equal lower and upper endpoints
structural_inference_point_rows <- function(frame, value, reason = "point available") {
  stopifnot(is.numeric(value), identical(names(value), frame$term), all(is.finite(value)))
  frame$lower <- frame$upper <- unname(value)
  frame$lower_status <- frame$upper_status <- "bounded"
  frame$lower_reason <- frame$upper_reason <- reason
  frame$lower_geometry <- frame$upper_geometry <- "point"
  frame
}

# Helper function: combine attained mean endpoints with independent geometry evidence
structural_inference_mean_rows <- function(frame, box, evidence, fit, box_reason) {
  dimensions <- ncol(fit$w2)
  n_beta <- length(fit$beta1r)
  # frame rows run structural coefficients then news, evidence rows the reverse
  order <- c(dimensions + seq_len(n_beta), seq_len(dimensions))
  stopifnot(nrow(frame) == length(order), nrow(evidence$summary) == length(order))
  for (side in c("lower", "upper")) {
    state <- evidence$summary[[paste0(side, "_state")]][order]
    frame[[paste0(side, "_geometry")]] <- state
    frame[[paste0(side, "_status")]] <- ifelse(state == "unbounded", "unbounded",
      "unreliable"
    )
    frame[[paste0(side, "_reason")]] <- ifelse(state == "unbounded", "verified mean tail",
      ifelse(state == "bounded", box_reason, "mean geometry unresolved")
    )
    frame[[side]][state == "unbounded"] <- if (side == "lower") -Inf else Inf
    if (is.null(box)) next
    values <- c(box$beta1_bounds[[side]], box$bounds[[side]])
    stopifnot(identical(c(box$beta1_bounds$coef, box$bounds$coef), frame$term))
    for (i in seq_len(nrow(frame))) {
      if (state[i] == "unbounded") next
      # keep finite attained values visible even when geometry is unresolved
      if (is.finite(values[i])) frame[[side]][i] <- values[i]
      if (state[i] != "bounded") next
      if (!is.finite(values[i])) {
        stop("A box infinity conflicts with verified bounded mean geometry.", call. = FALSE)
      }
      # the box owns numerical witness acceptance, the evidence checker needs a
      # strict interior margin and cannot be used to reject boundary endpoints
      frame[[paste0(side, "_status")]][i] <- "bounded"
      frame[[paste0(side, "_reason")]][i] <-
        "finite public-box endpoint; mean boundedness verified"
    }
  }
  frame
}
