# Helper function: calibrate point and range statistics and gate their publication
structural_inference_calibrate <- function(full, draws, settings) {
  gates <- structural_inference_failure_gates(draws, settings)
  passed <- stats::setNames(gates$passed, gates$coef)
  # a reported cell is publishable only when its coefficient passes the cap
  gate <- function(summary) {
    ok <- unname(passed[summary$coef])
    summary$publication_allowed <- ok & summary$reason == "reported"
    summary$failure_reason <- ifelse(ok, NA_character_,
      "failed-replication share exceeds application limit"
    )
    summary
  }
  zero <- full$frame$tau == 0
  points <- gate(hetid::bootstrap_point_statistics(
    stats::setNames(full$frame$lower[zero], full$frame$coef[zero]),
    draws$lower[, zero, drop = FALSE], draws$lower_status[, zero, drop = FALSE],
    min_reps = settings$min_reps, stability = settings$stability
  ))
  intervals <- hetid::bootstrap_set_interval(
    full$frame[!zero, ],
    lapply(
      draws[c("lower", "upper", "lower_status", "upper_status")],
      function(x) x[, !zero, drop = FALSE]
    ),
    target = settings$interval_target, alpha = settings$alpha,
    min_reps = settings$min_reps, stability = settings$stability,
    control = settings$interval_control
  )
  intervals$summary <- gate(intervals$summary)
  # hetid keeps the conservative critical value when its calibration search runs
  # out of evaluations or precision, where the paper stops. block the cell
  # instead, so the run fails before publication but io still saves the draws
  summary <- intervals$summary
  stopped <- summary$search_stop %in% c("max_evals", "precision")
  reason <- paste("calibration search stopped at", summary$search_stop[stopped])
  summary$publication_allowed[stopped] <- FALSE
  summary$failure_reason[stopped] <- ifelse(is.na(summary$failure_reason[stopped]), reason,
    paste(summary$failure_reason[stopped], reason, sep = ", ")
  )
  intervals$summary <- summary
  if (any(stopped)) {
    warning("calibration search stopped early for ", paste(summary$coef[stopped],
      collapse = ", "
    ), ", publication is blocked", call. = FALSE)
  }
  list(
    point_summary = points, intervals = intervals, failure_gates = gates,
    publication_ok = all(gates$passed) && !any(stopped)
  )
}

# Helper function: apply the failed-draw cap separately to every coefficient side
structural_inference_failure_gates <- function(draws, settings) {
  stopifnot(length(draws$results) == nrow(draws$lower))
  # a sampled PPML range with any failed candidate fit is unreliable rather than
  # failed, but its failures are numerical and count toward the cap. unresolved
  # mean geometry carries no fit counts and stays outside it
  numerical <- matrix(vapply(draws$results, function(frame) {
    stopifnot(identical(frame$coef, colnames(draws$lower)))
    !is.na(frame$n_failed) & frame$n_failed > 0L
  }, logical(ncol(draws$lower))), nrow = nrow(draws$lower), byrow = TRUE)
  lower <- colMeans(draws$lower_status == "failed" | numerical)
  upper <- colMeans(draws$upper_status == "failed" | numerical)
  data.frame(
    coef = colnames(draws$lower), failed_share_lower = unname(lower),
    failed_share_upper = unname(upper),
    passed_lower = unname(lower <= settings$maximum_failed_share),
    passed_upper = unname(upper <= settings$maximum_failed_share),
    passed = unname(pmax(lower, upper) <= settings$maximum_failed_share),
    stringsAsFactors = FALSE
  )
}
