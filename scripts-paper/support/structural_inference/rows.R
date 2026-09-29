# Helper function: align a summary to the coefficient axis, one row per coefficient
structural_inference_join <- function(summary, coef, what) {
  if (anyDuplicated(summary$coef) || !setequal(summary$coef, coef)) {
    stop(what, " does not cover the coefficient axis exactly once.", call. = FALSE)
  }
  summary[match(coef, summary$coef), , drop = FALSE]
}

# Helper function: join package summaries, failure gates and the reference into display rows
structural_inference_rows <- function(result) {
  frame <- result$bootstrap$full$frame
  if (anyDuplicated(frame$coef)) stop("The coefficient axis repeats a coefficient.", call. = FALSE)
  zero <- frame$tau == 0
  point <- structural_inference_join(
    result$bootstrap$point_summary, frame$coef[zero],
    "The point summary"
  )
  interval <- structural_inference_join(
    result$bootstrap$intervals$summary, frame$coef[!zero],
    "The interval summary"
  )
  gates <- structural_inference_join(
    result$bootstrap$failure_gates, frame$coef,
    "The failure gates"
  )
  # the package summarizes the same full-sample points, so a mismatch is a broken join
  stopifnot(identical(unname(point$point), frame$lower[zero]))
  by_tau <- function(at_zero, positive) {
    out <- rep(at_zero[NA_integer_], nrow(frame))
    out[zero] <- at_zero
    out[!zero] <- positive
    out
  }
  # a proven unbounded side is infinite, the package lets it arrive as NA, and
  # an unreliable or failed side is unavailable whatever value it retains
  endpoint <- function(side, infinity) {
    status <- frame[[paste0(side, "_status")]]
    value <- frame[[side]]
    valid <- ifelse(status == "bounded", is.finite(value),
      status != "unbounded" | is.na(value) | value %in% infinity
    )
    if (!all(valid)) stop("A ", side, " endpoint contradicts its status.", call. = FALSE)
    ifelse(status == "bounded", value, ifelse(status == "unbounded", infinity, NA_real_))
  }
  lower <- endpoint("lower", -Inf)
  upper <- endpoint("upper", Inf)
  # the variance equation loses a quarter the mean equation keeps, so each panel
  # reports its own sample
  n_obs <- c(mean = result$prepared$n_obs, variance = result$prepared$variance_n_obs)
  if (length(n_obs) != 2L || !all(frame$panel %in% names(n_obs))) {
    stop("The prepared input must give the mean and the variance sample sizes.", call. = FALSE)
  }
  published <- by_tau(point$publication_allowed, interval$publication_allowed) %in% TRUE
  gated <- function(x) ifelse(published, x, NA_real_)
  rows <- data.frame(
    panel = frame$panel, term = frame$term,
    column = ifelse(zero, "tau0", sprintf("tau%.2f", frame$tau)), coef = frame$coef,
    # a point estimate stays visible when its bootstrap statistic is withheld
    estimate = ifelse(zero, lower, NA_real_),
    statistic = gated(by_tau(point$statistic, NA_real_)),
    p_value = gated(by_tau(point$p_value_normal, NA_real_)),
    p_value_empirical = gated(by_tau(point$p_value, NA_real_)),
    lower = ifelse(zero, NA_real_, lower), upper = ifelse(zero, NA_real_, upper),
    ci_lower = gated(by_tau(NA_real_, interval$ci_lower)),
    ci_upper = gated(by_tau(NA_real_, interval$ci_upper)),
    n_obs = as.numeric(n_obs[frame$panel]), r_squared = NA_real_,
    approximation = frame$approximation,
    lower_status = frame$lower_status, upper_status = frame$upper_status,
    lower_geometry = frame$lower_geometry, upper_geometry = frame$upper_geometry,
    lower_reason = frame$lower_reason, upper_reason = frame$upper_reason,
    inference_reason = by_tau(point$reason, interval$reason),
    failure_reason = by_tau(point$failure_reason, interval$failure_reason),
    n_eligible_lower = by_tau(point$n_valid_point, interval$n_lower),
    n_eligible_upper = by_tau(point$n_valid_point, interval$n_upper),
    n_common = by_tau(point$n_valid_point, interval$n_common),
    n_non_failed_lower = by_tau(point$n_non_failed, interval$n_non_failed_lower),
    n_non_failed_upper = by_tau(point$n_non_failed, interval$n_non_failed_upper),
    failed_share_lower = gates$failed_share_lower, failed_share_upper = gates$failed_share_upper,
    # full-sample candidate PPML fits behind a sampled variance range
    n_fits_attempted = frame$n_attempted, n_fits_failed = frame$n_failed,
    stringsAsFactors = FALSE
  )
  reasons <- rows[c("lower_reason", "upper_reason", "inference_reason", "failure_reason")]
  rows$reason <- apply(reasons, 1L, function(x) paste(unique(x[!is.na(x)]), collapse = "; "))
  reference <- result$reference$frame
  key <- function(x) paste(x$panel, x$term, sep = "|")
  if (anyDuplicated(key(reference)) || !setequal(key(reference), key(frame))) {
    stop("The reference does not cover the coefficient terms exactly once.", call. = FALSE)
  }
  reference <- reference[match(unique(key(frame)), key(reference)), ]
  # NA rows of the same schema, then the reference's own fields
  ref <- rows[rep(NA_integer_, nrow(reference)), ]
  ref$panel <- reference$panel
  ref$term <- reference$term
  ref$column <- "reference"
  ref$estimate <- reference$estimate
  ref$statistic <- ifelse(reference$available, reference$statistic, NA_real_)
  ref$p_value <- ifelse(reference$available, reference$p_value, NA_real_)
  ref$n_obs <- as.numeric(reference$n_obs)
  ref$r_squared <- reference$r_squared
  ref$approximation <- ifelse(reference$panel == "mean", "OLS with HAC statistics",
    "PPML on squared OLS residuals, HAC statistics"
  )
  ref$inference_reason <- ref$reason <- reference$reason
  out <- rbind(ref, rows)
  rownames(out) <- NULL
  stopifnot(!anyDuplicated(paste(out$panel, out$term, out$column, sep = "|")))
  out
}
