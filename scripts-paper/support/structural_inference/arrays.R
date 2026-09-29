# Helper function: resample whole mean rows, keeping each row's variance availability
structural_inference_arrays <- function(prepared, index) {
  stopifnot(
    is.numeric(index), length(index) == prepared$n_obs,
    all(is.finite(index)), all(index == floor(index)),
    all(index >= 1L & index <= prepared$n_obs)
  )
  # the mean equation reads every drawn row, the variance equation the drawn
  # rows whose source quarter has PC_R. variance_rows marks them in the draw,
  # variance_source gives their prepared rows
  variance_rows <- prepared$variance[index]
  # variance predictors centred within the draw, as the baseline centres them
  # over its sample, so the intercept is the log variance at the draw's mean
  list(
    y = prepared$y[index], x = prepared$x[index, , drop = FALSE],
    y2 = prepared$y2[index, , drop = FALSE], z = prepared$z[index, , drop = FALSE],
    x_var = scale(prepared$x_var[index[variance_rows], , drop = FALSE],
      center = TRUE, scale = FALSE
    ),
    variance_rows = variance_rows, variance_source = index[variance_rows]
  )
}

# Helper function: declare stable panel, coefficient and tau identities
structural_inference_axis <- function(prepared, settings) {
  terms <- list(
    mean = c("(Intercept)", colnames(prepared$x), colnames(prepared$y2)),
    variance = c("(Intercept)", colnames(prepared$x_var))
  )
  rows <- lapply(names(terms), function(panel) {
    do.call(rbind, lapply(c(0, settings$taus), function(tau) {
      term <- terms[[panel]]
      data.frame(
        coef = paste(panel, term, sprintf("tau=%.2f", tau), sep = "|"),
        panel = panel, term = term, tau = tau,
        lower = NA_real_, upper = NA_real_,
        lower_status = "failed", upper_status = "failed",
        lower_reason = "not evaluated", upper_reason = "not evaluated",
        lower_geometry = "not evaluated", upper_geometry = "not evaluated",
        approximation = if (tau == 0) {
          "point"
        } else if (panel == "mean") {
          "attained mean range"
        } else {
          "sampled PPML coefficient range"
        },
        n_attempted = NA_integer_, n_failed = NA_integer_,
        stringsAsFactors = FALSE
      )
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  stopifnot(!anyDuplicated(out$coef))
  out
}
