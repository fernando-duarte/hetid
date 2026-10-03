#' Compute Approximation-Error Variance Bounds Across Maturities
#'
#' Composes the envelope and first-order-cancelled news bounds and the expected-
#' SDF bound, retaining both news arms and their pointwise minimum.
#'
#' @template param-yields-term-premia
#' @param step Positive integer number of months per news period, no greater than
#'   half of \code{HETID_CONSTANTS$MAX_MATURITY}. The default quarterly step is
#'   \code{HETID_CONSTANTS$MONTHS_PER_QUARTER}. Adjacent input observations must
#'   be \code{step} months apart.
#' @param maturities \code{NULL} (default), or a nonempty numeric vector of
#'   distinct finite integer maturities in months. Each must be a positive step
#'   multiple no greater than \code{effective_max_maturity(step)}. The default
#'   selects every such multiple; an explicit vector retains its supplied order.
#'
#' @return A list containing \code{per_maturity} and \code{summary}.
#'   The data frame has columns \code{Maturity}, \code{Variance_Bound},
#'   \code{Expected_SDF_Bound}, \code{News_Envelope_Bound}, and
#'   \code{News_Q_Bound}. Summary rows are Mean, Median, Minimum, Maximum,
#'   Standard Deviation, and columns are SDF news and Expected SDF.
#'   A single maturity gives NA for its standard deviation, as in \code{sd()}.
#'
#' @details
#' The default step is quarterly, from \code{HETID_CONSTANTS$MONTHS_PER_QUARTER}.
#' Align yields and term premia by date before selecting numeric columns.
#' Row frequency and term-premium rollover conventions must match the step.
#' Dates are not matched or returned by this cross-maturity summary.
#'
#' Calls preserve the component estimators' individual samples: all envelope
#' bounds are computed first, then all news q bounds, then all expected-SDF
#' bounds. The reported news bound is \code{pmin(envelope, q)}, without removing
#' missing values. The envelope, reported news and expected-SDF bounds must be
#' finite and strictly positive. Positive infinity is allowed only in the news
#' q arm and loses to the finite envelope. Invalid component results raise a
#' structured \code{hetid_error} naming the series and offending maturities;
#' input errors from the scalar estimators propagate unchanged.
#'
#' Scalar zero bounds can be valid for degenerate data, but are rejected by this
#' strictly positive reporting contract. Expected horizon zero is excluded.
#' Summary standard deviations use \code{stats::sd()}, with divisor N - 1.
#' These plug-in approximation-error bounds are not certified finite-sample bounds
#' on population variance. The expected-SDF bound corresponds to the paired
#' estimator, not to \code{compute_expected_sdf()}'s unpaired default.
#'
#' @seealso \code{\link{compute_variance_bound}}, \code{\link{compute_news_q_bound}},
#'   \code{\link{compute_expected_sdf_variance_bound}}
#' @export
#' @examples
#' acm <- extract_acm_data(maturities = c(1, 2, 3), auto_download = FALSE)
#' compute_variance_bounds(acm[c("y1", "y2", "y3")],
#'   acm[c("tp1", "tp2", "tp3")],
#'   step = 1,
#'   maturities = c(1, 2)
#' )
compute_variance_bounds <- function(yields, term_premia,
                                    step = HETID_CONSTANTS$MONTHS_PER_QUARTER,
                                    maturities = NULL) {
  validate_step(step)
  if (is.null(maturities)) {
    maturities <- seq.int(step, effective_max_maturity(step), by = step)
  }
  validate_maturities(
    maturities, effective_max_maturity(step),
    min_value = step
  )
  assert_bad_argument_ok(
    all(maturities %% step == 0L),
    "maturities must be positive multiples of step", "maturities"
  )
  validate_row_alignment(yields, term_premia)
  bound_series <- function(fn) {
    vapply(maturities, function(i) fn(yields, term_premia, i = i, step = step), numeric(1))
  }
  news_envelope <- bound_series(compute_variance_bound)
  news_q <- bound_series(compute_news_q_bound)
  expected_sdf <- bound_series(compute_expected_sdf_variance_bound)
  news_bound <- pmin(news_envelope, news_q)
  valid <- list(
    News_Envelope_Bound = is.finite(news_envelope) & news_envelope > 0,
    News_Q_Bound = !is.na(news_q) & news_q > 0,
    Variance_Bound = is.finite(news_bound) & news_bound > 0,
    Expected_SDF_Bound = is.finite(expected_sdf) & expected_sdf > 0
  )
  for (series in names(valid)) {
    if (!all(valid[[series]])) {
      stop_hetid(paste0(
        "Invalid ", series, " at maturities: ",
        paste(maturities[!valid[[series]]], collapse = ", ")
      ))
    }
  }
  per_maturity <- data.frame(
    Maturity = maturities, Variance_Bound = news_bound, Expected_SDF_Bound = expected_sdf,
    News_Envelope_Bound = news_envelope, News_Q_Bound = news_q
  )
  statistics <- function(x) {
    c(
      Mean = mean(x), Median = stats::median(x), Minimum = min(x),
      Maximum = max(x), "Standard Deviation" = stats::sd(x)
    )
  }
  bound_summary <- cbind(
    "SDF news" = statistics(news_bound), "Expected SDF" = statistics(expected_sdf)
  )
  list(per_maturity = per_maturity, summary = bound_summary)
}
