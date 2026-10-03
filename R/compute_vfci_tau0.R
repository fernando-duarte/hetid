#' Compute the Tau-Zero Volatility Financial Conditions Index
#'
#' Composes the tau-zero mean-equation point with a PPML log-variance fit
#' on prepared, dated inputs. The index is half the centered volatility
#' regressor contribution to log variance, excluding the intercept.
#'
#' @param data A data frame of observations already aligned by calendar date,
#'   with a finite, dimensionless \code{Date} column named \code{date}.
#'   Column names must be unique, nonempty, and nonmissing. Selected variable
#'   columns must be numeric vectors. Quarter-start dates are relabeled to
#'   quarter-end without changing observations. Normalized quarters must be
#'   strictly increasing and unique; calendar gaps are allowed.
#' @param y Single column name for the mean-equation outcome.
#' @param x Nonempty character vector naming the common conditioning regressors,
#'   without an intercept. See \code{\link{compute_tau0_system}} for its design
#'   contract. Application PCs are of nominal financial asset returns.
#' @param y2 Nonempty character vector naming the news/innovation columns.
#' @param z Single column name for the heteroskedasticity instrument.
#' @param het Nonempty character vector naming the volatility regressors, without
#'   an intercept. These may differ from \code{x}. Each vector of selectors must
#'   have unique, nonblank, nonmissing names; overlap between roles is allowed.
#' @param date_begin,date_end Finite scalar \code{Date} bounds of the inclusive
#'   forecast-origin window. Bounds are normalized to quarter-end. No lead or
#'   lag is applied to the supplied observations.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{ts}{A plain data frame with \code{date}, \code{vfci}, \code{mu},
#'     and \code{epsilon}, in mean-sample order. Dates are calendar quarter-ends.
#'     \code{epsilon = w1 - w2 \%*\% theta} and \code{mu = y - epsilon}.
#'     \code{vfci} is missing on mean rows omitted from the volatility fit.}
#'   \item{tau0, logvar}{The unchanged, validated \code{hetid_tau0_fit} and
#'     \code{hetid_log_variance_fit} containers from the two estimators.}
#'   \item{masks}{Logical vectors \code{window}, \code{mean}, and
#'     \code{variance}, each of \code{nrow(data)} elements, indexed to the
#'     original input rows. Variance rows are a subset of mean rows.}
#'   \item{call}{The matched call.}
#' }
#' Invalid inputs and insufficient samples signal structured \code{hetid_error}
#' conditions. No unique tau-zero point or an unsuccessful PPML fit signals a
#' \code{hetid_error}; underlying estimator errors and warnings are preserved.
#'
#' @details
#' The mean sample is the window complete on \code{y}, \code{x}, \code{y2},
#' and \code{z}. The volatility sample further requires complete \code{het}.
#' Missing values, including \code{NaN}, omit a row only from the equation
#' reading that value. Infinite values on otherwise selected fit rows are errors.
#' Each sample needs at least its number of regressors plus two observations.
#' Neither sample must be contiguous, and no observations are filled in.
#'
#' Volatility regressors are centered over their own fitted sample, without
#' scaling. On those rows the index equals half their slope projection, so its
#' sample mean is zero up to roundoff. It is not half the total fitted log variance.
#' The mean fit uses the defaults of \code{\link{compute_tau0_system}}; the
#' volatility fit uses the unchanged PPML defaults of
#' \code{\link{fit_log_variance_at_b}}. This function supplies no inference for
#' the generated residuals or index.
#'
#' @seealso \code{\link{compute_tau0_system}},
#'   \code{\link{fit_log_variance_at_b}}, \code{\link{to_period_end}}
#' @export
#'
#' @examples
#' (function() {
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
#'       rm(".Random.seed", envir = .GlobalEnv)
#'     }
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   n <- 150
#'   z <- rnorm(n)
#'   x <- rnorm(n)
#'   innovation <- x + exp(z / 2) * rnorm(n)
#'   data <- data.frame(
#'     date = seq(as.Date("1980-01-01"), by = "3 months", length.out = n),
#'     y = 0.3 + x + 0.5 * innovation + rnorm(n), x = x, news = innovation, z = z
#'   )
#'   out <- compute_vfci_tau0(
#'     data, "y", "x", "news", "z", "x",
#'     min(data$date), max(data$date)
#'   )
#'   head(out$ts)
#' })()
compute_vfci_tau0 <- function(data, y, x, y2, z, het, date_begin, date_end) {
  matched_call <- match.call()
  prepared <- vfci_tau0_inputs(data, y, x, y2, z, het, date_begin, date_end)
  masks <- prepared$masks
  mean_data <- data[masks$mean, , drop = FALSE]
  rownames(mean_data) <- NULL
  fit <- compute_tau0_system(
    mean_data[[y]], as.matrix(mean_data[y2]), as.matrix(mean_data[x]), mean_data[[z]]
  )
  if (is.null(fit$point)) {
    stop_hetid("The stacked tau-zero system has no unique consistent solution")
  }
  theta <- stats::setNames(fit$point$theta, colnames(fit$w2))
  epsilon <- drop(fit$w1 - fit$w2 %*% theta)
  rows <- masks$variance[masks$mean]
  x_het <- scale(as.matrix(mean_data[het])[rows, , drop = FALSE], center = TRUE, scale = FALSE)
  logvar <- fit_log_variance_at_b(
    theta, fit$w1[rows], fit$w2[rows, , drop = FALSE], x_het
  )
  if (!log_variance_fit_ok(logvar)) {
    stop_hetid("The tau-zero VFCI PPML log-variance fit did not converge")
  }
  assert_bad_argument_ok(
    identical(names(logvar$coef), c(LOG_VARIANCE_INTERCEPT_LABEL, colnames(x_het))),
    "PPML coefficient labels must match the volatility design in order",
    arg = "het"
  )
  vfci <- rep(NA_real_, nrow(mean_data))
  vfci[rows] <- drop(x_het %*% logvar$coef[-1L]) / 2
  list(
    ts = data.frame(
      date = prepared$dates[masks$mean], vfci = vfci,
      mu = mean_data[[y]] - epsilon, epsilon = epsilon
    ),
    tau0 = fit, logvar = logvar, masks = masks, call = matched_call
  )
}
