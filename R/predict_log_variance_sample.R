#' Predict an Envelope from Jointly Fitted Sampled Coefficients
#'
#' Projects every successful joint coefficient vector retained by
#' \code{\link{sample_log_variance_set}} onto the requested design rows. Each
#' row's attained range keeps its mean-parameter witnesses and candidate IDs.
#' Marginal coefficient endpoints are never combined into artificial corners.
#'
#' @param object A \code{hetid_log_variance_sample} returned by
#'   \code{\link{sample_log_variance_set}}.
#' @param newdata Numeric matrix or data frame of regressors without an intercept,
#'   or \code{NULL} (the default) to use the training rows. Values must be finite;
#'   missing values are rejected. Column count must match the training regressors;
#'   supplied names must match in order. Unnamed columns are positional.
#' @param type Single character string selecting the prediction scale:
#'   \code{"log_variance"} (eta, the default), \code{"variance"} (exp(eta)), or
#'   \code{"volatility"} (exp(eta/2)).
#' @param include_intercept Single non-missing logical value. \code{TRUE} (the
#'   default) includes the fitted intercept. \code{FALSE} explicitly zeroes its
#'   design column, preserving the coefficient axis.
#' @param dates Optional unique, non-missing \code{Date} labels for the prediction
#'   rows, one per row. Training dates are reused only when \code{newdata} is
#'   \code{NULL}. Dates and row order are preserved; align separate time series by
#'   date before supplying inputs. Supply period-end labels for a time series.
#' @param ... Unused; additional arguments raise a structured error.
#' @details
#' These are pointwise ranges over successfully fitted sampled mean parameters.
#' They are neither full-set bounds, confidence bands nor prediction intervals for
#' future observations. Different rows may attain their endpoints at different
#' candidates; their endpoints need not form a jointly attainable path.
#'
#' Status \code{sampled_partial} records omission of nonconverged fits; \code{sampled}
#' still carries no full-set coverage guarantee. An infinite box, absent candidates,
#' or all failed fits keeps the sampling reason and returns missing predictions.
#' Nonfinite projection arithmetic makes that row unavailable, with status
#' \code{nonfinite_projection} on both sides. Exponential overflow or underflow at
#' finite eta makes only that transformed side missing, with status
#' \code{transform_overflow} or \code{transform_underflow}; it never proves infinity
#' or an attained zero. No prediction step refits a candidate or draws randomness.
#' Tied endpoints use the first successful candidate in the original sample order.
#' Invalid objects or prediction inputs raise structured \code{hetid_error}
#' conditions; rows are not dropped to handle missing values.
#' @return A list with \code{bounds}, a data frame with one row per prediction row
#'   (row index, requested-scale lower/upper,
#'   eta_lower/eta_upper and per-side statuses; date first when supplied),
#'   \code{arg_lower}/\code{arg_upper}, numeric matrices with one row per prediction
#'   row and the mean-parameter columns of \code{object$candidates} (parameters
#'   attaining the finite eta endpoints), and
#'   \code{candidate_lower}/\code{candidate_upper}, integer vectors indexing the
#'   original sample rows, including failed rows in that numbering. Unavailable
#'   endpoints, witnesses and candidate indices are \code{NA}. Eta witnesses
#'   remain available when only the requested-scale transformation fails. The
#'   design, dates, type, intercept choice and original sample are retained.
#' @seealso \code{\link[stats:predict]{stats::predict}}
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   n <- 100
#'   z <- rnorm(n)
#'   x <- cbind(x = rnorm(n))
#'   y2 <- cbind(news = exp(z / 2) * rnorm(n))
#'   y1 <- 0.3 + x[, 1] + 0.5 * y2[, 1] + rnorm(n)
#'   fit <- compute_tau0_system(y1, y2, x, z)
#'   box <- compute_identified_set_box(fit, 0.01, n_grid = 3)
#'   sampled <- sample_log_variance_set(box, x)
#'   predict(sampled, x[1:3, , drop = FALSE])$bounds
#'   dates <- as.Date(c("2024-03-31", "2024-06-30", "2024-09-30"))
#'   predict(sampled, x[1:3, , drop = FALSE],
#'     type = "volatility",
#'     include_intercept = FALSE, dates = dates
#'   )$bounds
#' })
#' @importFrom stats predict
#' @export
predict.hetid_log_variance_sample <- function(object, newdata = NULL,
                                              type = "log_variance",
                                              include_intercept = TRUE,
                                              dates = NULL, ...) {
  validate_hetid_log_variance_sample(object)
  assert_bad_argument_ok(
    length(list(...)) == 0L, "additional prediction arguments are unsupported",
    arg = "..."
  )
  assert_bad_argument_ok(
    is.character(type) && length(type) == 1L &&
      !is.na(type) && type %in% c("log_variance", "variance", "volatility"),
    "type must be log_variance, variance or volatility",
    arg = "type"
  )
  assert_flag(include_intercept, "include_intercept")
  input <- log_variance_prediction_design(object, newdata, include_intercept, dates)
  design <- input$design
  out <- empty_log_variance_envelope(object, input, type, include_intercept)
  ok <- vapply(object$fits, function(fit) identical(fit$fit_status, "ok"), logical(1))
  if (!any(ok) || !nrow(design)) {
    return(out)
  }
  ids <- which(ok)
  eta <- tcrossprod(design, object$coefficients[ok, , drop = FALSE])
  valid <- rowSums(!is.finite(eta)) == 0L
  eta[!valid, ] <- 0
  lower <- max.col(-eta, ties.method = "first")
  upper <- max.col(eta, ties.method = "first")
  out$bounds$eta_lower[valid] <- eta[cbind(which(valid), lower[valid])]
  out$bounds$eta_upper[valid] <- eta[cbind(which(valid), upper[valid])]
  out$candidate_lower[valid] <- ids[lower[valid]]
  out$candidate_upper[valid] <- ids[upper[valid]]
  status <- if (any(!ok)) "sampled_partial" else "sampled"
  for (side in c("lower", "upper")) {
    index <- out[[paste0("candidate_", side)]]
    out[[paste0("arg_", side)]][valid, ] <- object$candidates[index[valid], , drop = FALSE]
    transformed <- transform_sampled_log_variance(out$bounds[[paste0("eta_", side)]], type)
    out$bounds[[side]] <- transformed$value
    out$bounds[[paste0(side, "_status")]] <- ifelse(valid, status, "nonfinite_projection")
    bad <- valid & !is.na(transformed$failure)
    out$bounds[[paste0(side, "_status")]][bad] <- transformed$failure[bad]
  }
  out
}
