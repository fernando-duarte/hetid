#' Build the Start Ladder
#'
#' The default order is the supplied start, each fallback start, the
#' intercept-only start, then the \code{glm.fit} default (\code{NULL}).
#' The validated \code{START_ORDER} control reorders these groups.
#' The intercept-only start is omitted when the scaled response has zero mean.
#'
#' @param start Numeric start vector of length \code{p}, or \code{NULL}.
#' @param fallback_starts List of numeric start vectors of length \code{p}.
#' @param y_scaled Finite nonnegative response vector on the scaled fit.
#' @param p Positive integer number of design columns, including the intercept.
#' @param control Validated PPML fitting controls, including \code{START_ORDER}.
#'
#' @return A list with \code{candidates}, a list of start vectors or
#'   \code{NULL}, and \code{labels}, a character vector of matching group
#'   names in the same order.
#' @noRd
ppml_start_ladder <- function(
  start, fallback_starts, y_scaled, p, control = log_variance_fit_control("ppml")
) {
  intercept_start <- if (mean(y_scaled) > 0) {
    list(c(log(mean(y_scaled)), rep(0, p - 1L)))
  } else {
    list()
  }
  groups <- list(
    supplied = if (is.null(start)) list() else list(start),
    fallback = fallback_starts,
    intercept_only = intercept_start,
    glm_default = list(NULL)
  )
  groups <- groups[control$START_ORDER]
  list(
    candidates = unlist(groups, recursive = FALSE),
    labels = rep(names(groups), lengths(groups))
  )
}

#' Screen a Candidate Start
#'
#' @param cand Numeric candidate vector of length \code{ncol(x_mat)}, or
#'   \code{NULL} for the \code{glm.fit} default.
#' @param x_mat Finite numeric design matrix, intercept column included.
#'
#' @return A logical scalar: \code{TRUE} when the candidate coefficients or
#'   their exponentiated linear predictor are nonfinite, and \code{FALSE}
#'   otherwise, including for \code{NULL}. Underflow to zero is not rejected.
#' @noRd
ppml_start_invalid <- function(cand, x_mat) {
  !is.null(cand) &&
    !(all(is.finite(cand)) && all(is.finite(exp(drop(x_mat %*% cand)))))
}

#' Fit the PPML Log-Variance Response
#'
#' Fits \code{y / response_scale} by quasi-Poisson IRLS with a log link and
#' returns the first accepted start-ladder fit, with coefficients recovered
#' on the original response scale.
#'
#' @param y Finite nonnegative numeric vector of length \code{nrow(x_mat)}
#'   on the original response scale. Missing values are not allowed.
#' @param x_mat Finite numeric design matrix from
#'   \code{\link{log_variance_design}}, with the intercept first and unique,
#'   non-missing, non-blank column labels. Rows must align with \code{y}.
#' @param start Numeric vector of length \code{ncol(x_mat)} on the scaled
#'   response, or \code{NULL} (the default). Names, if present, must match
#'   \code{colnames(x_mat)} in order. Nonfinite starts may reach this helper
#'   when the boundary permits them and are recorded as invalid attempts.
#' @param fallback_starts List of start vectors following the same scale,
#'   length, and naming rules as \code{start}. Defaults to an empty list.
#' @param response_scale Positive finite numeric scalar dividing \code{y}
#'   before fitting. Defaults to \code{1}.
#' @param control Complete validated PPML fitting-control list. Defaults to
#'   the PPML controls resolved by \code{log_variance_fit_control("ppml")}.
#' @param design Fixed-design quantities matching \code{x_mat} and
#'   \code{control}; computed from them by default.
#'
#' @return A validated \code{hetid_log_variance_fit} list, visibly, retaining
#'   the original response and design. On success, \code{fit_status = "ok"}
#'   and \code{coef} is named by the design columns; \code{warm_start} stays
#'   on the scaled response. On failure, \code{fit_status = "nonconvergence"},
#'   \code{coef} and \code{warm_start} are \code{NULL}, and the reason is in
#'   \code{diagnostics$error_class}. See \code{\link{hetid_log_variance_fit}}.
#'
#' @details
#' Arguments must already satisfy the validation contract of
#' \code{\link{fit_log_variance}} or \code{\link{make_log_variance_fitter}};
#' this internal solver does not replace that boundary validation.
#' An all-zero response, loss of full column rank among positive-response
#' rows, response-scaling underflow or overflow, or a ladder with no accepted
#' rung returns a failure object for validated inputs.
#'
#' By default, starts are tried in this order: the supplied start, each
#' fallback, an intercept-only start, and the \code{\link[stats]{glm.fit}}
#' default. The validated \code{START_ORDER} control reorders these groups.
#' Acceptance uses the convergence, score, and conditioning gates of
#' \code{\link{ppml_accept}}. The intercept in \code{coef} adds
#' \code{log(response_scale)} to the scaled-fit intercept; slopes are unchanged.
#' The objective and score diagnostics refer to the scaled response.
#'
#' Warnings and messages from the final attempted fit, or the accepted fit,
#' are captured in \code{diagnostics$warnings} and \code{diagnostics$messages}.
#' Earlier attempts retain their source and failure class in
#' \code{diagnostics$start_attempts}, but not their warning or message text.
#' @keywords internal
ppml_fit_response <- function(y, x_mat, start = NULL, fallback_starts = list(),
                              response_scale = 1, control = log_variance_fit_control("ppml"),
                              design = log_variance_fixed_design(x_mat, "ppml", control)) {
  y_scaled <- y / response_scale
  scale_failure <- log_variance_scaled_response_class(y, y_scaled)
  if (!is.na(scale_failure)) {
    return(ppml_failure(scale_failure, y, x_mat, response_scale))
  }
  rank_x_pos <- if (all(y_scaled > 0)) {
    design$rank
  } else {
    ppml_pos_rank(y_scaled, x_mat, control)
  }
  if (rank_x_pos != ncol(x_mat)) {
    return(ppml_failure(
      "rank_unresolved", y, x_mat, response_scale,
      rank_x_pos = rank_x_pos,
      min_pos_response = min(y_scaled[y_scaled > 0])
    ))
  }
  ladder <- ppml_start_ladder(start, fallback_starts, y_scaled, ncol(x_mat), control)
  attempts <- list()
  last <- list(
    warnings = character(0), messages = character(0),
    error_class = "no_accepted_start"
  )
  for (i in seq_along(ladder$candidates)) {
    cand <- ladder$candidates[[i]]
    if (ppml_start_invalid(cand, x_mat)) {
      attempts <- c(attempts, list(list(
        source = ladder$labels[i], error_class = "invalid_start"
      )))
      last$error_class <- "invalid_start"
      next
    }
    run <- ppml_run_glm(cand, y_scaled, x_mat, control)
    last$warnings <- run$warnings
    last$messages <- run$messages
    if (is.null(run$fit)) {
      attempts <- c(attempts, list(list(
        source = ladder$labels[i], error_class = "fit_error"
      )))
      last$error_class <- "fit_error"
      next
    }
    acc <- ppml_accept(run$fit, y_scaled, x_mat, control)
    attempts <- c(attempts, list(list(
      source = ladder$labels[i],
      error_class = if (acc$accepted) NA_character_ else acc$reason
    )))
    if (acc$accepted) {
      return(ppml_success(
        acc, run, y, y_scaled, x_mat, response_scale, attempts, rank_x_pos
      ))
    }
    last$error_class <- acc$reason
  }
  ppml_failure(
    last$error_class, y, x_mat, response_scale, attempts,
    warnings = last$warnings, messages = last$messages,
    rank_x_pos = rank_x_pos, min_pos_response = min(y_scaled[y_scaled > 0])
  )
}
