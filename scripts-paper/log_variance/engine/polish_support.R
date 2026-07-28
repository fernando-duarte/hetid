# Polish-stage support for the estimator-generic set engine: the per-coefficient
# objective/gradient closures the endpoint stage optimizes, and the news-box
# escape measure that detects a polish and a scan searching different regions.
# Definitions only; sourced by api.R.

# fn/gr for coefficient j: the estimator's own closures when it supplies them,
# else engine-built ones over the cached evaluator (plus the jacobian when the
# estimator has one). A budget stop raised inside an optimizer callback is
# recorded on budget_hit and surfaced as NaN so nloptr unwinds normally and the
# caller re-raises it.
logvar_coef_objective_fns <- function(est, j, evaluate_fit, budget_hit) {
  # the caller advances j in a loop and the closures below only read it from
  # their body, so an unforced promise would resolve against whatever j held
  # when the optimizer first called them
  force(j)
  co <- if (is.null(est$coef_objective)) NULL else est$coef_objective(j)
  if (!is.null(co)) {
    return(list(fn = co$fn, gr = co$gr))
  }
  polish <- LOGVAR_ENGINE_PHASES[["polish"]]
  fn <- function(b) {
    tryCatch(
      {
        fit <- evaluate_fit(b, phase = polish)
        if (!logvar_fit_ok(fit)) NaN else unname(fit$coef[[j]])
      },
      logvar_budget_exhausted = function(e) {
        budget_hit$cond <- e
        NaN
      }
    )
  }
  gr <- if (is.null(est$jacobian_at_b)) {
    NULL
  } else {
    function(b) {
      tryCatch(
        {
          fit <- evaluate_fit(b, phase = polish)
          if (!logvar_fit_ok(fit)) {
            rep(NaN, length(b))
          } else {
            est$jacobian_at_b(b, fit)[j, ]
          }
        },
        logvar_budget_exhausted = function(e) {
          budget_hit$cond <- e
          rep(NaN, length(b))
        }
      )
    }
  }
  list(fn = fn, gr = gr)
}

# How far outside the news box an attaining point sits, relative to the box's
# own span (<= 0 when inside). The scan only ever sees b_tab's box while the
# polish is bounded by the quadratic constraints alone, so a positive value
# means the two halves of the search covered different regions and the box is
# not the outer screen it is contracted to be.
logvar_box_escape <- function(arg, b_tab) {
  lo <- b_tab$set_lower
  hi <- b_tab$set_upper
  if (is.null(arg) || anyNA(arg) || anyNA(lo) || anyNA(hi) ||
    !all(is.finite(c(lo, hi)))) {
    return(NA_real_)
  }
  scale <- pmax(hi - lo, abs(lo), abs(hi), 1)
  max(pmax(lo - arg, arg - hi) / scale)
}
