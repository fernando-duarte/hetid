#' @details
#' Statuses are `bounded`, `unbounded`, `unreliable`, or `failed`. A bounded
#' endpoint must be finite. An unbounded endpoint must be `NA` or the correctly
#' directed infinity; an unreliable endpoint may also be finite. A failed
#' endpoint must be `NA`. `NaN` is rejected. Lower must not exceed upper when
#' both are bounded. These labels describe supplied inference eligibility;
#' they do not certify the optimizer's global bounds. If `point` and
#' `point_status` matrices are included, both endpoints and statuses must be
#' exact mirrors of them, and no point may be unbounded.
#'
#' Each side's scale is `stats::mad()` over its own bounded draws. Failed draws
#' leave the stability denominator; unbounded and unreliable draws remain.
#' Two-sided intervals use the intersection of the two eligible pools, which
#' must also satisfy `min_reps`. Half-infinite intervals use only the live side.
#' Unavailable full-sample sides and failed gates produce missing intervals and
#' explicit reasons. Nonfinite roots in a required interval pool or overflow in
#' interval calibration raise a structured error.
#'
#' Containment uses the quantile of the maximum inward standardized deviation
#' and zero. Pointwise calibration subtracts distance-to-endpoint credits and
#' maximizes this quantile over all positions in the estimated interval.
#' A Lipschitz search returns an attained lower critical value and an upper
#' bound, capped by the containment critical value. `search_stop` distinguishes
#' tolerance, budget, floating-point precision, zero width, and half-infinite
#' cases. Containment skips this search. Zero width gives exact equality of
#' the two formulas, not a limiting assertion as a model parameter varies.
#'
#' The order statistic has rank `min(B, ceiling((B+1)*(1-alpha)))` in the fixed
#' eligible root pool. Capping at `B` does not establish the requested tail
#' probability when `alpha < 1/(B+1)`; counts, rank and tail resolution are
#' reported. The simultaneous diagnostic uses every gated full-sample bounded
#' side and reports their common pool. Its raw critical value is retained even
#' if that pool is below `min_reps`; inspect `active_sides` and `meets_min_reps`.
#' Nonfinite roots in the common pool make the entire simultaneous diagnostic
#' unavailable with a reason, while per-coefficient results remain available.
#' No draw is silently removed from the fixed root pool. It is not a confidence
#' interval for every coefficient.
#'
#' Coverage requires a valid bootstrap approximation for the endpoints and the
#' relevant dependence assumptions. Eligibility gates and algebraic search
#' bounds do not establish coverage under weak identification, nonregular
#' endpoints, or selective draw failure. The caller owns resampling and tuning.
