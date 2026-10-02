# Notes for the regularized log-projection pages and their tuning appendix
# table: the transformed response and what its coefficients mean, the tuning
# values, the small-residual shares, the Fuller scale certification, the search
# and its caveats, and the absence of inference for these panels. One string
# per item. Definitions only; sourced by estimator_page_specs.R.

.lp_num <- function(x) {
  paper_format_general(x, PAPER_REPORTING_CONTROL$precision$diagnostic_table)
}

.lp_response_note <- function(method) {
  if (identical(method, "log_plus")) {
    return(paste(
      "The panel projects $\\log(\\varepsilon^2 + h_T^2)$ on $(1, PC_R)$ at each",
      "news vector $b_N$, with $h_T = m \\hat s / \\sqrt{T}$ and $\\hat s^2$ the",
      "mean-sample mean square of the outcome's auxiliary residual; the",
      "coefficients describe a projection of the transformed squared residual,",
      "not a conditional variance."
    ))
  }
  paste(
    "The panel projects the two-pass Fuller transform",
    "$F(x, \\delta) = \\log(x + \\delta) - \\delta / (x + \\delta)$ of",
    "$x = \\varepsilon^2$ on $(1, PC_R)$ at each news vector $b_N$, with",
    "$c_T = m^2 / T$, the candidate's own mean-sample scale, and a second-pass",
    "adjustment profile from the first-pass slopes; the coefficients describe a",
    "projection of the transformed squared residual, not a conditional variance."
  )
}

build_log_projection_panel_notes <- function(result, tau_baseline) {
  method <- result$estimator$metadata$fit_control$method
  ep <- result$endpoints
  point <- ep[ep$role == "point", , drop = FALSE]
  base <- ep[ep$role == "endpoint" & !is.na(ep$tau) &
    abs(ep$tau - tau_baseline) < 1e-12, , drop = FALSE]
  share <- base$share_small[is.finite(base$share_small)]
  tuning <- if (identical(method, "log_plus")) {
    sprintf("$h_T = %s$", .lp_num(point$h_T[1L]))
  } else {
    sprintf("$c_T = %s$", .lp_num(point$c_T[1L]))
  }
  notes <- c(
    .lp_response_note(method),
    paste(
      "The OLS column applies this projection to the residuals of the",
      "unrestricted mean-equation OLS fit."
    ),
    sprintf(
      "Tuning: $m = %s$, so %s ($T_M = %d$, $T = %d$, $\\hat s = %s$).",
      .lp_num(result$multiplier), tuning, result$scale$n_mean,
      result$scale$n_vol, .lp_num(result$scale$s_hat)
    ),
    sprintf(
      paste(
        "The share of volatility-sample residuals below the threshold is %s at",
        "the $\\tau{=}0$ point and ranges over %s--%s across the endpoint",
        "candidates at $\\tau{=}%s$."
      ),
      .lp_num(point$share_small[1L]),
      if (length(share)) .lp_num(min(share)) else PAPER_NA_TOKEN,
      if (length(share)) .lp_num(max(share)) else PAPER_NA_TOKEN,
      paper_format_tau(tau_baseline)
    )
  )
  if (identical(method, "log_fuller")) {
    notes <- c(notes, if (isTRUE(result$scale$scale_lower_certified)) {
      sprintf(
        paste(
          "The candidate scale's unrestricted lower bound is certified positive;",
          "the second-pass adjustment profile spans %s to %s in logs relative to",
          "its first-pass level at the $\\tau{=}0$ point."
        ),
        .lp_num(point$profile_log_ratio_min[1L]),
        .lp_num(point$profile_log_ratio_max[1L])
      )
    } else {
      paste(
        "The candidate scale's positivity over the set is uncertified, so every",
        "endpoint is reported unreliable."
      )
    })
  }
  c(
    notes,
    paste(
      "Endpoints come from a full-lattice scan with an SLSQP polish, audited by",
      "an independent five-start search on the same lattice; a side the audit",
      "moves or contradicts is reported unreliable, as is a side whose bound",
      "loosens as $\\tau$ grows. The two sides of a bracket can come from",
      "different candidates, and the ranges are numerical approximations",
      "without global guarantees."
    ),
    paste(
      "Exponentiated sweep panels are not variance ratios for this projection.",
      "No inference is reported for Panel B; the tuning sensitivity is in the",
      "appendix table."
    )
  )
}

build_log_projection_tuning_notes <- function(tuning, tau_baseline, endpoints) {
  point <- endpoints[endpoints$role == "point", , drop = FALSE]
  point <- point[order(match(point$id, unique(tuning$id)), point$multiplier), ,
    drop = FALSE
  ]
  values <- vapply(seq_len(nrow(point)), function(i) {
    threshold <- if (identical(point$id[[i]], "log_plus")) {
      sprintf("$h_T = %s$", .lp_num(point$h_T[[i]]))
    } else {
      sprintf("$c_T = %s$", .lp_num(point$c_T[[i]]))
    }
    sprintf(
      "%s at $m = %s$: %s, share below %s",
      if (identical(point$id[[i]], "log_plus")) "additive" else "Fuller",
      .lp_num(point$multiplier[[i]]), threshold, .lp_num(point$share_small[[i]])
    )
  }, character(1))
  c(
    sprintf(
      paste(
        "Rows give the additive-threshold ($\\theta^{L}$) and two-pass Fuller",
        "($\\theta^{F}$) projections; columns pair the $\\tau{=}0$ point with the",
        "identified set at $\\tau{=}%s$ for each multiplier $m$, on identical",
        "candidate sets and samples."
      ),
      paper_format_tau(tau_baseline)
    ),
    paste(
      "The thresholds scale as $h_T = m \\hat s / \\sqrt{T}$ and $c_T = m^2 / T$,",
      "so doubling $m$ quadruples $h_T^2$ and $c_T$. At the $\\tau{=}0$ point:",
      paste0(paste(values, collapse = "; "), ".")
    ),
    paste(
      "A dash marks an unavailable point; unreliable marks a side the audit or",
      "the nesting check could not certify. No inference is reported for these",
      "estimates."
    ),
    paste(
      "Endpoint diagnostics ($\\hat s$, $h_T$, $s_b$, $c_T$, small-residual",
      "shares, Fuller profile ranges, audit discrepancies) are in",
      "\\texttt{log\\_var\\_eq\\_log\\_projection\\_endpoints.csv}."
    )
  )
}
