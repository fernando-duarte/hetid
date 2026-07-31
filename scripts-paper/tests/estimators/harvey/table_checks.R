# Rendering checks for the Harvey page of the per-estimator document. The
# fixture keeps the panel path offline while pinning the coefficient mapping,
# exact display-tau lookup, standard-error selection, and estimator notes.
# The panel is assembled by the shared logvar_estimator_panel_parts, so these
# exercise the same code the published page runs.

hvt_coef <- c("(Intercept)", "l.pc1")
hvt_set <- function(lo, hi) {
  data.frame(
    coef = hvt_coef, set_lower = lo, set_upper = hi,
    status = "bounded", stringsAsFactors = FALSE
  )
}
hvt_harvey <- list(
  table = data.frame(
    coef = hvt_coef, reference = c(-1.3, 0.2), point = c(-1.2, 0.18),
    stringsAsFactors = FALSE
  ),
  sets = stats::setNames(
    list(
      hvt_set(c(-1.25, 0.17), c(-1.15, 0.19)),
      hvt_set(c(-1.3, 0.16), c(-1.1, 0.2))
    ),
    vapply(c(0.05, 0.1), paper_tau_key, character(1))
  )
)
hvt_parts <- function(result, se_type = NULL) {
  logvar_estimator_panel_parts(
    result, 12L, c(0.05, 0.1), LOGVAR_HARVEY_PANEL_SPEC,
    se_type, LOGVAR_HARVEY_SE_TYPES, NULL,
    PAPER_REPORTING_CONTROL$cells$log_variance, NULL
  )
}
hvt_row <- function(parts, i) {
  paste(vapply(parts$columns, function(col) col[[i]], character(1)), collapse = " & ")
}
hvt_plain <- hvt_parts(hvt_harvey)

check("Harvey panel uses reference, point, and exact tau hulls", {
  identical(hvt_plain$rows[[1L]], "$\\theta^{H}_0$") &&
    identical(hvt_row(hvt_plain, 1L), paste0(
      "-1.300 & -1.200 & $[-1.250,\\,-1.150]$ & $[-1.300,\\,-1.100]$"
    )) &&
    identical(hvt_plain$rows[[3L]], "$\\theta^{H}_{1,R}$") &&
    identical(hvt_row(hvt_plain, 3L), paste0(
      "0.200 & 0.180 & $[0.170,\\,0.190]$ & $[0.160,\\,0.200]$"
    )) &&
    identical(hvt_row(hvt_plain, 6L), "12 & 12 & 12 & 12")
})
check("Harvey panel headers name the reference column and the display taus", {
  identical(
    hvt_plain$headers,
    c("Reference", "$\\tau{=}0$", "$\\tau{=}0.05$", "$\\tau{=}0.1$")
  )
})
check("Harvey notes carry the MLE and zero-safe disclosures", {
  notes <- paste(
    build_harvey_panel_notes(hvt_harvey, 0.05, 4000L, 20000L,
      se_type = NULL, se_hac_lags = NULL, include_ordering = FALSE
    ),
    collapse = " "
  )
  grepl("Gaussian multiplicative-variance MLE/QMLE", notes, fixed = TRUE) &&
    grepl("Zero squared residuals are handled", notes, fixed = TRUE) &&
    !grepl("No PPML standard errors", notes, fixed = TRUE) &&
    !grepl("appended after that ordered pair", notes, fixed = TRUE)
})

hvt_se_frame <- function(se_col) {
  data.frame(
    coef = hvt_coef, expected = se_col, observed = se_col, opg = se_col,
    robust = se_col, hac = c(0.65, 0.05), check.names = FALSE
  )
}
hvt_harvey_se <- c(hvt_harvey, list(se = list(
  reference = hvt_se_frame(c(0.5, 0.05)),
  point = hvt_se_frame(c(0.6, 0.05)),
  hac_lags = 4L
)))
hvt_hac <- hvt_parts(hvt_harvey_se, se_type = "hac")

check("Harvey panel renders hac t-stats/stars and selects the hac column", {
  # reference theta_0 = -1.3 / hac se 0.65 = -2.00 -> ** (observed 0.5 -> ***)
  identical(hvt_hac$columns[[1L]][[1L]], "-1.300$^{**}$") &&
    identical(hvt_hac$columns[[1L]][[2L]], "(-2.00)")
})

check("Harvey panel keeps set columns free of statistic cells", {
  # the theta_0 statistic row carries the two point-column t's and blank set
  # cells; pin it to confirm no endpoint statistics were emitted
  identical(hvt_row(hvt_hac, 2L), "(-2.00) & (-1.85) &  & ")
})

check("Harvey panel stays blank and notes stay deferred when se_type is NULL", {
  # the plain parts have blank statistic rows AND the notes keep the deferred
  # line -- panel and notes agree that NULL means standard errors are absent
  notes <- paste(
    build_harvey_panel_notes(hvt_harvey, 0.05, 4000L, 20000L,
      se_type = NULL, se_hac_lags = NULL, include_ordering = FALSE
    ),
    collapse = " "
  )
  identical(hvt_row(hvt_plain, 2L), " &  &  & ") &&
    grepl("No Harvey standard errors are reported (deferred)", notes, fixed = TRUE)
})

check("Harvey notes describe the hac SE variation and keep the caveat", {
  # these notes carry no tau > 0 interval rows, so they also pin the caveat's
  # tau = 0 sentence as unconditional and the analytic ratio as describing the
  # reference column alone
  notes <- paste(
    build_harvey_panel_notes(hvt_harvey_se, 0.05, 4000L, 20000L,
      se_type = "hac", se_hac_lags = 4L, include_ordering = FALSE
    ),
    collapse = " "
  )
  grepl("Newey", notes) && grepl("QMLE", notes) &&
    grepl("propagate the first-stage sampling error", notes, fixed = TRUE) &&
    grepl("in the reference column are", notes, fixed = TRUE) &&
    !grepl("No Harvey standard errors", notes, fixed = TRUE)
})

check("Harvey panel fails loud when se_type is set but the se frame is absent", {
  tryCatch(
    {
      hvt_parts(hvt_harvey, se_type = "hac")
      FALSE
    },
    error = function(e) TRUE
  )
})

check("Harvey panel rejects an unknown se_type", {
  tryCatch(
    {
      hvt_parts(hvt_harvey_se, se_type = "bogus")
      FALSE
    },
    error = function(e) TRUE
  )
})

rm(
  hvt_coef, hvt_set, hvt_harvey, hvt_parts, hvt_row, hvt_plain, hvt_se_frame,
  hvt_harvey_se, hvt_hac
)
