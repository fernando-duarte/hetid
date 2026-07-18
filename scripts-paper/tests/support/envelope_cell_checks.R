# Offline checks for envelope_cell and its
# threading into logvar_ppml_table_parts and logvar_harvey_build_fragment: the
# vol-equation set-endpoint confidence-envelope row rendered beneath each set
# cell. Self-contained fixtures so this file can be sourced from either
# estimator's test entry point independent of the other check files' cleanup.

check("envelope_cell renders all four forms exactly", {
  identical(envelope_cell(-1.25, -1.05, "two-sided"), "$[-1.250,\\,-1.050]$") &&
    identical(envelope_cell(-Inf, -1.05, "upper"), "$(-\\infty,\\,-1.050]$") &&
    identical(envelope_cell(-1.25, Inf, "lower"), "$[-1.250,\\,\\infty)$") &&
    identical(envelope_cell(NA_real_, NA_real_, "none"), "")
})
check("envelope_cell is vectorized and blanks a non-finite ci on a live side", {
  identical(
    envelope_cell(c(-1.25, NA_real_), c(-1.05, -1.05), c("two-sided", "two-sided")),
    c("$[-1.250,\\,-1.050]$", "")
  )
})

ecc_coef <- c("(Intercept)", "l.pc1")
ecc_set <- function(lo, hi) {
  data.frame(
    coef = ecc_coef, set_lower = lo, set_upper = hi, status = "bounded",
    stringsAsFactors = FALSE
  )
}
ecc_env_frame <- function(ci_lo, ci_hi, side) {
  data.frame(
    coef = ecc_coef, ci_lower = ci_lo, ci_upper = ci_hi, side = side,
    stringsAsFactors = FALSE
  )
}
# tau = 0.05: coef1 two-sided, coef2 upper (unbounded lower). tau = 0.1: coef1
# lower (unbounded upper), coef2 none (suppressed) -- exercises all four
# envelope_cell forms through the real table-assembly path, per coef and tau.
ecc_keys <- vapply(c(0.05, 0.1), paper_tau_key, character(1))
ecc_envelope <- stats::setNames(
  list(
    ecc_env_frame(c(-1.28, -Inf), c(-1.12, 0.21), c("two-sided", "upper")),
    ecc_env_frame(c(-1.35, NA_real_), c(Inf, NA_real_), c("lower", "none"))
  ),
  ecc_keys
)
ecc_ppml <- list(
  sample = list(n = 12L),
  table = data.frame(
    coef = ecc_coef, reference = c(-1.3, 0.2), point = c(-1.2, 0.18),
    stringsAsFactors = FALSE
  ),
  sets = stats::setNames(
    list(ecc_set(c(-1.25, 0.17), c(-1.15, 0.19)), ecc_set(c(-1.3, 0.16), c(-1.1, 0.2))),
    ecc_keys
  )
)
ecc_ppml_parts <- logvar_ppml_table_parts(
  ecc_ppml, c(0.05, 0.1), 1L,
  envelope = ecc_envelope
)
check("PPML table parts render the envelope on a separate row beneath the set cell", {
  identical(ecc_ppml_parts$columns[[3]], c(
    "$[-1.250,\\,-1.150]$", "$[-1.280,\\,-1.120]$",
    "$[0.170,\\,0.190]$", "$(-\\infty,\\,0.210]$", "--", "12"
  )) &&
    identical(ecc_ppml_parts$columns[[4]], c(
      "$[-1.300,\\,-1.100]$", "$[-1.350,\\,\\infty)$",
      "$[0.160,\\,0.200]$", "", "--", "12"
    ))
})
check("PPML table parts stay byte-identical to the no-envelope path when envelope is NULL", {
  no_env <- logvar_ppml_table_parts(ecc_ppml, c(0.05, 0.1), 1L)
  identical(no_env$columns[[3]][2], "") && identical(no_env$columns[[4]][4], "")
})

# the Harvey fragment builder is only loaded by the Harvey test entry point;
# this file is shared with the PPML entry point, so guard its use
if (exists("logvar_harvey_build_fragment")) {
  ecc_harvey <- list(
    table = data.frame(
      coef = ecc_coef, reference = c(-1.3, 0.2), point = c(-1.2, 0.18),
      stringsAsFactors = FALSE
    ),
    sets = ecc_ppml$sets
  )
  ecc_harvey_fragment <- logvar_harvey_build_fragment(
    ecc_harvey, 12L, c(0.05, 0.1),
    envelope = ecc_envelope
  )
  check("Harvey fragment renders the envelope on a separate row beneath the set cell", {
    any(ecc_harvey_fragment == paste0(
      "$\\theta^{H}_0$ & -1.300 & -1.200 & ",
      "$[-1.250,\\,-1.150]$ & $[-1.300,\\,-1.100]$ \\\\"
    )) &&
      any(ecc_harvey_fragment ==
        " &  &  & $[-1.280,\\,-1.120]$ & $[-1.350,\\,\\infty)$ \\\\") &&
      any(ecc_harvey_fragment == paste0(
        "$\\theta^{H}_{1,R}$ & 0.200 & 0.180 & ",
        "$[0.170,\\,0.190]$ & $[0.160,\\,0.200]$ \\\\"
      )) &&
      any(ecc_harvey_fragment == " &  &  & $(-\\infty,\\,0.210]$ &  \\\\")
  })
  check("Harvey fragment stays byte-identical to the no-envelope path when envelope is NULL", {
    no_env <- logvar_harvey_build_fragment(ecc_harvey, 12L, c(0.05, 0.1))
    any(no_env == "$N$ & 12 & 12 & 12 & 12 \\\\") &&
      !any(grepl("-1.280", no_env, fixed = TRUE))
  })
  rm(ecc_harvey, ecc_harvey_fragment)
}

rm(ecc_coef, ecc_set, ecc_env_frame, ecc_keys, ecc_envelope, ecc_ppml, ecc_ppml_parts)
