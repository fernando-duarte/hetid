# Offline checks for the analytic PPML QMLE standard errors
# from the PPML standard-error module: the covariance variants pinned against
# sandwich/glm references, the fail-closed guards, the point-column t-stat
# rendering in logvar_ppml_table_parts, and the notes computation clause.
# Sourced by test_ppml.R after table_checks.R; reuses ppml_fx
# and the ptbl_* fixtures defined there.

# lag count emulating the pipeline's logvar_ppml_se_hac_lags; sandwich is a
# pipeline dependency but the offline suite must survive its absence, so its
# comparison checks pass trivially when it is not installed
test_hac_lags <- 4L
have_sandwich <- requireNamespace("sandwich", quietly = TRUE)

# a zero-inflated response variant: the PPML setting admits exact zeros
se_y0 <- ppml_fx$y
se_y0[1:5] <- 0
se_m <- stats::glm(ppml_fx$y ~ ppml_fx$pcr, family = stats::quasipoisson("log"))
se_coef <- unname(stats::coef(se_m))
se_vc <- logvar_ppml_vcov(se_coef, ppml_fx$y, ppml_fx$x_mat, hac_lags = test_hac_lags)
se_d <- function(a, b) max(abs(unname(a) - unname(b)))

# naive anchors on stats::vcov (always available); hc/hac anchor on sandwich
check(
  "ppml se naive == vcov(quasipoisson glm)",
  se_d(se_vc$naive, stats::vcov(se_m)) < 1e-8
)
check(
  "ppml se hc0 == sandwich vcovHC HC0",
  !have_sandwich || se_d(se_vc$hc0, sandwich::vcovHC(se_m, type = "HC0")) < 1e-8
)
check(
  "ppml se hc1 == sandwich vcovHC HC1",
  !have_sandwich || se_d(se_vc$hc1, sandwich::vcovHC(se_m, type = "HC1")) < 1e-8
)
check(
  "ppml se hac == NeweyWest bartlett lags",
  !have_sandwich || se_d(
    se_vc$hac,
    sandwich::NeweyWest(se_m, lag = test_hac_lags, prewhite = FALSE, adjust = FALSE)
  ) < 1e-8
)
check(
  "ppml se hac at lags 0 collapses to hc0",
  se_d(logvar_ppml_vcov(se_coef, ppml_fx$y, ppml_fx$x_mat, 0L)$hac, se_vc$hc0) < 1e-12
)

# zeros in the response are admissible and still match a glm on the same y
check("ppml se handles exact-zero responses like the reference glm", {
  m0 <- stats::glm(se_y0 ~ ppml_fx$pcr, family = stats::quasipoisson("log"))
  !have_sandwich || se_d(
    logvar_ppml_vcov(unname(stats::coef(m0)), se_y0, ppml_fx$x_mat, test_hac_lags)$hc0,
    sandwich::vcovHC(m0, type = "HC0")
  ) < 1e-8
})

# column-normalized conditioning gate: a fit with one PC column rescaled by 1e6
# has raw rcond(A) ~ 5e-13 but a well conditioned normalized matrix, so the SE
# must stay finite and match sandwich -- a raw rcond(A) gate would reject it
check("ppml se accepts a column-rescaled fit via the normalized gate", {
  pcr_s <- ppml_fx$pcr
  pcr_s[, 2] <- pcr_s[, 2] * 1e6
  xs <- cbind(1, pcr_s)
  colnames(xs) <- c("(Intercept)", colnames(pcr_s))
  ms <- stats::glm(ppml_fx$y ~ pcr_s, family = stats::quasipoisson("log"))
  vs <- logvar_ppml_vcov(unname(stats::coef(ms)), ppml_fx$y, xs, test_hac_lags)
  all(is.finite(vs$hc0)) &&
    (!have_sandwich || se_d(vs$hc0, sandwich::vcovHC(ms, type = "HC0")) < 1e-8)
})

# fail-closed: non-finite coef and a singular design give all-NA variance
check("ppml se fails closed on non-finite coef", {
  bad <- logvar_ppml_vcov(
    rep(NA_real_, ncol(ppml_fx$x_mat)), ppml_fx$y, ppml_fx$x_mat, test_hac_lags
  )
  all(vapply(bad, function(v) all(is.na(v)), logical(1)))
})
check("ppml se fails closed on a rank-deficient design", {
  xd <- cbind(ppml_fx$x_mat, ppml_fx$x_mat[, 2]) # duplicate column -> singular A
  all(is.na(logvar_ppml_vcov(rep(0, ncol(xd)), ppml_fx$y, xd, test_hac_lags)$hc0))
})
check("ppml se fails closed when n <= p", {
  xw <- ppml_fx$x_mat[1:4, , drop = FALSE] # 4 rows, 5 columns
  all(is.na(logvar_ppml_vcov(rep(0, ncol(xw)), ppml_fx$y[1:4], xw, test_hac_lags)$hac))
})
check("ppml vcov rejects a malformed hac_lags argument", {
  tryCatch(
    {
      logvar_ppml_vcov(se_coef, ppml_fx$y, ppml_fx$x_mat, -1L)
      FALSE
    },
    error = function(e) TRUE
  )
})

# the canonical variant keys are exactly the four validated types
check("ppml se types are the four canonical keys", {
  identical(LOGVAR_PPML_SE_TYPES, c("naive", "hc0", "hc1", "hac")) &&
    identical(match.arg("hac", LOGVAR_PPML_SE_TYPES), "hac") &&
    tryCatch(
      {
        match.arg("HC9", LOGVAR_PPML_SE_TYPES)
        FALSE
      },
      error = function(e) TRUE
    )
})

# the SE frame: one row per coefficient, one numeric column per variant
check("ppml se frame is one row per coef, one column per variant", {
  fr <- logvar_ppml_se_frame(se_coef, ppml_fx$y, ppml_fx$x_mat, test_hac_lags)
  identical(fr$coef, colnames(ppml_fx$x_mat)) &&
    all(LOGVAR_PPML_SE_TYPES %in% names(fr)) &&
    all(fr$naive > 0) && nrow(fr) == ncol(ppml_fx$x_mat)
})

# point-column render: a fixture ppml carrying an $se frame prints t = coef/se
# in the interleaved stat row with normal-approximation stars. The naive and hac
# columns differ so the test also confirms se_type selects the right column:
# theta_0 = -1.3 at hac se 0.65 gives t = -2.00, p = 0.0455 -> ** (the naive se
# 0.5 would give -2.60 -> ***, so ** proves the hac column was used).
se_ppml_fix <- ptbl_ppml
se_ppml_fix$se <- list(
  reference = data.frame(
    coef = ptbl_coef, naive = c(0.5, 0.02),
    hc0 = c(0.55, 0.02), hc1 = c(0.56, 0.02), hac = c(0.65, 0.02),
    check.names = FALSE
  ),
  point = data.frame(
    coef = ptbl_coef, naive = c(0.6, 0.05),
    hc0 = c(0.6, 0.05), hc1 = c(0.6, 0.05), hac = c(0.6, 0.05),
    check.names = FALSE
  ),
  hac_lags = test_hac_lags
)
check("ppml parts render hac t-stats/stars and select the hac column", {
  pr <- logvar_ppml_table_parts(se_ppml_fix, c(0.05, 0.1), 1L, se_type = "hac")
  grepl("^{**}", pr$columns[[1]][1], fixed = TRUE) && # -1.3/0.65 = -2.00 -> **
    !grepl("***", pr$columns[[1]][1], fixed = TRUE) && # not the naive -2.60 ***
    identical(pr$columns[[1]][2], "(-2.00)") &&
    identical(pr$columns[[3]][2], "") # set columns keep blank stat rows
})
check("ppml parts stay blank when se_type is NULL (back-compat)", {
  pr <- logvar_ppml_table_parts(ptbl_ppml, c(0.05, 0.1), 1L)
  identical(pr$columns[[1]][2], "") && identical(pr$columns[[2]][2], "")
})
# a finite coefficient whose SE failed the conditioning gate must show its value
# with a "--" stat row (SE unavailable), never a blank row that would read as a
# tested-but-insignificant coefficient
check("ppml parts mark an unavailable SE with -- , not a blank stat row", {
  se_ppml_na <- se_ppml_fix
  se_ppml_na$se$reference$hac[1] <- NA_real_
  pr <- logvar_ppml_table_parts(se_ppml_na, c(0.05, 0.1), 1L, se_type = "hac")
  identical(pr$columns[[1]][1], "-1.300") && # coefficient value still printed
    identical(pr$columns[[1]][2], "--") # stat row marks the SE unavailable
})
check("ppml parts fail loud when se_type is set but se is absent", {
  tryCatch(
    {
      logvar_ppml_table_parts(ptbl_ppml, c(0.05, 0.1), 1L, se_type = "hac")
      FALSE
    },
    error = function(e) TRUE
  )
})
check("ppml parts reject an unknown se_type", {
  tryCatch(
    {
      logvar_ppml_table_parts(se_ppml_fix, c(0.05, 0.1), 1L, se_type = "bogus")
      FALSE
    },
    error = function(e) TRUE
  )
})
check("ppml notes explain the SE computation, name the default, keep the caveat", {
  nt <- paste(build_ppml_table_notes(ptbl_note_fixture, 0.05, 1L, 2L,
    se_type = "hac", se_hac_lags = test_hac_lags
  ), collapse = " ")
  grepl("Newey", nt) && grepl("Eicker", nt) && grepl("QMLE", nt) &&
    grepl("condition on the plug-in", nt) &&
    !grepl("No PPML standard errors are reported", nt, fixed = TRUE)
})
check("ppml inference notes do not claim endpoint inference is deferred", {
  nt <- paste(build_ppml_panel_notes(ptbl_note_fixture, 0.05, 1L, 2L,
    se_type = "hac", se_hac_lags = test_hac_lags,
    set_endpoint_inference = TRUE
  ), collapse = " ")
  grepl("outer confidence envelopes", nt, fixed = TRUE) &&
    !grepl("uncertainty is deferred", nt, fixed = TRUE)
})
