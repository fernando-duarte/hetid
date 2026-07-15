# Offline checks for the vol-equation max-root OUTER confidence envelope
# (scripts/utils/logvar_set_envelope.R). The construction: inward endpoint
# deviations D_L = L* - Lhat, D_U = Uhat - U*, robust (MAD, location-invariant)
# scales, per-draw root max(0, D_L/s_L, D_U/s_U), conservative order-statistic
# critical value, envelope [Lhat - c s_L, Uhat + c s_U]. Half-infinite sets use
# the single live side; a divergent-draw fraction or a degenerate scale fails the
# regularity gate. NOT IM/Stoye, NOT percentiles.
set.seed(1L)

# two-sided: draws tight around a finite [-2, -1] set -> envelope pads OUTWARD
lse_two <- list(
  lower = matrix(rnorm(200, -2, 0.10), 100),
  upper = matrix(rnorm(200, -1, 0.10), 100),
  lower_status = matrix("bounded", 100, 2),
  upper_status = matrix("bounded", 100, 2)
)
lse_full2 <- data.frame(
  coef = c("a", "b"), set_lower = c(-2, -2), set_upper = c(-1, -1),
  lower_status = "bounded", upper_status = "bounded", stringsAsFactors = FALSE
)
check("envelope two-sided expands both endpoints strictly outward", {
  e <- logvar_endpoint_envelope(lse_two, lse_full2, alpha = 0.10)
  all(e$side == "two-sided") && all(e$ci_lower < -2) && all(e$ci_upper > -1) &&
    all(is.finite(e$c_value)) && all(e$c_value > 0)
})
check("envelope critical value is the conservative order statistic", {
  # single coef, upper side only, known deviations -> hand-check the rank
  d <- list(
    lower = matrix(-Inf, 20, 1), upper = matrix(-2 - (1:20) / 100, 20, 1),
    lower_status = matrix("unbounded", 20, 1),
    upper_status = matrix("bounded", 20, 1)
  )
  f <- data.frame(
    coef = "a", set_lower = -Inf, set_upper = -2,
    lower_status = "unbounded", upper_status = "bounded",
    stringsAsFactors = FALSE
  )
  e <- logvar_endpoint_envelope(d, f, alpha = 0.10, min_reps = 2L)
  s <- robust_scale(-2 - (1:20) / 100)
  root <- pmax(0, ((-2) - (-2 - (1:20) / 100)) / s) # D_U = Uhat - U*
  k <- min(20L, ceiling(21 * 0.90))
  isTRUE(all.equal(e$c_value, sort(root)[k], tolerance = 1e-9)) &&
    e$side == "upper" && is.infinite(e$ci_lower)
})

# half-infinite: full-sample lower genuinely unbounded, upper bounded
lse_one <- list(
  lower = matrix(-Inf, 100, 1),
  upper = matrix(rnorm(100, -2.34, 0.15), 100),
  lower_status = matrix("unbounded", 100, 1),
  upper_status = matrix("bounded", 100, 1)
)
lse_full1 <- data.frame(
  coef = "a", set_lower = -Inf, set_upper = -2.34,
  lower_status = "unbounded", upper_status = "bounded", stringsAsFactors = FALSE
)
check("envelope gives a one-sided upper band for a genuine (-Inf, hi] set", {
  e <- logvar_endpoint_envelope(lse_one, lse_full1, alpha = 0.10)
  e$side == "upper" && is.infinite(e$ci_lower) && is.finite(e$ci_upper) &&
    e$ci_upper > -2.34
})
check("envelope does NOT fabricate a band when the opposite side is unreliable", {
  f <- lse_full1
  f$lower_status <- "unreliable"
  f$set_lower <- NA_real_
  e <- logvar_endpoint_envelope(lse_one, f, alpha = 0.10)
  e$side == "none" && is.na(e$ci_upper) && grepl("not certified", e$reason)
})
check("envelope fails the gate on a bounded side with a degenerate scale", {
  d <- list(
    lower = matrix(-Inf, 100, 1), upper = matrix(-2.34, 100, 1), # constant -> se 0
    lower_status = matrix("unbounded", 100, 1),
    upper_status = matrix("bounded", 100, 1)
  )
  e <- logvar_endpoint_envelope(d, lse_full1, alpha = 0.10)
  e$side == "none" && !e$gate_upper && grepl("degenerate", e$reason)
})
check("envelope fails the gate on a bounded side unstable across draws", {
  d <- lse_one
  d$upper[1:40, 1] <- Inf # 40% diverge -> bounded frac 0.60 < 0.85
  d$upper_status[1:40, 1] <- "unbounded"
  e <- logvar_endpoint_envelope(d, lse_full1, alpha = 0.10)
  e$side == "none" && !e$gate_upper && grepl("unstable", e$reason)
})
check("envelope renders no band for a set unbounded on both sides", {
  f <- data.frame(
    coef = "a", set_lower = -Inf, set_upper = Inf,
    lower_status = "unbounded", upper_status = "unbounded",
    stringsAsFactors = FALSE
  )
  d <- list(
    lower = matrix(-Inf, 100, 1), upper = matrix(Inf, 100, 1),
    lower_status = matrix("unbounded", 100, 1),
    upper_status = matrix("unbounded", 100, 1)
  )
  e <- logvar_endpoint_envelope(d, f, alpha = 0.10)
  is.na(e$ci_lower) && is.na(e$ci_upper) && grepl("unbounded on both", e$reason)
})
check("simultaneous critical value is >= the max coefficientwise value", {
  e <- logvar_endpoint_envelope(lse_two, lse_full2, alpha = 0.10)
  cs <- logvar_simultaneous_critical(lse_two, lse_full2, alpha = 0.10)
  is.finite(cs) && cs >= max(e$c_value) - 1e-9
})

# two-sided, distinct 10-draw dropouts per side: each side's own pool stays
# at 90% bounded (gate passes), even though the two-sided common pool (both
# sides bounded on the same draw) is smaller -- divergent draws are counted
# in frac, never dropped from the gate.
set.seed(2L)
lse_two_partial <- list(
  lower = matrix(rnorm(100, -2, 0.10), 100, 1),
  upper = matrix(rnorm(100, -1, 0.10), 100, 1),
  lower_status = matrix("bounded", 100, 1),
  upper_status = matrix("bounded", 100, 1)
)
lse_two_partial$lower[1:10, 1] <- Inf
lse_two_partial$lower_status[1:10, 1] <- "unbounded"
lse_two_partial$upper[11:20, 1] <- Inf
lse_two_partial$upper_status[11:20, 1] <- "unbounded"
lse_full2_partial <- data.frame(
  coef = "a", set_lower = -2, set_upper = -1,
  lower_status = "bounded", upper_status = "bounded", stringsAsFactors = FALSE
)
check("envelope two-sided gate passes with 10 distinct divergent draws per side", {
  e <- logvar_endpoint_envelope(lse_two_partial, lse_full2_partial, alpha = 0.10)
  e$side == "two-sided" && e$gate_lower && e$gate_upper &&
    isTRUE(all.equal(e$frac_lower, 0.90)) && isTRUE(all.equal(e$frac_upper, 0.90)) &&
    e$ci_lower < -2 && e$ci_upper > -1 && is.finite(e$c_value)
})

check("envelope fails the gate for too few bounded draws (insufficient reason)", {
  # one-sided upper-live cell: only 5 of 100 draws bounded on the upper
  # side, well under boot_min_reps(100) = 50 -> "insufficient" gate reason.
  d <- list(
    lower = matrix(-Inf, 100, 1),
    upper = matrix(c(rnorm(5, -2, 0.10), rep(Inf, 95)), 100, 1),
    lower_status = matrix("unbounded", 100, 1),
    upper_status = matrix(c(rep("bounded", 5), rep("unbounded", 95)), 100, 1)
  )
  f <- data.frame(
    coef = "a", set_lower = -Inf, set_upper = -2,
    lower_status = "unbounded", upper_status = "bounded", stringsAsFactors = FALSE
  )
  e <- logvar_endpoint_envelope(d, f, alpha = 0.10)
  e$side == "none" && !e$gate_upper && grepl("insufficient", e$reason)
})
