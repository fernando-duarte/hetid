# Offline identity and fallback checks for the PPML estimator constructor
# (scripts-paper/log_var_eq_ppml.R). The canonical spec_id must separate
# fit-changing inputs -- adjacent response scales and the realized point-vs-anchor
# fallback branch -- while staying invariant to display settings such as
# options(digits). A missing Lewbel point yields a NULL point start bundle and a
# recorded baseline-grid anchor provenance; an anchor whose response has no
# positive value fails closed rather than inventing a scale. Split out of
# test_logvar_ppml_fit.R to keep both files under the hard 200-line cap. RED:
# the estimator does not exist yet, so each assertion is failure-guarded.

fx <- ppml_fx
w1 <- fx$w1
w2 <- fx$w2
pcr <- fx$pcr
qtr <- fx$qtr
b_ref <- fx$b_ref
anchor_b <- c(0.2, -0.1)
old_digits <- getOption("digits")

# The estimator's canonical spec_id, addressed through the constructor so the
# test does not depend on logvar_ppml_spec_id's private argument order.
spec_of <- function(scale = 1, b_point = NULL,
                    anchor = anchor_b, source = "baseline_grid_first") {
  logvar_ppml_estimator(
    w1, w2, pcr, qtr,
    b_point = b_point, scale_anchor_b = anchor,
    scale_anchor_source = source, response_scale = scale
  )$metadata$spec_id
}

check("adjacent response scales give distinct spec_id", tryCatch(
  !identical(spec_of(1), spec_of(1 + 1e-10)),
  error = function(e) FALSE
))

check("the realized point-vs-anchor branch changes spec_id", tryCatch(
  !identical(
    spec_of(1, b_point = NULL, source = "baseline_grid_first"),
    spec_of(1, b_point = b_ref, source = "lewbel_point")
  ),
  error = function(e) FALSE
))

check("a different scale anchor changes spec_id", tryCatch(
  !identical(spec_of(1, anchor = anchor_b), spec_of(1, anchor = c(0.25, -0.1))),
  error = function(e) FALSE
))

check("spec_id is invariant to options(digits)", tryCatch(
  {
    base_id <- spec_of(1)
    options(digits = 3L)
    low_id <- spec_of(1)
    options(digits = old_digits)
    identical(base_id, low_id)
  },
  error = function(e) {
    options(digits = old_digits)
    FALSE
  }
))

check("a null b_point yields a null bundle and grid anchor provenance", tryCatch(
  {
    est <- logvar_ppml_estimator(
      w1, w2, pcr, qtr,
      b_point = NULL, scale_anchor_b = anchor_b,
      scale_anchor_source = "baseline_grid_first"
    )
    is.null(est$start_bundle) &&
      identical(est$metadata$scale_anchor_source, "baseline_grid_first")
  },
  error = function(e) FALSE
))

check("an all-zero anchor response fails closed", tryCatch(
  {
    w1_flat <- drop(w2 %*% anchor_b)
    logvar_ppml_estimator(
      w1_flat, w2, pcr, qtr,
      b_point = NULL, scale_anchor_b = anchor_b,
      scale_anchor_source = "baseline_grid_first"
    )
    FALSE
  },
  error = function(e) TRUE
))

# The one preparation path errors before any fit on a corrupted key universe
# or a drifted sample: duplicate or missing qtr keys, wrong PC names, and a
# sample_id mismatch all stop; the intact contract passes unchanged.
contract_of <- function(q) {
  list(
    qtr = q, n = fx$n, pc_names = colnames(pcr),
    sample_id = logvar_sample_id(q, w1, w2, pcr)
  )
}
inputs_of <- function(q) list(w1 = w1, w2 = w2, pcr = pcr, qtr = q)
guard_errors <- function(inputs, contract) {
  tryCatch(
    {
      logvar_ppml_validate_inputs(inputs, contract)
      FALSE
    },
    error = function(e) TRUE
  )
}
check("valid inputs pass the preparation-path validator", tryCatch(
  {
    logvar_ppml_validate_inputs(inputs_of(qtr), contract_of(qtr))
    TRUE
  },
  error = function(e) FALSE
))
check("duplicate qtr keys error before fitting", tryCatch(
  {
    qtr_dup <- qtr
    qtr_dup[2] <- qtr_dup[1]
    guard_errors(inputs_of(qtr_dup), contract_of(qtr))
  },
  error = function(e) FALSE
))
check("missing qtr keys error before fitting", tryCatch(
  {
    qtr_na <- qtr
    qtr_na[1] <- NA
    guard_errors(inputs_of(qtr_na), contract_of(qtr))
  },
  error = function(e) FALSE
))
check("renamed PC columns error before fitting", tryCatch(
  {
    bad <- inputs_of(qtr)
    colnames(bad$pcr) <- c("l.pc1", "l.pc2", "l.pc3", "pc4")
    guard_errors(bad, contract_of(qtr))
  },
  error = function(e) FALSE
))
check("a drifted sample_id errors before fitting", tryCatch(
  {
    drifted <- contract_of(qtr)
    drifted$sample_id <- paste0(drifted$sample_id, "x")
    guard_errors(inputs_of(qtr), drifted)
  },
  error = function(e) FALSE
))
