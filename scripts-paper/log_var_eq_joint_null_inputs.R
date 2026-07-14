# Pure input-seam helper for the joint-null diagnostic: add a qtr-aligned naive
# reference residual eps_ref to the frozen log_var_eq$inputs list without mutating
# any pre-existing field. eps_ref supplies the crossing-stability scale
# e_scale_ref = median(abs(eps_ref)); it is the mean-equation naive OLS residual
# (the log-variance fit lives in log-squared units, the wrong reference for a
# residual-zero census), aligned to the inputs by the unique qtr key. Definitions
# only; sourced by log_var_eq.R before the inputs list is finalized.

# align eps_ref_raw (keyed by qtr) to inputs$qtr and attach it as inputs$eps_ref;
# every pre-existing field is left identical() and the alignment is shuffle-
# invariant because the merge is by qtr, never by row position
logvar_joint_null_extend_inputs <- function(inputs, eps_ref_raw, qtr) {
  stopifnot(is.list(inputs), !is.null(inputs$qtr))
  if (length(eps_ref_raw) != length(qtr)) {
    stop("eps_ref_raw and its qtr key must have equal length")
  }
  if (anyDuplicated(qtr) || anyDuplicated(inputs$qtr)) {
    stop("qtr keys must be unique to align eps_ref for the joint-null diagnostic")
  }
  idx <- match(inputs$qtr, qtr)
  if (anyNA(idx)) {
    stop("every inputs$qtr must have a matching eps_ref qtr")
  }
  eps_ref <- unname(eps_ref_raw[idx])
  if (length(eps_ref) != length(inputs$qtr) || any(!is.finite(eps_ref)) ||
    !(stats::median(abs(eps_ref)) > 0)) {
    stop("eps_ref must be finite, qtr-length, with positive median absolute value")
  }
  inputs$eps_ref <- eps_ref
  inputs
}
