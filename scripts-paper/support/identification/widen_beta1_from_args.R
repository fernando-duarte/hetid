# Widen the beta1 interval onto the same certified points the theta box was
# widened onto. beta1(w) = beta1R - beta2R'w is a linear image of the same set,
# so every accepted argmax is a feasible w whose image belongs in the beta1
# interval. Both full-sample and bootstrap refinement use this operation.
#
# Invisible under spec A, where beta2R is zero and every beta1 row is a point,
# which is why it survived until spec B made beta1 set-valued.
widen_beta1_from_args <- function(beta1_tab, beta1r, beta2r, args) {
  if (!length(args)) {
    return(beta1_tab)
  }
  bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
  # A loading that is zero in exact arithmetic can arrive as 1e-13. The PCs are
  # prcomp scores, so both blocks are mean-centred and the intercept's loading is
  # a known zero corrupted by lm()'s arithmetic. Testing it with == 0 passes under
  # spec A, where beta2R is a literal zero matrix, and fails under spec B.
  #
  # Widening such a row optimizes a constant functional: every certified point
  # maps to the same value in exact arithmetic, but sum(loading * w) differs in
  # the last bits across points, so min and max separate by ~1e-15. That is
  # enough to defeat the renderer's exact degeneracy test and print a spurious
  # [x, x] cell with a confidence interval beneath a row that is point
  # identified. Compare against the matrix scale so the test is scale-free.
  null_scale <- max(abs(beta2r)) * sqrt(.Machine$double.eps)
  for (k in seq_len(nrow(beta1_tab))) {
    p <- beta1_tab$coef[[k]]
    loading <- beta2r[, p]
    # a null column is point identification, not a wide interval to tighten
    if (!any(abs(loading) > null_scale) ||
      !identical(beta1_tab$status[[k]], bounded)) {
      next
    }
    vals <- vapply(
      args, function(w) unname(beta1r[[p]] - sum(loading * w)), numeric(1)
    )
    beta1_tab$set_lower[[k]] <- min(beta1_tab$set_lower[[k]], min(vals))
    beta1_tab$set_upper[[k]] <- max(beta1_tab$set_upper[[k]], max(vals))
  }
  beta1_tab
}
