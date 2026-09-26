# Every checked point updates both eligible sides of its structural image.
widen_beta1_from_args <- function(beta1_tab, beta1r, beta2r, args) {
  if (!length(args)) {
    return(beta1_tab)
  }
  bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
  for (k in seq_len(nrow(beta1_tab))) {
    p <- beta1_tab$coef[[k]]
    loading <- beta2r[, p]
    if (all(loading == 0)) next
    values <- vapply(args, function(w) unname(beta1r[[p]] - sum(loading * w)), 0)
    stopifnot(all(is.finite(values)))
    if (beta1_tab$lower_status[k] == bounded) {
      beta1_tab$set_lower[k] <- min(beta1_tab$set_lower[k], values)
    }
    if (beta1_tab$upper_status[k] == bounded) {
      beta1_tab$set_upper[k] <- max(beta1_tab$set_upper[k], values)
    }
  }
  beta1_tab
}
