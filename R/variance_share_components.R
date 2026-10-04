# Squared coefficient intervals attain zero when the interval crosses zero
variance_share_component_range <- function(tab, s_block, var_c) {
  share_scale <- 100 * diag(s_block) / var_c
  lo <- ifelse(tab$set_lower <= 0 & tab$set_upper >= 0, 0,
    pmin(tab$set_lower^2, tab$set_upper^2)
  )
  data.frame(
    lo = share_scale * lo, hi = share_scale * pmax(tab$set_lower^2, tab$set_upper^2),
    status = tab$status
  )
}

variance_share_assert_coherent <- function(cc, blocks, control) {
  for (block in blocks) {
    comps <- block[1L] + seq_len(block[2L])
    if (all(is.finite(c(cc$lo[c(block[1L], comps)], cc$hi[c(block[1L], comps)])))) {
      if (cc$hi[block[1L]] < control$coherence_ratio * max(cc$hi[comps])) {
        stop_hetid("Block share max below a component max.")
      }
      if (cc$lo[block[1L]] < control$coherence_ratio * max(cc$lo[comps]) -
        control$coherence_slack) {
        stop_hetid("Block share min below a component min.")
      }
    }
  }
  invisible(cc)
}

variance_share_set_column <- function(st, e_rows, objectives, covariance, control) {
  joint_status <- profile_status_worst(st$theta$status)
  share_grid <- variance_share_grid(st$theta, st$quadratic, control)
  e_rng <- variance_share_range(
    st$theta, st$quadratic, objectives$expected, control, share_grid
  )
  news_rng <- variance_share_range(
    st$theta, st$quadratic, objectives$news, control, share_grid
  )
  combined_rng <- variance_share_range(
    st$theta, st$quadratic, objectives$combined, control, share_grid
  )
  e_comp <- variance_share_component_range(
    st$beta1[e_rows, ],
    covariance$s_e, covariance$var_c
  )
  n_comp <- variance_share_component_range(st$theta, covariance$s_n, covariance$var_c)
  list(
    lo = unname(c(e_rng[1], e_comp$lo, news_rng[1], n_comp$lo, combined_rng[1])),
    hi = unname(c(e_rng[2], e_comp$hi, news_rng[2], n_comp$hi, combined_rng[2])),
    status = c(joint_status, e_comp$status, joint_status, n_comp$status, joint_status)
  )
}
