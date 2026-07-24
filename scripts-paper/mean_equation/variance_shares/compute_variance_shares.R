# Variance shares of consumption growth for the structural equation of
# estimate_identified_set.R: the share of Var(Delta c_{t+1}) attributable to the
# expected-SDF block PC_{E,t}' b_E and the SDF-news block PC_{N,t+1}' b_N,
# per block and per component, at the OLS coefficients, the closed-form
# Lewbel point at tau = 0, and as ranges over the joint identified set at
# each tau_display slack. Leaves the var_share result list for
# render_variance_share_table.R (row order: E block, E components, news block, news
# components).
# Run from run_pipeline.R after the identified-set estimator.

paper_source_once(paper_path(
  "mean_equation",
  "variance_shares",
  "share_optimization.R"
))
paper_source_once(paper_path("support", "reporting", "cells.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path(
  "support",
  "identification",
  "quadratic_system.R"
))

# Centered 1/T second moments use the identification moments' convention.
s_e <- centered_cov_t(set_id_mean_eq$data[set_id_mean_eq$x_cols])
s_n <- centered_cov_t(set_id_mean_eq$y2)
var_c <- drop(centered_cov_t(set_id_mean_eq$y1))

# each block's share is a quadratic theta' P theta + q' theta + r in theta,
# in percent of Var(dc); the news block directly, the E block through
# b_E(theta) = beta1R_E - (beta2R_E)' theta (constant under the null)
beta1r_e <- set_id_mean_eq$beta1r[set_id_mean_eq$x_cols]
beta2r_e <- set_id_mean_eq$beta2r[, set_id_mean_eq$x_cols, drop = FALSE]
b_map <- t(beta2r_e) # p x I: b_E(theta) = beta1r_e - b_map %*% theta
news_quad_share <- share_quad(100 * s_n / var_c, numeric(n_pc), 0)
e_quad_share <- share_quad(
  100 * crossprod(b_map, s_e %*% b_map) / var_c,
  drop(-200 * beta2r_e %*% (s_e %*% beta1r_e)) / var_c,
  100 * drop(beta1r_e %*% s_e %*% beta1r_e) / var_c
)

# row-order guards, as in render_structural_equation_table.R
stopifnot(
  identical(
    set_id_mean_eq$theta_table$coef,
    PAPER_ANALYSIS_CONTRACT$model$news_pc_cols
  ),
  identical(
    set_id_mean_eq$beta1_table$coef,
    c(
      PAPER_ANALYSIS_CONTRACT$model$intercept_col,
      PAPER_ANALYSIS_CONTRACT$model$lag_expected_pc_cols
    )
  )
)
e_rows <- match(set_id_mean_eq$x_cols, set_id_mean_eq$beta1_table$coef)

# share ranges over the joint identified set at each display slack
quads <- lapply(set_id_mean_eq$tau_display, function(tau) {
  build_pipeline_quadratic_system(
    set_id_mean_eq$gamma, rep(tau, ncol(set_id_mean_eq$gamma)),
    set_id_mean_eq$moments
  )$quadratic
})
# under the maintained null b_E does not vary over the set, so its share is
# the constant at beta1R and needs no optimizer; otherwise it ranges over
# the same joint set as the news block
e_const <- block_share(matrix(beta1r_e, 1), s_e)
set_share_cols <- Map(function(nm, quad) {
  st <- set_id_mean_eq$set_tables[[nm]]
  # a block share ranges over the whole joint theta set, so it is only as
  # established as the least-established row of that set: an uncertified row
  # makes the block share uncertified too, and validity outranks boundedness
  # exactly as in the solvers' own status3
  joint_status <- paper_endpoint_status_worst(st$theta$status)
  e_rng <- if (impose_beta2r_null) {
    rep(e_const, 2)
  } else {
    set_share_range(st$theta, quad, e_quad_share)
  }
  news_rng <- set_share_range(st$theta, quad, news_quad_share)
  e_comp <- component_share_range(st$beta1[e_rows, ], s_e)
  n_comp <- component_share_range(st$theta, s_n)
  list(
    lo = c(e_rng[1], e_comp$lo, news_rng[1], n_comp$lo),
    hi = c(e_rng[2], e_comp$hi, news_rng[2], n_comp$hi),
    # under the null b_E is a constant, so its block share is the value at
    # beta1R and stays certified whatever the theta set does
    status = c(
      if (impose_beta2r_null) PAPER_ENDPOINT_STATUS[["bounded"]] else joint_status,
      e_comp$status, joint_status, n_comp$status
    )
  )
}, names(set_id_mean_eq$set_tables), quads)

# coherence of the polished block ranges against the exact component ranges:
# a block share dominates each of its component shares pointwise (up to the
# PCs' tiny sample cross-covariances), so the block max must reach the
# largest component max and the block min the largest component min
news_row <- n_pc + 2L
for (cc in set_share_cols) {
  for (block_row in c(1L, news_row)) {
    comps <- block_row + seq_len(n_pc)
    if (all(is.finite(c(cc$lo[c(block_row, comps)], cc$hi[c(block_row, comps)])))) {
      stopifnot(
        "block share max below a component max" =
          cc$hi[block_row] >=
            PAPER_ANALYSIS_CONTRACT$variance_share$coherence_ratio *
              max(cc$hi[comps]),
        "block share min below a component min" =
          cc$lo[block_row] >=
            PAPER_ANALYSIS_CONTRACT$variance_share$coherence_ratio *
              max(cc$lo[comps]) -
              PAPER_ANALYSIS_CONTRACT$variance_share$coherence_slack
      )
    }
  }
}

var_share <- list(
  ols = fixed_shares("ols"),
  point = fixed_shares("point"),
  set_cols = set_share_cols,
  news_row = news_row,
  sd_c = sqrt(var_c)
)

cat(sprintf(
  "variance shares: news block %s--%s%% of Var(dc) at tau = %s\n",
  paper_format_number(
    var_share$set_cols[[1]]$lo[news_row],
    PAPER_REPORTING_CONTROL$cells$variance_share$digits,
    "na"
  ),
  paper_format_number(
    var_share$set_cols[[1]]$hi[news_row],
    PAPER_REPORTING_CONTROL$cells$variance_share$digits,
    "na"
  ),
  paper_format_tau(set_id_mean_eq$tau_baseline)
))

rm(
  centered_cov_t, s_e, s_n, var_c, block_share, share_quad, beta1r_e,
  beta2r_e, b_map, news_quad_share, e_quad_share, e_const,
  polish_extreme, set_share_range, component_share_range,
  e_rows, fixed_shares, quads, set_share_cols, news_row, cc, block_row, comps
)
