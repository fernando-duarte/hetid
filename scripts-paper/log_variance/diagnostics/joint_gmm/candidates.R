# Candidate construction and observed-sign-pattern accounting for the joint
# log-variance GMM search (joint-GMM, logvar-joint-gmm, dossier section 6). The
# sign-pattern census reads the literal +/- orthant of the residual e(b) and
# marks near-zero rows unavailable; the exact raw-sign gate admits only the true
# orthant, so generic constraint slack can never let the wrong sign in; the
# guard slack reports min_i(s_i e_i(b)) - eps_floor in raw units. The candidate
# table is the complete deduplicated (b, beta) product over the scanned feasible
# grid, and the start selector keeps the best-scoring start in each observed
# pattern with a deterministic separated-extra pass. Definitions only; sourced by
# the joint-GMM test entrypoint and the compatibility-search driver.

# Observed sign census of the residual e: the literal signs, the availability
# mask (rows whose magnitude clears the floor), and the +/- orthant id string.
logvar_joint_sign_pattern <- function(e, eps_floor) {
  list(
    signs = sign(e),
    available = abs(e) > eps_floor,
    id = paste(ifelse(e > 0, "+", "-"), collapse = "")
  )
}

# Exact raw-sign gate: TRUE only when every guarded row sits in its target
# orthant s_i e_i(b) > 0. No slack, so a wrong-orthant point inside a generic
# 1e-8 tolerance still fails.
logvar_joint_accept_raw_signs <- function(b, w1, w2, signs) {
  all(signs * drop(w1 - w2 %*% b) > 0)
}

# Raw-unit guard slack min_i(s_i e_i(b)) - eps_floor: nonnegative when every
# guarded row clears the floor inside its target orthant.
logvar_joint_guard_slack <- function(b, w1, w2, signs, eps_floor) {
  min(signs * drop(w1 - w2 %*% b)) - eps_floor
}

# Deduplicate a list of numeric vectors by canonical serialization,
# preserving first-occurrence order so replay is deterministic.
logvar_joint_dedupe_vectors <- function(vectors) {
  keys <- vapply(vectors, function(v) {
    paste(paper_numeric_key(v), collapse = "|")
  }, character(1))
  vectors[!duplicated(keys)]
}

# Complete candidate table over the scanned grid: one row per (b, beta) pair,
# with the beta seeds deduplicated first so repeated seeds never inflate the
# product. Columns carry the grid b, the beta seed, and its retained index.
logvar_joint_candidate_table <- function(grid_b, beta_seeds) {
  grid_b <- as.matrix(grid_b)
  if (is.matrix(beta_seeds)) {
    beta_seeds <- lapply(seq_len(nrow(beta_seeds)), function(i) beta_seeds[i, ])
  }
  seeds <- logvar_joint_dedupe_vectors(beta_seeds)
  n_b <- nrow(grid_b)
  n_s <- length(seeds)
  b_idx <- rep(seq_len(n_b), times = n_s)
  s_idx <- rep(seq_len(n_s), each = n_b)
  b_block <- grid_b[b_idx, , drop = FALSE]
  b_fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields$mean
  stopifnot(length(b_fields) == ncol(grid_b))
  colnames(b_block) <- b_fields
  seed_full <- matrix(unlist(seeds), nrow = n_s, byrow = TRUE)
  seed_block <- seed_full[s_idx, , drop = FALSE]
  beta_fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields$variance
  stopifnot(length(beta_fields) == ncol(seed_block))
  colnames(seed_block) <- beta_fields
  data.frame(
    b_block, seed_block,
    seed_index = s_idx,
    row.names = NULL, check.names = FALSE
  )
}

# Deterministic separated-start selection: the best-scoring (smallest score)
# start in each observed pattern, capped at pattern_cap in observation order,
# then up to extra_cap extras farther than min_sep in normalized coordinates.
# Coverage is incomplete when more patterns exist than the cap admits.
logvar_joint_select_starts <- function(coords, pattern_ids, score,
                                       pattern_cap = logvar_joint_gmm_constants$pattern_start_cap,
                                       extra_cap =
                                         logvar_joint_gmm_constants$candidate_extra_start_cap,
                                       min_sep =
                                         logvar_joint_gmm_constants$candidate_min_separation) {
  coords <- as.matrix(coords)
  uniq_pat <- unique(pattern_ids)
  best_per_pattern <- unname(vapply(uniq_pat, function(p) {
    rows <- which(pattern_ids == p)
    rows[which.min(score[rows])]
  }, integer(1)))
  coverage_status <- if (length(uniq_pat) > pattern_cap) {
    "incomplete"
  } else {
    "complete"
  }
  keep <- seq_len(min(length(best_per_pattern), pattern_cap))
  retained <- best_per_pattern[keep]
  remaining <- setdiff(seq_along(pattern_ids), retained)
  remaining <- remaining[order(score[remaining], remaining)]
  extras <- integer(0)
  for (cand in remaining) {
    if (length(extras) >= extra_cap) break
    ref <- c(retained, extras)
    d <- vapply(ref, function(j) {
      sqrt(sum((coords[cand, ] - coords[j, ])^2))
    }, numeric(1))
    if (all(d >= min_sep)) extras <- c(extras, cand)
  }
  list(indices = c(retained, extras), coverage_status = coverage_status)
}
