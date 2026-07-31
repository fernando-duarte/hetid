# T4, volatility half. The mean gate already exercises every shared post-draw
# module, so what is left to establish here is that splitting
# set_bootstrap_builders.R and moving the target modules did not disturb the
# volatility assembly.
#
# Two columns are out of scope by construction: tau0_se_analytic and tau0_ratio
# depend on the full-sample PPML/Harvey analytic standard errors, which come
# from standard_error_estimators.R -- untouched by this refactor and expensive
# to reproduce. Everything else in the artifact is a pure function of the cache
# and is compared.
setwd("/Users/fduarte/hetid-worktrees/unified-inference")
BASE <- file.path(
  "/private/tmp/claude-502",
  "-Users-fduarte-Library-CloudStorage-Dropbox-Personal-MyPackages-hetid",
  "511ff928-e438-4f98-a805-111cac88d6d2/scratchpad/baseline"
)
source(normalizePath(file.path("scripts-paper", "config", "paths.R")))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "status_contract.R"))
paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "inference_post", "endpoint_target_cells.R"))
paper_source_once(paper_path("support", "inference_post", "endpoint_point_statistic.R"))
paper_source_once(paper_path("support", "inference_post", "logvar_point_summaries.R"))
paper_source_once(paper_path("support", "inference", "bootstrap_stage_logvar_contract.R"))
paper_source_once(paper_path("support", "inference", "bootstrap_stage_result_helpers.R"))
paper_source_once(paper_path("log_variance", "inference", "set_envelope.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))

d <- readRDS(file.path(BASE, "bootstrap_stage_draws.rds"))
ids <- c("ppml", "harvey")
coefs <- c(
  PAPER_ANALYSIS_CONTRACT$model$intercept_col,
  PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
)
spec <- list(coefs = coefs, estimator_ids = ids)
stage_spec <- list(tau = list(
  display = PAPER_ANALYSIS_CONTRACT$tau$display,
  union = c(0, PAPER_ANALYSIS_CONTRACT$tau$display)
))
layout <- bootstrap_stage_display_layout(stage_spec)
alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha

full <- bootstrap_stage_anchor_frames(d$anchor, spec)
prim <- bootstrap_stage_envelopes(d$volatility_primary, full, ids, layout, alpha)
sens <- bootstrap_stage_envelopes(d$volatility_sensitivity, full, ids, layout, alpha)
point_t <- logvar_boot_point_t(ids, d$volatility_primary, d$anchor, spec, layout$tau0_slot)
sim <- lapply(ids, function(id) {
  stats::setNames(vapply(seq_along(layout$taus), function(i) {
    logvar_simultaneous_critical(
      d$volatility_primary[[id]][[layout$slots[[i]]]],
      full[[id]][[layout$slots[[i]]]], alpha = alpha
    )
  }, numeric(1)), layout$keys)
})
names(sim) <- ids

rows <- list()
for (est in ids) {
  for (j in seq_along(layout$taus)) {
    key <- layout$keys[j]
    pub <- full[[est]][[layout$slots[[j]]]]
    rows[[length(rows) + 1L]] <- data.frame(
      estimator = est, tau = layout$taus[j], prim[[est]][[key]],
      c_sim = sim[[est]][[key]],
      anchor_lower = pub$set_lower, anchor_upper = pub$set_upper,
      sens_ci_lower = sens[[est]][[key]]$ci_lower,
      sens_ci_upper = sens[[est]][[key]]$ci_upper,
      point_estimate = point_t[[est]]$point,
      point_se = point_t[[est]]$se,
      point_statistic = point_t[[est]]$statistic,
      point_p_value = point_t[[est]]$p_value,
      row.names = NULL, stringsAsFactors = FALSE
    )
  }
}
fresh <- do.call(rbind, rows)

ref <- utils::read.csv(
  file.path(BASE, "diagnostics", "log_var_eq_set_inference_diagnostics.csv"),
  stringsAsFactors = FALSE
)
key <- function(d) paste(d$estimator, d$coef, sprintf("%.10g", d$tau))
ref <- ref[match(key(fresh), key(ref)), , drop = FALSE]
stopifnot(!anyNA(ref$coef))

shared <- setdiff(
  intersect(names(fresh), names(ref)),
  c("tau0_se_analytic", "tau0_ratio")
)
num <- shared[vapply(shared, function(c) is.numeric(fresh[[c]]), logical(1))]
chr <- setdiff(shared, num)
cat(sprintf("rows %d, comparing %d numeric and %d non-numeric columns\n\n",
            nrow(fresh), length(num), length(chr)))

worst <- 0
for (cc in num) {
  a <- as.numeric(fresh[[cc]]); b <- as.numeric(ref[[cc]])
  na_ok <- identical(is.na(a), is.na(b))
  dd <- suppressWarnings(max(abs(a - b), na.rm = TRUE))
  if (!is.finite(dd)) dd <- 0
  worst <- max(worst, dd)
  if (dd > 1e-12 || !na_ok) {
    cat(sprintf("  %-22s %.3e%s\n", cc, dd, if (!na_ok) "  NA PATTERN DIFFERS" else "  MOVED"))
  }
}
bad <- chr[!vapply(chr, function(cc) {
  identical(as.character(fresh[[cc]]), as.character(ref[[cc]]))
}, logical(1))]

cat(sprintf("\nworst numeric drift: %.3e\n", worst))
cat(sprintf("non-numeric mismatches: %s\n",
            if (length(bad)) paste(bad, collapse = ", ") else "none"))
cat("\n################ GATE VERDICT ################\n")
cat(sprintf("volatility panel post-draw equivalence: %s\n",
            if (worst <= 1e-12 && !length(bad)) "PASS" else "FAIL"))
