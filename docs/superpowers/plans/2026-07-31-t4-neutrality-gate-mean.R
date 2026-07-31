# T4 NEUTRALITY GATE. Feed the frozen pre-refactor cache through the CURRENT
# post-draw layer and require it to reproduce the published numbers exactly.
# Phase I is supposed to be numerically inert; this is what establishes that,
# before any semantic change lands on top and makes the diff unattributable.
#
# Compares the full-precision diagnostics CSV, never the rounded .tex: .tex
# agreement at three decimals would hide a real change in the fourth.
# Deliberately bypasses schema validation -- the baseline cache is schema 2 and
# carries tau_star_draws / n_capped, which the current validator rejects by
# design. That bypass is exactly why this gate proves LESS than a full rerun:
# it establishes post-draw equivalence conditional on legacy primitives, and
# says nothing about the new draw producer, cache writing, or manifest tiering.
setwd("/Users/fduarte/hetid-worktrees/unified-inference")
BASE <- file.path(
  "/private/tmp/claude-502",
  "-Users-fduarte-Library-CloudStorage-Dropbox-Personal-MyPackages-hetid",
  "511ff928-e438-4f98-a805-111cac88d6d2/scratchpad/baseline"
)

source(normalizePath(file.path("scripts-paper", "config", "paths.R")))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis.R"))
paper_source_once(paper_path("support", "data", "acm_inputs.R"))
create_artifact_directories()
quarterly_acm_inputs <- paper_load_quarterly_acm(all_mats)
paper_source_once(paper_path("data_preparation", "fred_download_patch.R"))
for (s in c("build_sdf_series", "build_consumption_growth", "build_yield_volatility",
            "build_asset_return_pcs", "build_sdf_pcs")) {
  paper_source_once(paper_path("data_preparation", paste0(s, ".R")))
}
paper_source_once(paper_path("mean_equation", "fit_ols.R"))
paper_source_once(paper_path("mean_equation", "estimate_identified_set.R"))
paper_source_once(paper_path("mean_equation", "inference", "boot_results.R"))
paper_source_once(paper_path("support", "identification", "identified_set_bootstrap.R"))
paper_source_once(paper_path("support", "inference", "bootstrap_stage_mean_result_inputs.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))

cached <- readRDS(file.path(BASE, "bootstrap_stage_draws.rds"))
alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha

# strip the two fields the refactor removed; everything else is untouched
mean_collected <- cached$mean[setdiff(
  names(cached$mean), c("tau_star_draws", "n_capped")
)]
cat("legacy mean fields:", paste(names(cached$mean), collapse = ", "), "\n")
cat("passed to new layer:", paste(names(mean_collected), collapse = ", "), "\n\n")

prov <- cached$provenance[c(
  "resampler", "sample_size", "b_reps", "block", "seed", "rng_kind",
  "block_rule", "index_sha256"
)]
result <- mean_boot_results(mean_collected, set_id_mean_eq, alpha, prov)
projected <- bootstrap_stage_mean_result_inputs(set_id_mean_eq)
# the artifact is the metadata frame cbound to the diagnostics, exactly as
# bootstrap_stage_results.R:22 assembles it -- compare at that level, not one
# below it, or three writer-added columns read as a regression
fresh <- set_id_boot_diagnostics(
  result, result$inference, projected$set_tables,
  set_id_mean_eq$tau_display, result$point_t
)
fresh <- cbind(
  paper_inference_metadata_frame(nrow(fresh)), fresh,
  stringsAsFactors = FALSE
)

ref <- utils::read.csv(
  file.path(BASE, "diagnostics", "set_id_inference_diagnostics.csv"),
  stringsAsFactors = FALSE
)

cat("=== shape ===\n")
cat(sprintf("fresh: %d x %d   reference: %d x %d\n",
            nrow(fresh), ncol(fresh), nrow(ref), ncol(ref)))
key <- function(d) paste(d$coef, sprintf("%.10g", d$tau))
stopifnot(identical(sort(key(fresh)), sort(key(ref))))
ref <- ref[match(key(fresh), key(ref)), , drop = FALSE]

shared <- intersect(names(fresh), names(ref))
num <- shared[vapply(shared, function(c) is.numeric(fresh[[c]]), logical(1))]
chr <- setdiff(shared, num)
cat(sprintf("comparing %d numeric and %d non-numeric columns\n\n", length(num), length(chr)))

cat("=== NUMERIC COLUMNS: max |fresh - reference| ===\n")
worst <- 0
for (cc in num) {
  a <- as.numeric(fresh[[cc]]); b <- as.numeric(ref[[cc]])
  na_ok <- identical(is.na(a), is.na(b))
  d <- suppressWarnings(max(abs(a - b), na.rm = TRUE))
  if (!is.finite(d)) d <- 0
  worst <- max(worst, d)
  flag <- if (!na_ok) "  <-- NA PATTERN DIFFERS" else if (d > 1e-12) "  <-- MOVED" else ""
  if (d > 0 || !na_ok) cat(sprintf("  %-24s %.3e%s\n", cc, d, flag))
}
cat(sprintf("\nworst numeric drift across all columns: %.3e\n", worst))

cat("\n=== NON-NUMERIC COLUMNS ===\n")
bad <- character(0)
for (cc in chr) {
  if (!identical(as.character(fresh[[cc]]), as.character(ref[[cc]]))) {
    bad <- c(bad, cc)
    cat(sprintf("  %-24s DIFFERS\n", cc))
  }
}
if (!length(bad)) cat("  all identical\n")

cat("\n=== columns present on one side only ===\n")
only_fresh <- setdiff(names(fresh), names(ref))
only_ref <- setdiff(names(ref), names(fresh))
cat("only in fresh:", if (length(only_fresh)) paste(only_fresh, collapse = ", ") else "none", "\n")
cat("only in reference:", if (length(only_ref)) paste(only_ref, collapse = ", ") else "none", "\n")

cat("\n################ GATE VERDICT ################\n")
pass <- worst <= 1e-12 && !length(bad) && !length(only_ref)
cat(sprintf("mean panel post-draw equivalence: %s\n", if (pass) "PASS" else "FAIL"))
cat(sprintf("  worst numeric drift %.3e (threshold 1e-12)\n", worst))
cat(sprintf("  non-numeric mismatches: %d\n", length(bad)))
cat(sprintf("  reference columns lost: %d\n", length(only_ref)))
