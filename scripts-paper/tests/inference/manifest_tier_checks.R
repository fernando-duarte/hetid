# The draw/presentation manifest split is only worth having if it actually
# behaves: editing post-draw code must leave the draw hash alone so a ten-
# thousand-draw cache survives, and editing draw code must still invalidate it.
# Both directions matter. A tier that never invalidates is worse than no tier,
# because it silently serves draws that current code would not reproduce.
# Run from root:
#   Rscript scripts-paper/tests/inference/manifest_tier_checks.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_cache_validation.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_code_manifest.R"
))
paper_source_once(paper_path("support", "statistics", "boot_freshness.R"))
paper_source_once(paper_path("inference", "bootstrap_stage_cache.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

mt_draw <- function() paper_boot_code_sha(bootstrap_stage_code_manifest())
mt_pres <- function() paper_boot_code_sha(bootstrap_stage_presentation_manifest())

mt_draw0 <- mt_draw()
mt_pres0 <- mt_pres()

check(
  "the two manifests are disjoint",
  length(intersect(
    bootstrap_stage_code_manifest(),
    bootstrap_stage_presentation_manifest()
  )) == 0L
)
check(
  "both manifests are non-empty",
  length(bootstrap_stage_code_manifest()) > 0L &&
    length(bootstrap_stage_presentation_manifest()) > 0L
)

# Append a comment, hash, restore. The file is rewritten from its own lines, so
# a failure between the two writes leaves it intact apart from one probe line.
mt_probe <- function(path) {
  before <- readLines(path)
  on.exit(writeLines(before, path), add = TRUE)
  writeLines(c(before, "# manifest tier probe"), path)
  c(draw = mt_draw(), presentation = mt_pres())
}

mt_after_pres <- mt_probe(
  paper_path("support", "inference_post", "endpoint_targets.R")
)
check(
  "editing post-draw code moves the presentation hash and not the draw hash",
  identical(unname(mt_after_pres[["draw"]]), mt_draw0) &&
    !identical(unname(mt_after_pres[["presentation"]]), mt_pres0)
)

mt_after_draw <- mt_probe(
  paper_path("support", "identification", "identified_set_bootstrap.R")
)
check(
  "editing draw code moves the draw hash and not the presentation hash",
  !identical(unname(mt_after_draw[["draw"]]), mt_draw0) &&
    identical(unname(mt_after_draw[["presentation"]]), mt_pres0)
)

check(
  "both probes restored the tree",
  identical(mt_draw(), mt_draw0) && identical(mt_pres(), mt_pres0)
)

# presentation_sha must be recorded and format-checked, but never compared:
# it is absent from the semantic list precisely so a table edit cannot discard
# draws. If it ever migrates into a recomputation there, this fails.
check(
  "presentation_sha is a recorded provenance field and a checked hash",
  "presentation_sha" %in% BOOTSTRAP_STAGE_PROVENANCE_FIELDS &&
    "presentation_sha" %in% BOOTSTRAP_STAGE_SHA_FIELDS
)

.test$finish()
