# Paper-owned structural inference: settings, prepared inputs, the reference
# column, the full-sample fit and its circular-block bootstrap, and the cache
# that holds them. The date-keyed input frames (gr1_pcecc96, the SDF and return
# PCs, the instrument) come from data_preparation/ and are read only when
# paper_structural_inference_prepare() runs with its defaults.

paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis.R"))
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "artifacts", "typed_artifacts.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "latex", "dropbox_ignore.R"))
paper_source_once(paper_path("log_variance", "engine", "contracts.R"))
paper_source_once(paper_path("support", "structural_inference", "failures.R"))
paper_source_once(paper_path("support", "structural_inference", "settings.R"))
paper_source_once(paper_path("support", "structural_inference", "inputs.R"))
paper_source_once(paper_path("support", "structural_inference", "arrays.R"))
paper_source_once(paper_path("support", "structural_inference", "positive.R"))
paper_source_once(paper_path("support", "structural_inference", "fit.R"))
paper_source_once(paper_path("support", "structural_inference", "reference.R"))
paper_source_once(paper_path("support", "structural_inference", "calibration.R"))
paper_source_once(paper_path("support", "structural_inference", "bootstrap.R"))
paper_source_once(paper_path("support", "structural_inference", "identity.R"))
paper_source_once(paper_path("support", "structural_inference", "cache_validate.R"))
paper_source_once(paper_path("support", "structural_inference", "cache.R"))
paper_source_once(paper_path("support", "structural_inference", "run.R"))
