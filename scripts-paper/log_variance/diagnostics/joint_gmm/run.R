# Single ordered source of the joint moment-compatibility (joint_gmm) stack, so
# run_pipeline.R gains exactly one line (the benchmark map-module pattern). The
# committed decision record and the replication/search modules are siblings sourced in
# dependency order; the specification module self-sources the artifacts, projection, and
# guarded pipeline driver, and that driver runs at source time when the pipeline
# objects exist. Runs after the joint-null driver and before the panels table.
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "decision_schema.R"))
paper_source_once(paper_path("config", "decisions", "joint_gmm.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "moments.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "basis.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "profiles.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "identity.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "budget.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "candidates.R"))
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "epigraph.R"))
paper_source_once(paper_path(
  "log_variance", "diagnostics", "joint_gmm", "specification_and_replication.R"
))
