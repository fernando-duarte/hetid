# Single ordered source of the joint moment-compatibility (joint_gmm) stack, so
# run_pipeline.R gains exactly one line (the benchmark map-module pattern). The
# committed decision record and the replication/search modules are siblings sourced in
# dependency order; the specification module self-sources the artifacts, projection, and
# guarded pipeline driver, and that driver runs at source time when the pipeline
# objects exist. Runs after the joint-null driver and before the panels table.
source(paper_path("log_variance", "diagnostics", "joint_gmm", "decision_schema.R"))
source(paper_path("config", "decisions", "joint_gmm.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "moments.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "basis.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "profiles.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "identity.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "budget.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "candidates.R"))
source(paper_path("log_variance", "diagnostics", "joint_gmm", "epigraph.R"))
source(paper_path(
  "log_variance", "diagnostics", "joint_gmm", "specification_and_replication.R"
))
