# The committed joint moment-compatibility decision record (joint-GMM,
# logvar-joint-gmm). This is the checked-in NO-ANSWER DEFAULT: the dossier's
# ratification items were presented in the startup decision packet and left at
# their conservative defaults, so every optional scientific
# switch is FALSE, the intercept target is a_L, and the moment_delta grid is empty.
# A noninteractive clean checkout therefore runs the moment-specification and
# graph-replication invariance checks -- the dossier's minimum useful product -- without
# mutating any configuration at runtime. If item-specific answers are ever ratified,
# replace this call with the explicit choices and set decision_provenance to
# "user_ratified" before committing; never edit the record at runtime.
#
# The pure spec-ID/default/validation helpers live in decision_schema.R,
# sourced here and by the tests before the record is built. The driver validates
# this record (exact schema, branch consistency, fresh spec ID) before any moment
# evaluation. decided_on is a fixed ISO date, not Sys.Date().

paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "decision_schema.R"))

logvar_joint_gmm_decision <- logvar_joint_decision_default("2026-07-14")
