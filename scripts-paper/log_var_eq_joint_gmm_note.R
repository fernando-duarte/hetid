# The marker-wrapped panels-table note (LaTeX comments) for the joint moment-
# compatibility diagnostic (Plan 4, logvar-joint-gmm). Mirrors the joint-null
# note: the byte-identity gate excludes exactly this block and the per-tau numbers
# live in the CSV and RDS artifacts. Appended by log_var_eq_ppml_table.R, which
# reaches this builder through log_var_eq_joint_gmm_run.R. The wording carries no
# set or rejection claim: a residualized-z block would report components of the
# Lewbel instrument orthogonal to the static variance regressors, and the
# generated-regressor provenance is stated for the deferred inference round.
logvar_joint_gmm_panel_note <- function(joint_gmm) {
  repl <- joint_gmm$graph_replication$replication_status
  c(
    "% BEGIN LOGVAR JOINT GMM NOTE",
    sprintf(
      paste(
        "%% Joint moment-compatibility (joint_gmm): just-identified graph",
        "replication %s -- the stacked moment layer reproduces the benchmark",
        "two-step map, a software-invariance check, not new identification."
      ),
      repl
    ),
    paste(
      "% Both substantive gates sit at the no-answer default, so no",
      "overidentified block runs; a ratified residualized-z block would report",
      "components of the Lewbel instrument orthogonal to the static variance",
      "regressors."
    ),
    paste(
      "% z and the mean-equation systems are estimated inputs (generated",
      "regressors); inference is deferred. Per-tau numbers live in the CSV and",
      "RDS artifacts."
    ),
    "% END LOGVAR JOINT GMM NOTE"
  )
}
