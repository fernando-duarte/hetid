# Offline tri-state gate-reader checks for the quantreg dependency decision
# dependency gate. Quantreg-free by construction: package
# availability and the installed version are injected so every gate state is
# reachable on any machine and the reader is never allowed to read package
# presence as approval. This file never skips. Run from the worktree root:
#   Rscript scripts-paper/tests/estimators/lad/test_dependency_gate.R
#
# Interface assumption surfaced for the implementer (File structure names the
# module dependency_gate.R; the reader name is the plausible
# logvar_lad_gate_read): the reader is called as
#   logvar_lad_gate_read(path, available, installed_version)
# and returns a list with $decision ("unanswered"/"declined"/"approved"),
# $ready, and $source_lad, raising a classed error on a malformed record, a
# schema/gate mismatch, a duplicate field, an approved-but-missing package, or a
# stale recorded version. A split reader/runtime-contract naming may differ;
# adjust read_gate() if so.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "dependency_gate.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
# Fail a check closed when the required reader errors or is absent.
gate_try <- .test$safe

# Write a DCF record (one key: value per line) to a fresh temp path.
gate_dcf <- function(lines) {
  path <- tempfile(fileext = ".dcf")
  writeLines(lines, path)
  path
}
# The approved-and-ready baseline record; individual checks override fields.
approved_lines <- c(
  "schema_version: 1.0.0",
  "gate: logvar-lad-quantreg",
  "decision: approved",
  "decided_at: 2026-07-14T13:45:40-0400",
  "rationale: user approved the analysis-layer dependency",
  "install_consent: approved",
  "quantreg_version: 6.1"
)
# Injected-availability wrapper so no check depends on quantreg being present.
read_gate <- function(path, available = TRUE, version = "6.1") {
  logvar_lad_gate_read(path, available = available, installed_version = version)
}
# Assert that a reader call rejects (raises) the given record, guarding first on
# the reader existing so an absent module fails rather than spuriously passes.
rejects <- function(lines, available = TRUE, version = "6.1") {
  stopifnot(exists("logvar_lad_gate_read"))
  identical(
    tryCatch(
      {
        read_gate(gate_dcf(lines), available = available, version = version)
        "ok"
      },
      error = function(e) "stopped"
    ),
    "stopped"
  )
}

# The production decision is tracked, schema-valid, and authorizes its version.
tracked_gate_path <- paper_path("config", "decisions", "lad.dcf")
tracked_version <- read.dcf(
  tracked_gate_path,
  fields = "quantreg_version"
)[1L, 1L]
check("tracked LAD decision authorizes its recorded version", gate_try({
  gate <- read_gate(tracked_gate_path, version = tracked_version)
  identical(gate$decision, "approved") && isTRUE(gate$source_lad)
}))

# A missing file reads as unanswered, never an error, and sources no LAD code.
check("gate a missing DCF reads as unanswered", gate_try({
  stopifnot(exists("logvar_lad_gate_read"))
  g <- read_gate(file.path(tempdir(), "no_such_gate_file.dcf"))
  identical(g$decision, "unanswered") && !isTRUE(g$source_lad)
}))

# A declined decision sources no LAD code and leaves the pipeline byte-identical.
check("gate a declined DCF sources no LAD code", gate_try({
  stopifnot(exists("logvar_lad_gate_read"))
  p <- gate_dcf(c(
    "schema_version: 1.0.0", "gate: logvar-lad-quantreg",
    "decision: declined", "decided_at: 2026-07-14T13:45:40-0400",
    "rationale: declined", "install_consent: not_asked", "quantreg_version:"
  ))
  g <- read_gate(p, available = FALSE, version = NULL)
  identical(g$decision, "declined") && !isTRUE(g$source_lad)
}))

# Approved with an available, version-matching package authorizes sourcing.
check("gate an approved ready DCF authorizes sourcing", gate_try({
  stopifnot(exists("logvar_lad_gate_read"))
  g <- read_gate(gate_dcf(approved_lines), available = TRUE, version = "6.1")
  identical(g$decision, "approved") && isTRUE(g$ready) && isTRUE(g$source_lad)
}))

# Approved but missing is a hard stop with install guidance, never a skip/stub.
check("gate an approved but missing package hard-fails", gate_try({
  stopifnot(exists("logvar_lad_gate_read"))
  out <- tryCatch(
    {
      read_gate(gate_dcf(approved_lines), available = FALSE, version = NULL)
      "no_stop"
    },
    error = function(e) "stopped"
  )
  identical(out, "stopped")
}))

# Package availability never stands in for approval: unanswered stays unanswered.
check("gate package availability never implies approval", gate_try({
  stopifnot(exists("logvar_lad_gate_read"))
  p <- gate_dcf(c(
    "schema_version: 1.0.0", "gate: logvar-lad-quantreg",
    "decision: unanswered", "decided_at:", "rationale:",
    "install_consent: not_asked", "quantreg_version:"
  ))
  g <- read_gate(p, available = TRUE, version = "6.1")
  identical(g$decision, "unanswered") && !isTRUE(g$source_lad)
}))

# A decision value outside the closed enum is rejected.
check("gate a malformed decision value is rejected", gate_try(rejects(c(
  "schema_version: 1.0.0", "gate: logvar-lad-quantreg",
  "decision: maybe", "decided_at:", "rationale:",
  "install_consent: not_asked", "quantreg_version:"
))))

# A duplicated field is ambiguous and rejected, never last-value-wins.
check(
  "gate a duplicate field is rejected",
  gate_try(rejects(c(approved_lines, "decision: declined")))
)

# A schema or gate identifier mismatch is rejected outright.
check("gate a schema or gate mismatch is rejected", gate_try(rejects(c(
  "schema_version: 1.0.0", "gate: some-other-gate",
  "decision: approved", "decided_at:", "rationale:",
  "install_consent: approved", "quantreg_version: 6.1"
))))

# A stale recorded version against the installed one is rejected, not rewritten.
check(
  "gate a stale recorded version is rejected",
  gate_try(rejects(approved_lines, available = TRUE, version = "6.0"))
)

# Headless and deterministic: repeated reads never prompt and never diverge.
check("gate reads are deterministic and non-interactive", gate_try({
  stopifnot(exists("logvar_lad_gate_read"))
  p <- gate_dcf(approved_lines)
  identical(read_gate(p), read_gate(p))
}))

.test$finish()
