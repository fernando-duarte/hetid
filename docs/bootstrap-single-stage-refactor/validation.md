# Single-stage bootstrap refactor validation

Updated: 2026-07-26 10:06 EDT

## Current cross-run acceptance

Current acceptance requires an explicit schema-3 reference recaptured from the
retained TeX tables. The candidate starts from an empty staged output tree,
runs the pipeline once, and is accepted only when table paths, numeric
coordinates, token counts, displayed values, and attached stars agree. The
retained schema-2 record and the earlier rerun/reuse comparisons are immutable
historical evidence, not current acceptance inputs.

## Current Task 5 validation

- Schema-3 table acceptance suite: passed.
- Semantic SSOT scan: passed across active R functions outside
  `scripts-paper/validation/`; renamed duplicate fixture detected.
- Compatibility comparator: passed from a temporary non-repository working
  directory.
- Legacy-output capture helper: requires an explicit schema-3 destination,
  rejects the protected historical schema-2 path, and preserves that path
  when either safety check fails.
- Mac compatibility wrapper: passed with the fixture pipeline only.
- Topology: 381 R files and 64 artifacts passed.

## Historical implementation evidence

- Focused unified stage, cache, pipeline, set-bootstrap, mean-result, Harvey,
  statistics, topology, and ownership tests.
- Paper test runner: all 34 suites and structural checks.
- Current-code real cache roundtrip: two primary and two sensitivity draws,
  one rerun callback, and zero reuse callbacks.
- Earlier full small-run integration: eight primary and eight sensitivity draws;
  candidate and reuse public objects were `identical()` and the cache SHA was
  unchanged.
- Historical legacy scientific reference: captured from completed 10,000-draw caches with
  both bootstrap runners in reuse mode and zero new draws.
- Historical legacy reference self-comparison: passed its former scientific rule.
- Legacy output inventory: 61 generated files.
- Package tests: 1,712 passed, 0 failed, 0 warned, 0 skipped.
- Package lint: no lints.
- `devtools::check()`: 0 errors, 1 warning, 0 notes.
- Comprehensive quality script: 10 of 11 tools passed; the remaining `pkgcheck`
  failure is an environment/network defect documented in BSF-170.
- Commit-time hooks for every implementation commit.
- R source parsing, line length below 100 columns, and file length below 200 lines.
- Oscar dependency setup completed under R 4.6.0 without installing another R.
- Oscar readiness checks verified the clean branch and commit, package manifest,
  baseline hashes, ACM gzip integrity, 16,223-row data extraction, and parsing
  of all 359 paper R files.
- Oscar scheduler preflight accepted the 20-CPU, 64-GiB job. Job `4267010`
  subsequently started on `node2327` and initialized the rerun pipeline.
- The complete validation-tool regression passes under both the local R runtime
  and Oscar R 4.6.0 after commit `f021d8e`.
- Oscar readiness passed on a clean checkout at pushed commit `f021d8e`.
- Replacement job `4269186` passed the two previously failing validation points,
  completed the LAD stage, and spawned 20 fork workers for the unified
  bootstrap on `node2304`.
- The isolated Mac candidate completed one 10,000-draw primary family and one
  10,000-draw doubled-block sensitivity family with 14 workers. It exited 0;
  no draw or pipeline error was reported.
- The unified `bootstrap_stage_draws.rds` cache was promoted successfully.
  The strict reuse pass completed without executing draw callbacks.
- Historical candidate rerun and reuse records were written successfully, and
  both raw-object comparisons passed under the former `1e-4` rule. That rule is
  retired in favor of the table-only displayed-precision gate below.
- LAD render suite: 11 passed, including dedicated two-decimal cells and the
  retained three-decimal generic log-variance default.
- Harvey render and estimator suite: 81 passed.
- PPML render and estimator suite: 103 passed.
- Table-only comparator checks pass for ordinary decimals, negative values,
  `e` notation, LaTeX `\times 10^{...}` notation, cross-precision overlap,
  numeric-versus-status cells, structure changes, and missing-table failures.
- Historical table projection covered all 22 then-versioned TeX tables and 2,120
  normalized numeric tokens.
- Historical full paper harness after final hardening: all 34 suites and structural checks;
  topology covered 364 R files and 64 artifacts.
- Historical schema-2 validator tests reject unsupported and empty records, traversal,
  drive and backslash paths, malformed coordinates and cells, nonfinite values,
  zero quanta, and matrix-valued data-frame columns.
- The historical schema-2 test passes from the repository root, its own tool directory, and
  `/tmp`. A malformed candidate preflight invoked from `/tmp` exited 1 before
  `rsync`, with zero source files staged.
- Final paired Claude/Codex review: no remaining critical, high, or medium
  findings.
- Historical schema-2 reference capture: all 23 retained TeX tables match the
  pinned fresh-run inventory byte-for-byte; the record validates, self-compares,
  and is `identical()` to a fresh direct projection.
- Historical schema-2 reference: 23 tables, 2,282 numeric tokens, 18 KiB, SHA-256
  `ce51b33e95cbfd1e013279bf83b5b37f7929411f8538e9a619edd851fb8b5f26`.
- No bootstrap process remained after capture.
- The tables-only capture regression passes with a fixture that contains no
  pipeline or cache. Re-running the repaired helper on the retained 23-table
  baseline reproduced the exact schema-2 SHA-256 above.
- Hostile R startup-profile regression: passed under `Rscript --vanilla`.
- Final capture-only Claude/Codex review: no remaining critical, high, or
  medium findings.

## Qualified

- The `devtools::check()` warning concerns possible compression of the unchanged
  `data/variables.RData`; see BSF-162.
- `pre-commit run --all-files`: all hooks other than the first-pass style hook
  passed. The style hook rewrites an unchanged, unrelated figure source; the
  refactor leaves it untouched. See BSF-163.

## Historical completed execution

- Mac candidate run root:
  `/tmp/hetid-mac-validation-20260724-rerun.GsjuzA`.
- Supervisor interval: 2026-07-24 11:23:42 EDT to 18:14:31 EDT.
- Primary draw elapsed time: 188.4 minutes; sensitivity draw elapsed time:
  194.8 minutes.
- Historical candidate records: `candidate-rerun.rds` and
  `candidate-reuse.rds`; their two-pass comparison is not current acceptance.

## Historical merge status

- Commit `9f1280c44ce8886a2af69710c1f9c3b3b37b91a2` and the
  `refactor/single-stage-bootstrap-4` work were already incorporated into both
  local `main` and `origin/main`. Local `git merge-base --is-ancestor` checks
  returned exit status 0 for both targets on 2026-07-26.
- Keep Oscar execution out of scope for this workstream.

## Failed before bootstrap

- Oscar job `4267010` exited after 3 minutes 29 seconds. Its R 4.6.0 parse
  output wrapped the EGARCH route source call, defeating a fixed deparse-string
  match in the ignored capture driver. The intended tolerant gate rebind did not
  run, and the exact serialized gate hash stopped the pipeline. No bootstrap
  draw executed; see BSF-176.
- Oscar job `4268879` exited after 3 minutes 12 seconds. The tolerant gate
  comparison passed, but the validation driver left reference `gate_q` and
  `gate_p` values in the runtime decision after rebinding its hash. The strict
  route validator stopped on that inconsistency before any bootstrap draw; see
  BSF-177.
