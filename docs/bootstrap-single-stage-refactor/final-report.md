# Single-stage bootstrap refactor final report

Updated: 2026-07-26 19:37 EDT

## Status

Implementation, focused validation, small-run validation, baseline capture,
preflight work, the historical local Mac full bootstrap run, the table-only
acceptance redesign, and review follow-up are complete. The historical
single-stage branch was already incorporated into local `main` and
`origin/main`. The acceptance redesign and Task 5 follow-up did not run another
10,000-draw bootstrap.

Current acceptance directly compares numeric tokens and attached stars in the
final TeX tables under two existing output roots. It does not serialize a
reference, stage a clean run, execute the pipeline, or inspect intermediate
artifacts. The retained schema records and earlier rerun/reuse comparisons are
immutable historical evidence, not current acceptance inputs.

## Historical isolation record

- Base commit: `18fc270c3e2035b9d3699739db6e436bbf646236`
- Branch: `refactor/single-stage-bootstrap-4`
- Worktree:
  `/Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid/.worktrees/single-stage-bootstrap-4`
- Current commit: `9f1280c44ce8886a2af69710c1f9c3b3b37b91a2`
- Tracked worktree status at the historical handoff: clean
- Versioned `scripts-paper/output` diff from the base: the LAD fragment and
  standalone TeX table only, both changed solely to two-decimal numeric cells
- The branch and `origin/refactor/single-stage-bootstrap-4` were synchronized
  at `9f1280c44ce8886a2af69710c1f9c3b3b37b91a2` at handoff.
- Local ancestry checks on 2026-07-26 show that `9f1280c` is an ancestor of
  both `main` and `origin/main`; the historical work is already merged.

## Resulting architecture

- One late bootstrap stage owns primary and sensitivity resampling.
- One primary index family is generated once and consumed by both mean and
  volatility inference.
- One doubled-block sensitivity family is generated once.
- Each primary draw calls `estimate_set_id_system()` once and shares its
  estimate and geometry across both branches.
- One transactional cache, `bootstrap_stage_draws.rds`, owns both collections
  and canonical provenance.
- Cache hits reconstruct both existing public result objects without executing
  a draw callback.
- Legacy mean and volatility orchestration runners and their two caches are
  retired.
- macOS defaults to 14 bootstrap workers on the current 16-logical-CPU machine.
- Cross-platform acceptance compares normalized numeric result tokens and
  their attached stars in every final TeX table. Tokens pass when their
  displayed rounding intervals overlap; paths, numeric coordinates, and token
  counts must also match.

## Commits

- `6b948fd` test: characterize legacy bootstrap behavior
- `5f2b177` Add canonical MBB index family API
- `ea0e832` Harden MBB family and preserve wrapper semantics
- `724b7cf` Reserve two macOS cores for bootstrap runs
- `3c6dc2f` Add shared bootstrap specification and draw stage
- `fff89cb` Add unified bootstrap cache and runner
- `62b4b63` Integrate unified bootstrap pipeline stage
- `2e8aaf9` Harden unified bootstrap validation
- `9da71d7` Close bootstrap provenance edge cases
- `1065c2e` Test namespace runtime fingerprint binding
- `441319a` Harden bootstrap preflight validation
- `770b258` Make validation driver R-version independent
- `f021d8e` Rebind platform-sensitive gate fields
- `f3604e2` Use two-decimal precision for LAD tables
- `fa1130d` Gate acceptance on displayed table numbers
- `2d23b64` Regenerate LAD tables at two decimals
- `72bb36a` Harden displayed-precision boundary checks
- `b87238a` Normalize published scientific table tokens
- `1462130` Validate table records before bootstrap runs
- `a9d9fb7` Make table record validation canonical
- `fa1d87d` Reject malformed table record columns
- `423bb16` Capture legacy table reference without bootstrap
- `9f1280c` Isolate legacy table capture from R profiles

Commit range:
`18fc270c3e2035b9d3699739db6e436bbf646236..9f1280c44ce8886a2af69710c1f9c3b3b37b91a2`

## Current direct-comparison validation

- Direct table acceptance suite: passed.
- Mutation matrix: accepted prose, nonnumeric-only table, and non-table changes;
  rejected displayed-value, star, numeric-coordinate, and token-count changes.
- Semantic SSOT scan: passed across active R functions outside
  `scripts-paper/validation/`; renamed parser, quantum, and rounding duplicate
  fixtures were detected.
- Topology and contract ownership: passed.
- Paper test runner: all 34 suites and structural checks passed.

## Historical validation completed

- Full paper harness: all 34 suites and structural checks passed.
- Historical topology: 364 R files and 64 artifacts passed.
- Focused Harvey suite: 81 of 81 checks passed.
- Focused set-bootstrap suite: 48 of 48 checks passed.
- Focused cache suite: 7 of 7 checks passed.
- Current-code real small-run roundtrip: two primary and two sensitivity draws,
  one rerun callback, zero reuse callbacks.
- Earlier eight-draw full integration: rerun and reuse public objects were
  `identical()` and the cache SHA did not change.
- Package tests: 1,712 passed; no failures, warnings, or skips.
- Package lint: no lints.
- Package check: 0 errors, 1 pre-existing data-compression warning, 0 notes.
- Comprehensive quality suite: 10 of 11 tools passed; the remaining `pkgcheck`
  result is an environment/network failure recorded as BSF-170.
- Commit hooks: passed for every implementation commit.
- Full-file pre-commit run: all hooks except the known unrelated first-pass
  style rewrite passed; BSF-163 records the unchanged source and disposition.
- Source parsing, 100-column limit, and 200-line R-file limit: passed.
- Versioned published outputs: only the two LAD TeX artifacts changed, with
  finite LAD cells rendered at two decimals.
- Historical table projection: all 22 then-versioned TeX tables and 2,120
  normalized numeric tokens.
- LAD suite: 11 passed; Harvey: 81 passed; PPML: 103 passed.
- Table-only comparator tests cover ordinary and scientific notation,
  cross-precision overlap, adjacent displayed digits, status/structure
  exclusions, and table-path mismatches.
- Historical schema-2 record validation passed from three working directories and rejected
  malformed paths, coordinates, cells, values, quanta, and matrix columns.
- A malformed Mac preflight exited before `rsync` with zero source files staged.
- Mac full candidate execution: 10,000 primary and 10,000 doubled-block
  sensitivity draws completed with 14 workers; supervisor exit status was 0.
- Mac unified cache promotion: passed; cache artifact is
  `scripts-paper/output/state/bootstrap_stage_draws.rds` in the retained run
  root `/tmp/hetid-mac-validation-20260724-rerun.GsjuzA`.
- Mac strict cache reuse: passed with zero draw callbacks, writing both
  candidate scientific records.
- Historical Mac rerun-to-reuse and candidate-to-legacy scientific
  comparisons passed under retired contracts. That two-pass evidence validates
  the unified bootstrap implementation but is not the current cross-platform
  publication acceptance rule.
- Mac supervisor interval: 2026-07-24 11:23:42 EDT to 18:14:31 EDT.

## Legacy reference

The completed non-refactored 10,000-draw caches in
`/private/tmp/hetid-fresh-pipeline-run-20260722` were captured through one
cache-reuse pipeline pass. Both legacy bootstrap stages reported reuse and
executed zero new draws.

- Archived historical schema-1 scientific record:
  `baseline-artifacts/fresh-legacy-scientific-record-schema1.rds`
- Archived record SHA-256:
  `98f85b32094a5d2f5f59d2e5c2a667b283cf35494685829aaa9e731c5e3e66db`
- Output inventory: `baseline-artifacts/fresh-output-inventory.csv`
- Inventory size: 61 files
- Mean point draws: 10,000
- Primary volatility transport failures: 0
- Sensitivity volatility transport failures: 0
- Primary schedule SHA-256:
  `5701196733f917351bcb22d111bff0d01ed8638daabf18df7561b5857b2a225e`
- Sensitivity schedule SHA-256:
  `d2550c39daa842fb3b14b19909bbfab1e26e4f3446c44bd80ce7874baab180ce`

The historical schema-2 reference remains:

- Record: `baseline-artifacts/fresh-legacy-scientific-record.rds`
- Tables: 23
- Normalized numeric tokens: 2,282
- SHA-256:
  `ce51b33e95cbfd1e013279bf83b5b37f7929411f8538e9a619edd851fb8b5f26`

This byte checksum is scoped to the local R serializer. It is historical
evidence only. Current cross-platform acceptance reads displayed numeric tokens
and attached stars directly from the two existing TeX output trees.

It was projected directly from the retained legacy TeX outputs. All 23 match
the pinned fresh-run inventory byte-for-byte. The previous 19 MiB schema-1
record remains at
`baseline-artifacts/fresh-legacy-scientific-record-schema1.rds` with its
original SHA-256. No bootstrap draw was used for schema-2 capture.

## Historical paired review

Claude and Codex independently reviewed the final table-acceptance changes from
read-only scratch copies. Their findings drove fixes for floating-point
boundaries, scientific notation, line limits, and malformed-record preflight
handling. Their final pass found no remaining critical, high, or medium
finding at that time. The later Task 5 review identified compatibility and
documentation follow-up; the current validation section records those fixes.
Continuation IDs and summaries are retained under `consultations/`.

## Findings

Twenty-eight findings are recorded as BSF-157 through BSF-184.

- All five original integration findings, BSF-157 through BSF-161, are fixed.
- The high- and medium-severity cache, provenance, ownership, comparator, and
  preflight defects BSF-165 through BSF-169 and BSF-172 through BSF-173 are
  fixed.
- BSF-164 records the contained concurrent writer in an abandoned worktree.
- BSF-162, BSF-163, and BSF-170 are low-severity pre-existing environment or
  unrelated tooling observations and remain documented without scope expansion.
- BSF-171 records an R-version-dependent serialized gate sample identity. The
  ignored validation driver handles it only after the scientific gate records
  agree under its internal gate comparison; production code is unchanged.
- BSF-178 through BSF-182 record and resolve the displayed-boundary,
  scientific-notation, schema-preflight, canonical-record, and matrix-column
  defects found during final review.
- BSF-183 records and resolves the legacy capture helper's unsafe rerun fallback.
- BSF-184 records and resolves R startup-profile execution during capture.

## Merge status

- The historical single-stage branch is already contained in local `main` and
  `origin/main`, verified with local ancestry checks.
- Current table-only acceptance work remains isolated on its feature branch;
  this report does not authorize an additional merge.

Oscar execution is out of scope for the current workflow and is not a merge
criterion.
