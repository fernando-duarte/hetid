# Baseline artifacts

Updated: 2026-07-26 09:47 EDT

- Legacy source: `/private/tmp/hetid-fresh-pipeline-run-20260722`
- Retained replay:
  `.worktrees/single-stage-bootstrap-1/docs/bootstrap-single-stage-refactor/baseline-artifacts/baseline-a-replay`
- Legacy branch: `fresh-pipeline-run-20260722`
- Legacy commit: `18fc270c3e2035b9d3699739db6e436bbf646236`
- Completed bootstrap draws: 10,000 primary and 10,000 sensitivity
- Output inventory: `fresh-output-inventory.csv` (61 files)
- Primary schedule SHA-256:
  `5701196733f917351bcb22d111bff0d01ed8638daabf18df7561b5857b2a225e`
- Doubled-block schedule SHA-256:
  `d2550c39daa842fb3b14b19909bbfab1e26e4f3446c44bd80ce7874baab180ce`

The historical `fresh-legacy-scientific-record.rds` is schema 2. It contains
only the normalized numeric result-cell projections of all 23 final TeX tables:

- Tables: 23
- Numeric tokens: 2,282
- Record size: 18 KiB
- Record SHA-256:
  `ce51b33e95cbfd1e013279bf83b5b37f7929411f8538e9a619edd851fb8b5f26`

The RDS checksum is immutable historical evidence for the local R build that
wrote it. Schema 2 lacks significance stars and is not accepted by the current
cross-run gate. Do not overwrite this RDS.

For new acceptance, recapture a schema-3 reference from these retained TeX
tables with `scripts-paper/validation/capture_table_record.R`, then run the
single-pass clean validator with that explicit reference.

The projection came directly from the retained replay at commit `18fc270`.
Every one of its 23 TeX files matches `fresh-output-inventory.csv`
byte-for-byte. No bootstrap draw executed during schema-2 capture.

The previous schema-1 record is preserved as
`fresh-legacy-scientific-record-schema1.rds`:

- Archived size: 19 MiB
- Archived SHA-256:
  `98f85b32094a5d2f5f59d2e5c2a667b283cf35494685829aaa9e731c5e3e66db`
- Verified mean point draws: 10,000
- Verified primary volatility transport failures: 0
- Verified sensitivity volatility transport failures: 0
