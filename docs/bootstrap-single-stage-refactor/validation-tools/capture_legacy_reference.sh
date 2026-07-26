#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
legacy_root=${HETID_LEGACY_ROOT:-/private/tmp/hetid-fresh-pipeline-run-20260722}
default_record=$repo_root/docs/bootstrap-single-stage-refactor
default_record=$default_record/baseline-artifacts/fresh-legacy-scientific-record.rds
record_path=${HETID_LEGACY_REFERENCE_RDS:-$default_record}
output_root=$legacy_root/scripts-paper/output

Rscript \
  "$repo_root/docs/bootstrap-single-stage-refactor/validation-tools/capture_table_record.R" \
  "$output_root" \
  "$record_path"
