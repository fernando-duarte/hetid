#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
if [ "$#" -ne 1 ]; then
  printf '%s\n' \
    "usage: capture_legacy_reference.sh destination-schema3.rds" >&2
  exit 64
fi

legacy_root=${HETID_LEGACY_ROOT:-/private/tmp/hetid-fresh-pipeline-run-20260722}
historical_root=$repo_root/docs/bootstrap-single-stage-refactor/baseline-artifacts
historical_record=$historical_root/fresh-legacy-scientific-record.rds
record_dir=$(dirname "$1")
mkdir -p "$record_dir"
record_dir=$(cd "$record_dir" && pwd -P)
record_path=$record_dir/$(basename "$1")
historical_root=$(cd "$historical_root" && pwd -P)
historical_record=$historical_root/fresh-legacy-scientific-record.rds
if [ "$record_path" = "$historical_record" ]; then
  printf '%s\n' \
    "historical schema-2 reference is immutable: $historical_record" >&2
  exit 64
fi
if [ -L "$record_path" ]; then
  printf '%s\n' "schema-3 destination must not be a symbolic link" >&2
  exit 64
fi
output_root=$legacy_root/scripts-paper/output

Rscript --vanilla \
  "$repo_root/docs/bootstrap-single-stage-refactor/validation-tools/capture_table_record.R" \
  "$output_root" \
  "$record_path"
