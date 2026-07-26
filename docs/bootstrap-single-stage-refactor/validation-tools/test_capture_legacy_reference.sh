#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
fixture_root=$(mktemp -d "${TMPDIR:-/tmp}/hetid-table-capture-test.XXXXXX")
legacy_root=$fixture_root/legacy
record_path=$fixture_root/reference.rds
caller_root=$fixture_root/caller
sentinel_path=$fixture_root/rprofile-ran

cleanup() {
  rm -rf "$fixture_root"
}
trap cleanup EXIT

mkdir -p "$legacy_root/scripts-paper/output/tables"
mkdir -p "$caller_root"
printf '%s\n' \
  '\begin{tabular}{lc}' \
  '\toprule' \
  ' & Value \\' \
  '\midrule' \
  'Estimate & 1.23 \\' \
  '\bottomrule' \
  '\end{tabular}' \
  >"$legacy_root/scripts-paper/output/tables/table.tex"

printf '%s\n' \
  'writeLines("startup profile ran", Sys.getenv("HETID_RPROFILE_SENTINEL"))' \
  >"$caller_root/.Rprofile"

(
  cd "$caller_root"
  HETID_RPROFILE_SENTINEL=$sentinel_path \
  HETID_LEGACY_ROOT=$legacy_root \
  HETID_LEGACY_REFERENCE_RDS=$record_path \
    bash \
    "$repo_root/docs/bootstrap-single-stage-refactor/validation-tools/capture_legacy_reference.sh"
)

test ! -e "$sentinel_path"

Rscript -e '
  record <- readRDS(commandArgs(TRUE)[[1L]])
  stopifnot(
    identical(record$schema_version, 2L),
    identical(names(record$published_tables), "table.tex")
  )
' "$record_path"

printf '%s\n' "test_capture_legacy_reference: PASS"
