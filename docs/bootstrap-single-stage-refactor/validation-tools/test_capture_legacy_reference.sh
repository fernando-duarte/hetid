#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
fixture_root=$(mktemp -d "${TMPDIR:-/tmp}/hetid-table-capture-test.XXXXXX")
legacy_root=$fixture_root/legacy
record_path=$fixture_root/reference.rds
caller_root=$fixture_root/caller
sentinel_path=$fixture_root/rprofile-ran
protected_record=$repo_root/docs/bootstrap-single-stage-refactor
protected_record=$protected_record/baseline-artifacts/fresh-legacy-scientific-record.rds
capture_script=$repo_root/docs/bootstrap-single-stage-refactor
capture_script=$capture_script/validation-tools/capture_legacy_reference.sh

cleanup() {
  rm -rf "$fixture_root"
}
trap cleanup EXIT

protected_fingerprint() {
  if test -e "$protected_record"; then
    shasum -a 256 "$protected_record"
  else
    printf '%s\n' "absent"
  fi
}

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

protected_before=$(protected_fingerprint)
missing_output=$(
  (
    cd "$caller_root"
    HETID_LEGACY_ROOT=$legacy_root \
    HETID_LEGACY_REFERENCE_RDS=$record_path \
      bash "$capture_script"
  ) 2>&1
) && missing_status=0 || missing_status=$?
missing_safe=false
if test "$missing_status" -ne 0 &&
  grep -Fq "usage: capture_legacy_reference.sh destination-schema3.rds" \
    <<<"$missing_output"; then
  missing_safe=true
fi

unsafe_output=$(
  (
    HETID_LEGACY_ROOT=$fixture_root/missing \
      bash "$capture_script" \
      "$protected_record"
  ) 2>&1
) && unsafe_status=0 || unsafe_status=$?
unsafe_safe=false
if test "$unsafe_status" -ne 0 &&
  grep -Fq "historical schema-2 reference is immutable" \
    <<<"$unsafe_output"; then
  unsafe_safe=true
fi
test "$(protected_fingerprint)" = "$protected_before"

(
  cd "$caller_root"
  HETID_RPROFILE_SENTINEL=$sentinel_path \
  HETID_LEGACY_ROOT=$legacy_root \
  HETID_LEGACY_REFERENCE_RDS=$record_path \
    bash "$capture_script" \
    "$record_path"
)

test ! -e "$sentinel_path"

Rscript -e '
  record <- readRDS(commandArgs(TRUE)[[1L]])
  stopifnot(
    identical(record$schema_version, 3L),
    identical(names(record$published_tables), "table.tex"),
    identical(
      record$published_tables$table.tex[[
        "tabular_1/row_1/column_1"
      ]]$stars,
      ""
    )
  )
' "$record_path"

if ! $missing_safe || ! $unsafe_safe; then
  printf '%s\n' \
    "capture safety regressions failed: missing=$missing_safe unsafe=$unsafe_safe" >&2
  exit 1
fi

printf '%s\n' "test_capture_legacy_reference: PASS"
