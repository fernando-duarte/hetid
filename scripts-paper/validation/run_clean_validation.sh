#!/usr/bin/env bash

set -euo pipefail

run_root="${HETID_VALIDATION_RUN_ROOT:-}"

report_run_root() {
  status=$?
  if [[ -n "$run_root" ]]; then
    printf 'validation run root: %s\n' "$run_root"
  else
    printf 'validation run root: not created\n'
  fi
  exit "$status"
}
trap report_run_root EXIT

if [[ $# -ne 1 ]]; then
  printf 'Usage: %s reference.rds\n' "$0" >&2
  exit 2
fi

script_dir="$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd -P
)"
repo_root="$(
  cd -- "$script_dir/../.."
  pwd -P
)"
if [[ -n "$run_root" ]]; then
  if [[ "$run_root" != /* ]]; then
    printf 'HETID_VALIDATION_RUN_ROOT must be an absolute path\n' >&2
    exit 2
  fi
  mkdir -p -- "$run_root"
  run_root="$(
    cd -- "$run_root"
    pwd -P
  )"
else
  validation_tmp_base="${TMPDIR:-/tmp}"
  run_root="$(mktemp -d "$validation_tmp_base/hetid-clean-validation.XXXXXX")"
fi
if [[ "$run_root" == / ]]; then
  printf 'HETID_VALIDATION_RUN_ROOT must not resolve to the filesystem root\n' >&2
  exit 2
fi
rm -f -- "$run_root/comparison-passed"

reference_input=$1
if [[ ! -f "$reference_input" ]]; then
  printf 'reference record does not exist: %s\n' "$reference_input" >&2
  exit 2
fi
reference_dir="$(
  cd -- "$(dirname -- "$reference_input")"
  pwd -P
)"
reference_path="$reference_dir/$(basename -- "$reference_input")"
compare_cli="$repo_root/scripts-paper/validation/compare_table_records.R"

Rscript --vanilla "$compare_cli" "$reference_path" "$reference_path"

source_root="$run_root/source"
if [[ -L "$source_root" ]]; then
  printf 'staged source must not be a symbolic link: %s\n' "$source_root" >&2
  exit 2
fi
mkdir -p -- "$source_root"
source_root="$(
  cd -- "$source_root"
  pwd -P
)"
case "$source_root" in
  "$run_root"/*)
    ;;
  *)
    printf 'staged source is outside the resolved run root: %s\n' "$source_root" >&2
    exit 2
    ;;
esac
preexisting_git="$(find "$source_root" -name .git -print -quit)"
if [[ -n "$preexisting_git" ]]; then
  printf 'preexisting staged Git metadata is not allowed: %s\n' \
    "$preexisting_git" >&2
  exit 2
fi
rsync -a --delete \
  --exclude .git \
  --exclude scripts-paper/output/ \
  "$repo_root/" "$source_root/"
staged_output="$source_root/scripts-paper/output"
preexisting_output="$run_root/preexisting-output"
if [[ -e "$staged_output" || -L "$staged_output" ]]; then
  if [[ -e "$preexisting_output" || -L "$preexisting_output" ]]; then
    printf 'recovery path already exists: %s\n' "$preexisting_output" >&2
    exit 2
  fi
  mv -- "$staged_output" "$preexisting_output"
fi
mkdir -p -- "$staged_output"

pipeline_script="${HETID_VALIDATION_PIPELINE_SCRIPT:-scripts-paper/run_pipeline.R}"
case "$pipeline_script" in
  "" | /*)
    printf 'pipeline script must be a relative path inside staged source\n' >&2
    exit 2
    ;;
esac
case "/$pipeline_script/" in
  */../* | */./*)
    printf 'pipeline script must not contain dot path components\n' >&2
    exit 2
    ;;
esac
producer_path="$source_root/$pipeline_script"
if [[ ! -f "$producer_path" ]]; then
  printf 'pipeline script does not exist: %s\n' "$pipeline_script" >&2
  exit 2
fi

export HETID_BOOT_REPS=10000
export HETID_BOOT_MODE=rerun
unset HETID_VALIDATION_STRICT_REUSE

pipeline_log="$run_root/pipeline.log"
printf 'producer: %s\n' "$pipeline_script" | tee "$pipeline_log"
(
  cd -- "$source_root"
  Rscript --vanilla "$pipeline_script"
) 2>&1 | tee -a "$pipeline_log"

candidate_record="$run_root/candidate.rds"
capture_cli="$source_root/scripts-paper/validation/capture_table_record.R"
staged_compare_cli="$source_root/scripts-paper/validation/compare_table_records.R"
Rscript --vanilla "$capture_cli" "$staged_output" "$candidate_record"
Rscript --vanilla "$staged_compare_cli" "$reference_path" "$candidate_record"
touch "$run_root/comparison-passed"
