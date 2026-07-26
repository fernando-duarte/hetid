#!/usr/bin/env bash

set -euo pipefail

run_root="${HETID_VALIDATION_RUN_ROOT:-}"
owner_signature="hetid-clean-validation:v1"

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
  case "/${run_root#/}/" in
    */../* | */./*)
      printf 'HETID_VALIDATION_RUN_ROOT must not contain dot components\n' >&2
      exit 2
      ;;
  esac
  if [[ -L "$run_root" ]]; then
    printf 'HETID_VALIDATION_RUN_ROOT must not be a symbolic link\n' >&2
    exit 2
  fi
  run_probe="$run_root"
  run_suffix=""
  while [[ ! -e "$run_probe" && ! -L "$run_probe" ]]; do
    run_suffix="/$(basename -- "$run_probe")$run_suffix"
    run_probe="$(dirname -- "$run_probe")"
  done
  if [[ ! -d "$run_probe" ]]; then
    printf 'run-root ancestor must be a directory: %s\n' "$run_probe" >&2
    exit 2
  fi
  resolved_probe="$(
    cd -- "$run_probe"
    pwd -P
  )"
  resolved_request="$resolved_probe$run_suffix"
  case "$resolved_request" in
    "$repo_root" | "$repo_root"/*)
      printf 'validation run root must not overlap the repository: %s\n' \
        "$resolved_request" >&2
      exit 2
      ;;
  esac
  case "$repo_root" in
    "$resolved_request" | "$resolved_request"/*)
      printf 'validation run root must not overlap the repository: %s\n' \
        "$resolved_request" >&2
      exit 2
      ;;
  esac
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

owner_marker="$run_root/.hetid-clean-validation-owner"
root_entry="$(find "$run_root" -mindepth 1 -maxdepth 1 -print -quit)"
if [[ -n "$root_entry" ]]; then
  if [[ -L "$owner_marker" || ! -f "$owner_marker" ||
    "$(<"$owner_marker")" != "$owner_signature" ]]; then
    printf 'nonempty validation run root is not validator-owned: %s\n' \
      "$run_root" >&2
    exit 2
  fi
else
  (
    umask 077
    printf '%s\n' "$owner_signature" > "$owner_marker"
  )
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

private_root="$run_root/.hetid-clean-validation-private"
if [[ -L "$private_root" ||
  ( -e "$private_root" && ! -d "$private_root" ) ]]; then
  printf 'validation private path must be a directory: %s\n' \
    "$private_root" >&2
  exit 2
fi
mkdir -p -- "$private_root"
private_run="$(mktemp -d "$private_root/run.XXXXXX")"
reference_snapshot="$private_run/reference.rds"
cp -p -- "$reference_path" "$reference_snapshot"
if ! cmp -s -- "$reference_path" "$reference_snapshot"; then
  printf 'could not preserve the reference record snapshot\n' >&2
  exit 2
fi
Rscript --vanilla "$compare_cli" "$reference_snapshot" "$reference_snapshot"

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
rsync_options=(
  -a
  --delete
  --exclude .git
  --exclude scripts-paper/output/
)
case "$reference_path" in
  "$source_root"/*)
    reference_relative="${reference_path#"$source_root"/}"
    rsync_options+=(--exclude "/$reference_relative")
    ;;
esac
rsync "${rsync_options[@]}" "$repo_root/" "$source_root/"
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
if [[ -L "$pipeline_log" ]]; then
  printf 'pipeline log must not be a symbolic link: %s\n' "$pipeline_log" >&2
  exit 2
fi
if [[ -e "$pipeline_log" && ! -f "$pipeline_log" ]]; then
  printf 'pipeline log must be a regular file: %s\n' "$pipeline_log" >&2
  exit 2
fi
printf 'producer: %s\n' "$pipeline_script" | tee "$pipeline_log"
(
  cd -- "$source_root"
  Rscript --vanilla "$pipeline_script"
) 2>&1 | tee -a "$pipeline_log"

candidate_record="$run_root/candidate.rds"
if [[ "$candidate_record" == "$reference_path" ]]; then
  candidate_record="$private_run/candidate.rds"
fi
capture_cli="$source_root/scripts-paper/validation/capture_table_record.R"
staged_compare_cli="$source_root/scripts-paper/validation/compare_table_records.R"
Rscript --vanilla "$capture_cli" "$staged_output" "$candidate_record"
Rscript --vanilla "$staged_compare_cli" "$reference_snapshot" "$candidate_record"
touch "$run_root/comparison-passed"
