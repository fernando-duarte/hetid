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

resolve_future_path() {
  requested_path=$1
  path_probe="$requested_path"
  path_suffix=""
  while [[ ! -e "$path_probe" && ! -L "$path_probe" ]]; do
    path_suffix="/$(basename -- "$path_probe")$path_suffix"
    path_probe="$(dirname -- "$path_probe")"
  done
  if [[ ! -d "$path_probe" ]]; then
    printf 'path ancestor must be a directory: %s\n' "$path_probe" >&2
    return 2
  fi
  resolved_path_probe="$(
    cd -- "$path_probe"
    pwd -P
  )"
  printf '%s\n' "$resolved_path_probe$path_suffix"
}

reject_repository_overlap() {
  overlap_path=$1
  overlap_label=$2
  if [[ "$overlap_path" == / ]]; then
    printf '%s must not overlap the repository: %s\n' \
      "$overlap_label" "$overlap_path" >&2
    return 2
  fi
  case "$overlap_path" in
    "$repo_root" | "$repo_root"/*)
      printf '%s must not overlap the repository: %s\n' \
        "$overlap_label" "$overlap_path" >&2
      return 2
      ;;
  esac
  case "$repo_root" in
    "$overlap_path" | "$overlap_path"/*)
      printf '%s must not overlap the repository: %s\n' \
        "$overlap_label" "$overlap_path" >&2
      return 2
      ;;
  esac
}

paths_identical() {
  first_path=$1
  second_path=$2
  if [[ "$first_path" == "$second_path" ]]; then
    return 0
  fi
  [[ -e "$first_path" && -e "$second_path" &&
    "$first_path" -ef "$second_path" ]]
}

path_is_within() {
  child_path=$1
  managed_root=$2
  case "$child_path" in
    "$managed_root" | "$managed_root"/*)
      return 0
      ;;
  esac
  if [[ ! -d "$managed_root" ]]; then
    return 1
  fi
  child_ancestor="$(dirname -- "$child_path")"
  while :; do
    if [[ -d "$child_ancestor" &&
      "$child_ancestor" -ef "$managed_root" ]]; then
      return 0
    fi
    next_ancestor="$(dirname -- "$child_ancestor")"
    if [[ "$next_ancestor" == "$child_ancestor" ]]; then
      return 1
    fi
    child_ancestor="$next_ancestor"
  done
}

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
  resolved_request="$(resolve_future_path "$run_root")"
  reject_repository_overlap "$resolved_request" "validation run root"
  mkdir -p -- "$run_root"
  run_root="$(
    cd -- "$run_root"
    pwd -P
  )"
else
  validation_tmp_base="${TMPDIR:-/tmp}"
  if [[ "$validation_tmp_base" != /* || ! -d "$validation_tmp_base" ]]; then
    printf 'TMPDIR must be an existing absolute directory\n' >&2
    exit 2
  fi
  resolved_tmp_base="$(
    cd -- "$validation_tmp_base"
    pwd -P
  )"
  reject_repository_overlap "$resolved_tmp_base" "TMPDIR"
  run_root="$(mktemp -d "$validation_tmp_base/hetid-clean-validation.XXXXXX")"
  run_root="$(
    cd -- "$run_root"
    pwd -P
  )"
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

comparison_marker="$run_root/comparison-passed"
pipeline_log="$run_root/pipeline.log"
candidate_record_path="$run_root/candidate.rds"
private_root="$run_root/.hetid-clean-validation-private"
preexisting_output="$run_root/preexisting-output"
staged_output_path="$run_root/source/scripts-paper/output"
reference_input=$1
reference_is_comparison_marker=false
if [[ -e "$reference_input" ]] &&
  paths_identical "$reference_input" "$comparison_marker"; then
  reference_is_comparison_marker=true
fi
if [[ "$reference_is_comparison_marker" == false ]]; then
  rm -f -- "$comparison_marker"
fi
if [[ -L "$reference_input" ]]; then
  printf 'reference record must not be a symbolic link: %s\n' \
    "$reference_input" >&2
  exit 2
fi
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
if paths_identical "$reference_path" "$comparison_marker" ||
  paths_identical "$reference_path" "$owner_marker" ||
  paths_identical "$reference_path" "$pipeline_log"; then
  printf 'reference record collides with managed path: %s\n' \
    "$reference_path" >&2
  exit 2
fi
candidate_alias=false
if paths_identical "$reference_path" "$candidate_record_path"; then
  candidate_alias=true
fi

Rscript --vanilla "$compare_cli" "$reference_path" "$reference_path"

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

managed_collision=""
if path_is_within "$reference_path" "$private_root"; then
  managed_collision="$private_root"
elif path_is_within "$reference_path" "$preexisting_output"; then
  managed_collision="$preexisting_output"
elif path_is_within "$reference_path" "$staged_output_path"; then
  managed_collision="$staged_output_path"
fi
if [[ -n "$managed_collision" ]]; then
  printf 'reference record collides with managed path: %s\n' \
    "$managed_collision" >&2
  exit 2
fi

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

candidate_record="$candidate_record_path"
if [[ "$candidate_alias" == true ]]; then
  candidate_record="$private_run/candidate.rds"
fi
capture_cli="$source_root/scripts-paper/validation/capture_table_record.R"
staged_compare_cli="$source_root/scripts-paper/validation/compare_table_records.R"
Rscript --vanilla "$capture_cli" "$staged_output" "$candidate_record"
Rscript --vanilla "$staged_compare_cli" "$reference_snapshot" "$candidate_record"
touch "$run_root/comparison-passed"
