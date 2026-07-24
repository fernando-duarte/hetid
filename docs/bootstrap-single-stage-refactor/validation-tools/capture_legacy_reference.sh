#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
legacy_root=${HETID_LEGACY_ROOT:-/private/tmp/hetid-fresh-pipeline-run-20260722}
default_record=$repo_root/docs/bootstrap-single-stage-refactor
default_record=$default_record/baseline-artifacts/fresh-legacy-scientific-record.rds
record_path=${HETID_LEGACY_REFERENCE_RDS:-$default_record}
run_root=$(mktemp -d "${TMPDIR:-/tmp}/hetid-legacy-reuse.XXXXXX")
source_root=$run_root/source
gate_reference=$run_root/legacy-gate.rds

mkdir -p "$source_root"
rsync -a --delete --exclude .git "$legacy_root/" "$source_root/"
rsync -a \
  "$repo_root/docs/bootstrap-single-stage-refactor/validation-tools/" \
  "$source_root/docs/bootstrap-single-stage-refactor/validation-tools/"
rsync -a \
  "$repo_root/scripts-paper/tests/support/scientific_comparison.R" \
  "$source_root/scripts-paper/tests/support/scientific_comparison.R"
cp \
  "$legacy_root/scripts-paper/output/state/log_var_eq_dynamics_gate.rds" \
  "$gate_reference"
cd "$source_root"

export HETID_BOOT_REPS=10000 HETID_BOOT_MODE=reuse
export HETID_GATE_REFERENCE_RDS="$gate_reference"
export HETID_VALIDATION_STRICT_REUSE=1
unset HETID_BOOT_CORES
mkdir -p "$(dirname "$record_path")"
Rscript \
  docs/bootstrap-single-stage-refactor/validation-tools/capture_pipeline_record.R \
  "$record_path" 2>&1 | tee "$run_root/legacy-reuse.log"

printf 'legacy reference retained at %s\n' "$record_path"
printf 'legacy reuse log retained at %s\n' "$run_root/legacy-reuse.log"
