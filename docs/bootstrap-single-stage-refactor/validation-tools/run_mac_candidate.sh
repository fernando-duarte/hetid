#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
run_root=${HETID_VALIDATION_RUN_ROOT:-$(mktemp -d "${TMPDIR:-/tmp}/hetid-mac-validation.XXXXXX")}
source_root=$run_root/source
legacy_root=${HETID_LEGACY_ROOT:-/private/tmp/hetid-fresh-pipeline-run-20260722}
gate_reference=$run_root/legacy-gate.rds

mkdir -p "$source_root"
rsync -a --delete --exclude .git --exclude scripts-paper/output/ \
  "$repo_root/" "$source_root/"
cp \
  "$legacy_root/scripts-paper/output/state/log_var_eq_dynamics_gate.rds" \
  "$gate_reference"
cd "$source_root"

export HETID_BOOT_REPS=10000
export HETID_GATE_REFERENCE_RDS="$gate_reference"
unset HETID_VALIDATION_STRICT_REUSE
unset HETID_BOOT_CORES
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1 HETID_BOOT_MODE=rerun
Rscript \
  docs/bootstrap-single-stage-refactor/validation-tools/capture_pipeline_record.R \
  "$run_root/candidate-rerun.rds" \
  2>&1 | tee "$run_root/candidate-rerun.log"

export HETID_BOOT_MODE=reuse
export HETID_VALIDATION_STRICT_REUSE=1
Rscript \
  docs/bootstrap-single-stage-refactor/validation-tools/capture_pipeline_record.R \
  "$run_root/candidate-reuse.rds" \
  2>&1 | tee "$run_root/candidate-reuse.log"

Rscript \
  docs/bootstrap-single-stage-refactor/validation-tools/compare_scientific_objects.R \
  "$run_root/candidate-rerun.rds" \
  "$run_root/candidate-reuse.rds"

default_legacy=$repo_root/docs/bootstrap-single-stage-refactor
default_legacy=$default_legacy/baseline-artifacts/fresh-legacy-scientific-record.rds
legacy_record=${HETID_LEGACY_REFERENCE_RDS:-$default_legacy}
Rscript \
  docs/bootstrap-single-stage-refactor/validation-tools/compare_scientific_objects.R \
  "$legacy_record" \
  "$run_root/candidate-rerun.rds"

printf 'validation run retained at %s\n' "$run_root"
