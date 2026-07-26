#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
if [ "$#" -ne 1 ]; then
  printf '%s\n' \
    "usage: run_mac_candidate.sh reference-schema3.rds" >&2
  exit 64
fi
exec bash \
  "$repo_root/scripts-paper/validation/run_clean_validation.sh" \
  "$1"
