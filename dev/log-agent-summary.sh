#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
LOG_FILE="$ROOT/dev/codex-session-log.org"

mkdir -p "$(dirname "$LOG_FILE")"

timestamp=$(date -u +"%Y-%m-%d %H:%M:%SZ")
run_id=${CODEX_SESSION_ID:-$timestamp}
files_touched=${CODEX_FILES_TOUCHED:-}

{
  echo "* ${timestamp} (${run_id})"
  if [[ -n "${files_touched}" ]]; then
    echo ":FILES: ${files_touched}"
  fi
  echo "#+BEGIN_EXAMPLE"
  cat
  echo "#+END_EXAMPLE"
  echo
} >> "$LOG_FILE"
