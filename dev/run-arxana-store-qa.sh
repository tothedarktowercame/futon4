#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FUTON1A_ROOT="${FUTON1A_ROOT:-/home/joe/code/futon1a}"
PORT="${ARXANA_STORE_QA_PORT:-18080}"
TS="$(date +%Y%m%d-%H%M%S)"
DATA_DIR="${ARXANA_STORE_QA_DATA_DIR:-/home/joe/code/storage/futon1a-qa/$TS}"
LOG_DIR="${ARXANA_STORE_QA_LOG_DIR:-/home/joe/code/storage/futon1a-qa}"
LOG_FILE="$LOG_DIR/futon1a-qa-$TS.log"

mkdir -p "$DATA_DIR" "$LOG_DIR"

echo "[qa] starting futon1a on port $PORT (data-dir=$DATA_DIR)"
(
  cd "$FUTON1A_ROOT"
  env FUTON1A_DATA_DIR="$DATA_DIR" \
      FUTON1A_PORT="$PORT" \
      FUTON1A_ALLOWED_PENHOLDERS="joe" \
      FUTON1A_COMPAT_PENHOLDER="joe" \
      clojure -M -m futon1a.system
) >"$LOG_FILE" 2>&1 &
PID=$!

cleanup() {
  echo "[qa] stopping futon1a pid=$PID"
  kill "$PID" >/dev/null 2>&1 || true
}
trap cleanup EXIT

echo "[qa] waiting for health..."
for _ in $(seq 1 80); do
  if curl -fsS "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then
    break
  fi
  sleep 0.25
done
curl -fsS "http://127.0.0.1:$PORT/health" >/dev/null

echo "[qa] running emacs batch QA (log=$LOG_FILE)"
emacs -Q --batch \
  -L "$ROOT_DIR" \
  --eval "(setq futon4-base-url \"http://127.0.0.1:$PORT/api/alpha\" futon4-enable-sync t)" \
  -l "$ROOT_DIR/arxana-store-qa.el"

echo "[qa] ok"

