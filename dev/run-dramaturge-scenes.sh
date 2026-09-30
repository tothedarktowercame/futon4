#!/usr/bin/env bash
# run-dramaturge-scenes.sh — run arxana-dramaturge scenes in a scratch daemon.
#
# Scenes NEVER run in Joe's Emacs.  The only permitted target is the
# daemon whose server name is "dramaturge"; this script refuses anything
# else.  Usage:
#
#   run-dramaturge-scenes.sh [SCENE-NAME]
#
# Starts `emacs --daemon=dramaturge -Q` if absent, loads
# arxana-dramaturge-scene with the futon3c/emacs and futon4/dev
# load-paths, runs all scenes (or SCENE-NAME), prints the report, and
# exits 1 on any failure.

set -euo pipefail

SERVER="dramaturge"

# Hard refusal: the target server name must be exactly "dramaturge".
# This guards against any future edit or environment that would point
# scene driving at the operator's (default) server.
if [[ "$SERVER" != "dramaturge" ]]; then
  echo "run-dramaturge-scenes: refusing target server '$SERVER' (must be 'dramaturge')" >&2
  exit 2
fi
# Belt and braces: never allow EMACS_SOCKET_NAME / --socket-name overrides.
if [[ -n "${EMACS_SOCKET_NAME:-}" && "${EMACS_SOCKET_NAME:-}" != "dramaturge" ]]; then
  echo "run-dramaturge-scenes: EMACS_SOCKET_NAME is set to something other than 'dramaturge'; refusing" >&2
  exit 2
fi

FUTON3C_EMACS="/home/joe/code/futon3c/emacs"
FUTON4_DEV="/home/joe/code/futon4/dev"
SCENE="${1:-}"

ensure_daemon() {
  if emacsclient -s "$SERVER" -e t >/dev/null 2>&1; then
    return 0
  fi
  echo "run-dramaturge-scenes: starting daemon '$SERVER'" >&2
  emacs --daemon="$SERVER" -Q \
    --eval "(setq load-path (append (list \"$FUTON3C_EMACS\" \"$FUTON4_DEV\") load-path))" \
    --eval "(require 'arxana-dramaturge)" \
    --eval "(require 'arxana-dramaturge-scene)" >/dev/null
  # wait until responsive
  for _ in $(seq 1 50); do
    if emacsclient -s "$SERVER" -e t >/dev/null 2>&1; then return 0; fi
    sleep 0.2
  done
  echo "run-dramaturge-scenes: daemon '$SERVER' did not come up" >&2
  exit 1
}

ensure_daemon

FORM="(arxana-dramaturge-scenes-batch ${SCENE:+\"$SCENE\"})"
VERDICT="$(emacsclient -s "$SERVER" -e "$FORM" | tr -d '\"')"

OUT="$(emacsclient -s "$SERVER" -e 'arxana-dramaturge-scene-out-dir' | tr -d '\"')"
cat "$OUT/report.txt"

if [[ "$VERDICT" == "PASS" ]]; then
  exit 0
else
  echo "run-dramaturge-scenes: verdict $VERDICT" >&2
  exit 1
fi
