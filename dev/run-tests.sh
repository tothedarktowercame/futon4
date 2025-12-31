#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="${EMACS:-emacs}"
export HOME="$ROOT/.home-tmp"
export EMACS_NATIVE_COMPILATION="${EMACS_NATIVE_COMPILATION:-0}"
mkdir -p "$HOME" "$HOME/.emacs.d/eln-cache"
TEST_FILES=(
  arxana-bootstrap-test.el
  arxana-store-test.el
  arxana-article-test.el
  arxana-scholium-test.el
  arxana-relations-test.el
  arxana-adjacency-test.el
  arxana-derivation-test.el
  arxana-inclusion-test.el
  arxana-inclusion-batch-test.el
  arxana-import-test.el
  arxana-export-test.el
  arxana-saving-test.el
  arxana-browse-test.el
  arxana-docbook-order-test.el
  arxana-docbook-redirect-test.el
  arxana-docbook-checkout-test.el
  arxana-xtdb-browse-test.el
  arxana-compat-test.el
  arxana-flexiarg-normalize-test.el
)

for test_file in "${TEST_FILES[@]}"; do
  echo "==> Running ${test_file}"
  "$EMACS_BIN" --batch \
    -l "$ROOT/dev/bootstrap.el" \
    -l "$ROOT/test/${test_file}" \
    -f ert-run-tests-batch-and-exit
  echo
done
