#!/usr/bin/env bash
set -euo pipefail

# Usage: dev/check-parens.sh [files...]
# If no files are provided, defaults to *.el under dev/ and test/.

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
FILES=("$@")

exec emacs -Q --batch -l "$ROOT/dev/check-parens.el" --funcall arxana-check-parens-cli -- "${FILES[@]}"
