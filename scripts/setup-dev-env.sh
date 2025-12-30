#!/usr/bin/env bash
set -euo pipefail

# Futon Stack Development Environment Setup
# Run with: ./scripts/setup-dev-env.sh [--with-emacs]

INSTALL_EMACS=false
for arg in "$@"; do
    case $arg in
        --with-emacs) INSTALL_EMACS=true ;;
    esac
done

echo "=== Futon Stack Dev Environment Setup ==="
echo

# Fully non-interactive apt
export DEBIAN_FRONTEND=noninteractive

# Check if running as root or can sudo
if [[ $EUID -ne 0 ]]; then
    SUDO="sudo"
    echo "Will use sudo for package installation..."
else
    SUDO=""
fi

# 1. Update apt
echo "[1/5] Updating package lists..."
$SUDO apt-get update -qq

# 2. Install Java
echo
echo "[2/5] Installing OpenJDK 21..."
if command -v java &>/dev/null && java -version 2>&1 | grep -q "21"; then
    echo "  Java 21 already installed, skipping."
else
    $SUDO apt-get install -y -qq openjdk-21-jdk
fi

# 3. Install Clojure dependencies
echo
echo "[3/5] Installing Clojure CLI dependencies..."
$SUDO apt-get install -y -qq curl rlwrap

# 4. Install Clojure CLI
echo
echo "[4/5] Installing Clojure CLI..."
if command -v clojure &>/dev/null; then
    echo "  Clojure CLI already installed: $(clojure --version 2>&1)"
    echo "  Skipping. Remove /usr/local/bin/clojure to force reinstall."
else
    TMPDIR=$(mktemp -d)
    cd "$TMPDIR"
    curl -L -O https://download.clojure.org/install/linux-install.sh
    chmod +x linux-install.sh
    $SUDO ./linux-install.sh
    cd -
    rm -rf "$TMPDIR"
fi

# 5. Optional: Emacs (use --with-emacs flag)
echo
echo "[5/5] Emacs (optional, use --with-emacs to install)..."
if command -v emacs &>/dev/null; then
    echo "  Emacs already installed: $(emacs --version 2>&1 | head -1)"
elif [[ "$INSTALL_EMACS" == "true" ]]; then
    $SUDO apt-get install -y -qq emacs-nox
else
    echo "  Skipping Emacs. Run with --with-emacs to install."
fi

# Verification
echo
echo "=== Verification ==="
echo
echo "Java:"
java -version 2>&1 | head -1 || echo "  FAILED"

echo
echo "Clojure:"
clojure --version 2>&1 || echo "  FAILED"

echo
echo "Python:"
python3 --version 2>&1 || echo "  NOT FOUND"

echo
echo "Emacs:"
emacs --version 2>&1 | head -1 || echo "  Not installed (optional)"

echo
echo "=== Setup Complete ==="
