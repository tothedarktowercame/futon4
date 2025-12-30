# Futon Stack Development Environment Setup

This guide sets up a complete development environment for the Futon Stack
(futon0–futon4) on Ubuntu 24.04. Each section includes verification commands.

## Prerequisites

Confirm you're on a compatible system:

```bash
cat /etc/os-release | grep -E "^(NAME|VERSION)="
# Expected: Ubuntu 24.04 or similar Debian-based
```

## 1. Core Tools

### 1.1 Git (usually pre-installed)

```bash
git --version
# If missing: sudo apt install git
```

### 1.2 Java (JDK 21+ for Clojure)

Clojure runs on the JVM. Install OpenJDK 21:

```bash
sudo apt update
sudo apt install -y openjdk-21-jdk

# Verify
java -version
# Expected: openjdk version "21.x.x"
```

### 1.3 Clojure CLI Tools

Official installer from clojure.org:

```bash
# Install dependencies
sudo apt install -y curl rlwrap

# Download and run official installer
curl -L -O https://download.clojure.org/install/linux-install.sh
chmod +x linux-install.sh
sudo ./linux-install.sh

# Verify
clojure --version
# Expected: Clojure CLI version 1.12.x.xxxx
```

### 1.4 Python 3 (usually pre-installed)

```bash
python3 --version
# Expected: Python 3.12.x or similar

# If you need pip:
sudo apt install -y python3-pip python3-venv
```

## 2. Optional Tools

### 2.1 Emacs (for futon4/arxana development)

```bash
sudo apt install -y emacs

# Verify
emacs --version | head -1
```

### 2.2 Babashka (fast Clojure scripting)

Useful for quick scripts without JVM startup time:

```bash
curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install
chmod +x install
sudo ./install

# Verify
bb --version
```

## 3. Repository Setup

### 3.1 Clone repositories (if not already present)

The Futon Stack consists of 5 layers. Adjust URLs to your git remote:

```bash
cd ~
# Example - replace with actual remotes:
# git clone git@github.com:USER/futon0.git
# git clone git@github.com:USER/futon1.git
# git clone git@github.com:USER/futon2.git
# git clone git@github.com:USER/futon3.git
# git clone git@github.com:USER/futon4.git
```

### 3.2 Verify layer structure

```bash
ls -d ~/futon{0,1,2,3,4} 2>/dev/null && echo "All layers present"
```

### 3.3 futon5 dependency

futon2 references `futon5/cyber` as a local dependency. If futon5 doesn't exist,
you'll need to either:

1. Clone futon5 alongside the others, or
2. Stub it out (create `~/futon5/deps.edn` with minimal config)

Check if needed:

```bash
grep -r "futon5" ~/futon*/deps.edn 2>/dev/null
```

## 4. Layer-Specific Setup

### 4.1 futon0 (Automation/Vitality)

Python scripts for vitality monitoring. No special setup beyond Python 3.

```bash
cd ~/futon0
python3 scripts/vitality_scanner.py --help 2>/dev/null || echo "Check script requirements"
```

### 4.2 futon1 (Persistence/NLP)

Multi-module Clojure project with API server, graph-memory, NLP pipeline.

```bash
cd ~/futon1

# Download dependencies (first run takes a while)
clojure -P

# Run tests
clojure -M:test

# Start API server (if configured)
# clojure -M:api
```

### 4.3 futon2 (Ant Simulation/Active Inference)

Clojure simulation engine. Requires futon5 dependency.

```bash
cd ~/futon2

# Download dependencies
clojure -P

# Run tests
clojure -M:test

# Run simulation
# clojure -M:run
```

### 4.4 futon3 (Central Hub/MUSN)

Main orchestration layer with Datascript, http-kit server.

```bash
cd ~/futon3

# Download dependencies
clojure -P

# Run tests
clojure -M:test

# Start MUSN server
# clojure -M:dev
```

### 4.5 futon4 (Arxana/Hypergraph)

Emacs-based hypergraph system with lab notebook tooling.

```bash
cd ~/futon4

# Lab export tooling (Clojure scripts)
clojure -M dev/lab-export-claude.clj --help

# Emacs package (load in Emacs)
# (load-file "~/futon4/dev/arxana-lab.el")
```

## 5. Lab Notebook System

The lab system captures coding sessions as research artifacts.

### 5.1 Directory structure

```bash
ls -la ~/futon4/lab/
# Should show: raw/ stubs/ trace/ doc-drafts/
```

### 5.2 Test the Claude exporter

```bash
cd ~/futon4

# Find a Claude session file
ls ~/.claude/projects/*//*.jsonl 2>/dev/null | head -3

# Dry run (replace SESSION_FILE with actual path)
# clojure -M dev/lab-export-claude.clj \
#   --session-file SESSION_FILE \
#   --repo-root $(pwd) \
#   --lab-root $(pwd)/lab \
#   --dry-run
```

### 5.3 Using fuclaude wrapper

The `fuclaude` script wraps Claude Code and auto-exports sessions:

```bash
# Make executable (if not already)
chmod +x ~/futon4/fuclaude

# Use instead of bare 'claude' command
cd ~/some-project
~/futon4/fuclaude "your prompt here"
```

## 6. Verification Checklist

Run this to verify your setup:

```bash
echo "=== Futon Stack Environment Check ==="
echo

echo "Java:"
java -version 2>&1 | head -1 || echo "  NOT INSTALLED"

echo
echo "Clojure:"
clojure --version 2>&1 || echo "  NOT INSTALLED"

echo
echo "Python:"
python3 --version 2>&1 || echo "  NOT INSTALLED"

echo
echo "Emacs:"
emacs --version 2>&1 | head -1 || echo "  NOT INSTALLED (optional)"

echo
echo "Repositories:"
for i in 0 1 2 3 4; do
  if [ -d ~/futon$i ]; then
    echo "  futon$i: OK"
  else
    echo "  futon$i: MISSING"
  fi
done

echo
echo "Lab directories:"
ls -d ~/futon4/lab/{raw,stubs,trace,doc-drafts} 2>/dev/null && echo "  OK" || echo "  MISSING"
```

## 7. Troubleshooting

### Clojure dependency download fails

```bash
# Clear cache and retry
rm -rf ~/.m2/repository
clojure -P
```

### Java version conflicts

```bash
# List installed versions
update-java-alternatives --list

# Set default
sudo update-alternatives --config java
```

### futon5 missing

If futon2 fails due to missing futon5, create a stub:

```bash
mkdir -p ~/futon5
cat > ~/futon5/deps.edn << 'EOF'
{:paths ["src"]
 :deps {org.clojure/clojure {:mvn/version "1.11.1"}}}
EOF
mkdir -p ~/futon5/src/futon5
echo "(ns futon5.cyber)" > ~/futon5/src/futon5/cyber.clj
```

---

*Last updated: 2024-12-30*
