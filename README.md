# Futon4 / Arxana reanimation

This repository hosts the active rewrite of the classic Arxana Emacs client.
Code lives in `dev/`, while documentation now lives in XTDB; filesystem docs
are treated as snapshots for editing or inspection. The surrounding docs
define the reanimation milestones and current storage workflows.

## Repository map

- `dev/` – Emacs Lisp for the Prototype 1 client plus the `run-tests.sh` helper.
- `test/` – ERT suites that gate the harness (`dev/run-tests.sh` runs them all).
- `docs/` – prose references including `docs/reanimation-plan.org` (living spec)
  and `docs/storage-bridge.org` (storage notes).

## Quickstart

1. Launch Futon1 (the XTDB helper service; default `http://localhost:8080`).
2. In Emacs, visit the repo root and evaluate `(load "dev/bootstrap.el")` or run
   `M-x load-file RET dev/bootstrap.el`.
3. Point the client at Futon1:
   `(setq futon4-base-url "http://localhost:8080/api/alpha")`
   and `(setq futon4-enable-sync t)`.
4. Register a buffer via `(make-current-buffer-into-article "Demo")` and try a
   scholium: `(scholium "demo/link" "Note" '(("Demo")))`.
5. Re-run `dev/run-tests.sh` after changes; it loads the Emacs modules and runs
   the ERT suites so regressions are caught before pushing.

## Testing and verification

- `dev/run-tests.sh` boots Emacs in batch mode and runs the suites under `test/`.
  Run it before publishing larger edits.
- For quick checks you can also call `M-x ert RET t RET`. The bootstrap script
  wires `dev/` onto the `load-path` so these tests see the current files.
- Storage-facing code paths should be verified against a live XTDB-backed
  instance; the commands to exercise each endpoint are documented in the
  storage bridge doc.
- Flexiarg sources must follow the canonical template enforced by
  `library-coherence/library-template-discipline`. Use the helpers in
  `dev/arxana-flexiarg-normalize.el`: `M-x arxana-flexiarg-normalize-file` (or
  `...-directory`) rewrites older IF/HOWEVER/THEN/BECAUSE stanzas into the standard
  `! conclusion` + `+ CLAUSE` form before re-running the ingest.

## Documentation links

- `docs/reanimation-plan.org` – canonical description of milestones, workflows,
  and contributor expectations.
- `docs/storage-bridge.org` – Futon API coverage plus manual verification steps.
- XTDB-backed docs are the canonical narrative source; use filesystem snapshots
  only when you need an editable or inspectable surface.

With this layout in place the repo stays focused on the XTDB-era client, while
historical material remains available for reference.
