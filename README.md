# Futon4 / Arxana reanimation

This repository hosts the active rewrite of the classic Arxana Emacs client.
The numbered `.org` chapters are the original literate manual, while the new
`spine2.org` holds the revived implementation that tangles into the `dev/`
Elisp modules.  The surrounding docs define the reanimation milestones and the
expected storage workflows against the Futon1 HTTP API.

## Repository map

- `spine2.org` / `spine2.{tex,pdf}` – the live literate spine. Edit this file first;
  tangling regenerates everything under `dev/`. `spine2-code-links.{tex,pdf}`
  are exported from the same Org source but replace code listings with
  `code>` markers that link directly to the tangled `dev/` files.
- `old/spine.org` – legacy spine kept for reference. It is outdated and
  superseded by `spine2.org`.
- `dev/` – tangled Emacs Lisp for the Prototype 1 client plus the `run-tests.sh`
  helper. Never edit these files directly—regenerate them from `spine2.org`.
- `test/` – ERT suites that gate the harness (`dev/run-tests.sh` runs them all).
- `docs/` – prose references including `docs/reanimation-plan.org` (living spec)
  and `docs/storage-bridge.org` (Futon HTTP notes).
- `arxana/` – historical material from the classic distribution (kept for
  reference only).
- Numbered `*.org` files – the Part/Chapter split of the original Arxana manual.

## Quickstart

1. Launch Futon1 locally (default `http://localhost:8080`).
2. In Emacs, visit the repo root and evaluate `(load "dev/bootstrap.el")` or run
   `M-x load-file RET dev/bootstrap.el`. This prepares the build commands.
3. Run `M-x arxana-build` (use `C-u` for a forced re-tangle) to generate
   `arxana-tangled.el` plus every module in `dev/`.
4. Point the client at Futon: `(setq futon4-base-url "http://localhost:8080/api/alpha")`
   and `(setq futon4-enable-sync t)`.
5. Register a buffer via `(make-current-buffer-into-article "Demo")` and try a
   scholium: `(scholium "demo/link" "Note" '(("Demo")))`.
6. Re-run `dev/run-tests.sh` after changes; it loads the tangled files and runs the
   ERT suites so regressions are caught before pushing.

## Working on the literate spine

- Every code change should start in `spine2.org`. Each section explains the
  feature, followed by an Org Babel block such as
  `#+BEGIN_SRC emacs-lisp :tangle dev/arxana-store.el`.
- Tangling is driven by `M-x arxana-build` (interactive) or
  `(org-babel-tangle-file "spine2.org")` in batch environments.
- The reanimation checklist (`docs/reanimation-plan.org`) tracks which modules
  have already been converted; keep it up to date when adding new sections.
- Exported artifacts (`spine2.tex`, `spine2.pdf`, `spine.el`, etc.) are disposable
  and can be regenerated at any time.

## Exporting the spine

- `dev/spine2-export.el` exposes `arxana-spine2-export-standard` (full LaTeX),
  `arxana-spine2-export-code-links` (the short code-link variant), and
  `arxana-spine2-export-both`. Run them interactively or via batch, for example:

  ```bash
  emacs --batch -l dev/spine2-export.el -f arxana-spine2-export-both
  ```

- The export step writes `spine2.tex` plus `spine2-code-links.tex`. Build PDFs
  with `xelatex spine2.tex` and `xelatex spine2-code-links.tex` (run twice for
  stable tables of contents). XeLaTeX is recommended so Unicode glyphs like
  “↔” render without additional header tweaks.

## Testing and verification

- `dev/run-tests.sh` boots Emacs in batch mode, loads `arxana-tangled.el`, and runs
  the suites under `test/`. Run it before publishing or tangling large edits.
- For quick checks you can also call `M-x ert RET t RET`. The bootstrap script
  wires `dev/` onto the `load-path` so these tests see the freshly tangled files.
- Storage-facing code paths should be verified against a live Futon instance; the
  commands to exercise each endpoint are documented in the storage bridge doc.
- Flexiarg sources must follow the canonical template enforced by
  `library-coherence/library-template-discipline`. Use the helpers in
  `dev/arxana-flexiarg-normalize.el`: `M-x arxana-flexiarg-normalize-file` (or
  `...-directory`) rewrites older IF/HOWEVER/THEN/BECAUSE stanzas into the standard
  `! conclusion` + `+ CLAUSE` form before re-running the ingest.

## Documentation links

- `docs/reanimation-plan.org` – canonical description of milestones, workflows,
  and contributor expectations.
- `docs/storage-bridge.org` – Futon API coverage plus manual verification steps.
- Legacy manual chapters (the numbered `.org` files) – background on scholia,
  derivative works, and the original system design.

With this layout in place the repo stays “safe to tangle”: generated files in
`dev/` are versioned, the spine explains itself, and anyone landing here can jump
straight into the Emacs/Futon loop without hunting for instructions.
