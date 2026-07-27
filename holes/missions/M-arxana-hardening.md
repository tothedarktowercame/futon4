# M-arxana-hardening — stop the Arxana UI from silently losing itself

Chartered 2026-07-27 (Joe: "too many issues here to hotfix"). Status:
IDENTIFY — evidence collected during the 2026-07-26/27 Outbox incident;
notes deposited by claude-3. Awaiting Joe's ratification/scoping.

## The incident that named the mission

Joe tried to use the cold outbox to send a warm email to JUXT and hit,
in sequence: (1) the Outbox entry missing from the browser home; (2) the
keys listed in `*Arxana Cold Outbox*` all dead; (3) the keys that DID
act in the browser view being media-player commands. All three were
regressions of work completed and verified on 2026-07-05.

## Findings (each is a class, not an instance)

### F1 — Features are lost silently because registration is manual
The browser home menu is a static list in `arxana-browser-core.el`; the
2026-07-18 rewrite of that file dropped the Outbox entry AND the entire
per-view keymap registry, and nothing noticed for eight days. Hotfixed
2026-07-26 (`3e2036a`, `b649db5` — registry restored WITH a
warn-once-per-session enforcement when a view renders unregistered).
**Hardening direction:** self-registration — features add their menu
entry and keymap on load (the view-keymap registry is the pattern);
the static list becomes a fallback ordering hint, not the source of
truth. A rewrite of browser-core then cannot lose a feature.

### F2 — The derived-mode keymap footgun has now fired twice
`defvar <mode>-map` below `define-derived-mode` is a silent no-op (the
mode defvars an empty map first). Fixed 2026-07-05 in cold-outbox;
reintroduced by 2026-07-08; refixed 2026-07-26 with the footgun
documented at the site. **Hardening direction:** a lint/smoke check —
every `arxana-*-mode-map` must be non-empty after load; greppable rule
in a check script beside check-parens (this is exactly the kind of
thing goals-must-fail was for).

### F3 — Shared-map key leaks on every unregistered view
Media keys (S/e/r/…) act on non-media listings. The restored registry
now warns once per session per unregistered view, so normal browsing
enumerates the adoption work-list. Known adopters needed: Sales,
Ledger, Field Desk, Evidence views, Missions, Scans, Invariants, Trace,
VSATARCS, Songs, Essays, Docs, Encyclopedia (audit as they warn).
**Hardening direction:** adoption sweep + flip the warning to a hard
render banner (or Reazon goal failure) once the sweep is done.

### F4 — Per-feature hydras blocked on the compile environment
Design intent: each feature gets its own hydra behind `?`. Batch
`emacs -Q` byte-compilation has no hydra on the load path, so top-level
`defhydra` cannot compile; 2026-07-26 shipped plain message-menus as a
stopgap. **Hardening direction:** fix the compile environment (declare
the dev load-path incl. hydra in a shared batch-compile script), then
convert menus to hydras feature-by-feature during the F3 sweep.

### F5 — Stale .elc shadowing (recurring; third occurrence 2026-07-26)
Old .elc beats edited .el on load. Already a recorded memory; still
manual. **Hardening direction:** either `load-prefer-newer t` in the
relevant Emacsen, or a compile-on-commit hook / single `make elc`
target, plus the batch-compile script from F4.

### F6 — UI invariants enforced only on Essays surfaces
Joe's standing invariants (two-up pairs; solo windows; RET = focus;
back restores the previous 2-up; NO self-inserting nav keys in editable
buffers; one counts vocabulary) are enforced + Reazon-gated in
`arxana-essays-twoup.el` / `arxana-window-constraints.el` only. Browser
and outbox surfaces predate them. **Hardening direction:** extend the
constraint layer to browser-core-rendered surfaces; the outbox edit
buffer (`e`) must obey the editable-buffer navigation rule.

### F7 — No regression net for any of the above
None of F1–F6 has a test. The 2026-07-05 work was verified live and
still evaporated. **Hardening direction:** a small `arxana-smoke.el`
batch suite: home menu contains every registered feature; every
registered view has a keymap; every arxana mode-map is non-empty;
byte-compile all dev modules clean. Run it like check-parens (gate in
handoffs; cheap enough for every commit).

## Evidence trail
- 2026-07-05 original wiring + porcelain fixes (recorded in memory;
  commits that era) — the work that was lost.
- 2026-07-18 browser-core rewrite (menu + registry dropped).
- 2026-07-26/27 hotfixes: futon4 `3e2036a` (menu restore),
  `b649db5` (registry + enforcement + outbox keys + footgun docs).
- The warn-once messages now accumulate the F3 work-list organically.

## Suggested slices (for scoping discussion)
1. S1: F5+F4 compile/loading hygiene (small; unblocks hydras).
2. S2: F1 self-registration for menu entries (medium).
3. S3: F3 adoption sweep with F4 hydras, view by view (incremental).
4. S4: F7 smoke suite; then flip F3's warning to enforcement (small).
5. S5: F6 invariant extension to browser surfaces (larger; needs Joe's
   eyes on interaction design).
