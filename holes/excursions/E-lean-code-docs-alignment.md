# E-lean-code-docs-alignment — Lean, code and docs as one Arxana hypergraph

Opened 2026-10-19 by claude-2, from a conversation with Joe.

## Why

The War Machine has three descriptions of the same behaviour. The formal one is
`mathlib4/DarkTower/WarMachine/*.lean`. The code is in futon2 and futon3c. The
informal one is the logic model in `futon2/holes/labs/wm-contract/logic-model/`
and the PLoP 2026 paper. Today they point at each other with `file:line`, which
goes stale with the next edit. The L1 Lean module cited
`full_loop_runtime.clj:475-505`, and the packet that commissioned it cited
`482-517` for the same code.

Arxana's link tiers (`dev/arxana-links.el`) replace a stored location with
something that can be found again. A Tier 1 link stores the finder rule and is
recomputed. A Tier 2 anchor stores a content hash plus its surrounding
context. Annotation becomes search. Joe, 2026-10-19: every organisation has
code its programmers have not read in months; Lean, code and docs that stay
aligned as the code moves would address that.

## Display constraint (Joe)

The display has two panes, not three: Joe works on a Pixel 8. The code buffer
and `*Arxana Code Docs*` stay side by side, as they do now for
`arxana-browser-browse.el`. The Lean layer is **transcluded** into the docs
buffer, using its own highlight colour. With point on a function, the doc chunk
lights in the existing docs colour. Each Lean declaration that cites the
function is shown inline, with its statement and docstring text, and lights in
a second colour.

## First slice: the self-heal route

All three columns exist and are small:

- **code:** futon2 `src/futon2/aif/full_loop_runtime.clj` (`run-opportunity!`)
  and `src/futon2/aif/meta_self_heal_production.clj` (`production-readiness`,
  `route!`, `prepare-acceptance`);
- **Lean:** `DarkTower/WarMachine/SelfHealRoute.lean` (mathlib4 `darktower`);
- **docs:** logic-model `phases.md` "Injury vs ordinary, level 1" and
  `refusals.md` "Self-heal route refusals".

## Packets (one behaviour each; Codex builds, claude-2 reviews)

- **A. Clojure definitions are linkable.** `arxana-browser-code.el` has the
  definition regexp copied five times (`--file-symbols`, `--defun-regexp`,
  `--current-def-symbol`, `--find-defun-bounds`, and the strategy's
  `--def-patterns`). These copies know `defn`/`defn-` but not `def`,
  `defmulti`, `defmethod`, `defprotocol`, `defrecord` or `deftype`, and they
  do not allow metadata between the form and the name (`(def ^:dynamic
  *launch-route-check!* …)`). Replace the copies with one regexp built from
  `arxana-browser-code--def-patterns`.
- **B. The browser can index a futon2 code root.** Set the roots outside the
  docbook's repo (absolute paths), and docs come from a futon2 docbook entry
  set for the self-heal route.
- **C. Lean transclusion.** Each Lean declaration's citation of code becomes
  an `:explicit` link from the declaration to the code symbol. That link is
  re-found by symbol, never by line. Pinned record witnesses (click 2's run
  record) are Tier 2 anchors. The docs buffer renders the cited declarations
  inline in a second face.
- **D. Options-map port finder.** Add a finder type `:options-key` that finds
  `(:x opts)` reads and the places that fill the key. With it, the neighbourhood
  of an R-node is computed rather than listed by hand. The coverage question
  (how much of the code do R-node finders plus port finders reach) is then
  answered by a strategy, not a one-off script.

## How we will know

Re-resolve the slice's links at an older futon2 sha and at the current one, and
count how many land. That count measures whether the links survive edits.

## Notes

- The PLoP 2026 docbook's code annotations are already org search links
  (`file:~/code/futon2/...::symbol @ sha`), not `file:line`. It is not yet
  known whether that docbook has the side-by-side cursor sync.
- Anchor format, port finder and coverage were discussed with Joe in the
  claude-2 REPL, 2026-10-19 (options-map ports, wire ledger
  `futon3c/holes/labs/M-wm-wiring/wm-wire-ledger.edn`, plop annotations).
