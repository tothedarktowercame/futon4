# INSTANTIATE — M-formal-arxana

Joe, 2026-11-08: codex-21 builds and reviews the design work claude-13 led;
claude-13 reviews the build. One increment per packet, in DERIVE-wiring §2
order: C1, C2, C3, then F1 / F3 / F5, then F2, F4, F6.

## Design review (codex-21, packet 1, job invoke-1794118087111-15570-f9d28324)

- VERIFY: agreed, except the Lean model's `acceptance` (latest assessment
  wins). Append order is not semantic authority; conflicting attributable
  assessments may coexist; selection needs a named policy. **Fixed** by
  claude-13 in futon5 476153d: `AcceptancePolicy` over all of a claim's
  assessments, `conflicting_assessments_coexist` proved.
- Lean `Identity`: its fixed content key abstracts per-type identity
  projections; C1 implements the stronger declared-field rule. Agreed.
- Refactored ARGUE: P1's "evidence and standing on a 'clusion" and P10's
  default dynamics reference are stale after round 9. Evidence, interpreter
  and method belong to attestations by default (DERIVE-core §18).
- CLIENTS / REPL Circus: agreed.
- Round-10 rulings: DERIVE-core §18 (futon4 d7a15a2).

## C1 — typed identity core (accepted)

Built by codex-21: futon3c aeb9dbc (merged to master), futon3 09ea3b3
(generated turnfeed), warrant `C1-warrant.edn` (futon4 fb3e32e). One packet,
so the C1 falsifier (more than two packets) did not fire.

Review (claude-13):
- **Record check:** the warrant's shas match the commits; 7 files in
  aeb9dbc (`records.clj`, `type_registry.clj`, vocabulary source and
  generator, `formal_c1_test.clj`, golden records, xiaoxiang-preview.el).
- **Test adequacy, by planted bad cases** in a review worktree at aeb9dbc:
  - id computed from sorted targets with roles dropped (futon1b's default)
    → caught by the role-swap test and all 7 pinned golden ids;
  - registry accepting an in-place declaration edit → caught by
    `declaration-revision-is-immutable`;
  - hand edit inside a generated vocabulary region → caught by the drift
    test.
- Golden records: all four F1 fixture turns and claude-19's four Circus
  candidates (NgDb6h, x3SMUF, Ndo9tN, T-car3), each id pinned.
- Lean-mirroring tests are named after the theorems they mirror.
- Not re-run: the 84-test Arxana suite (warrant present, shas match).

Noted for Joe: the generator rewrites the key table in
`~/code/reply-proforma.md` between generated markers. That file is part of
every agent's instructions and is not under git; the 23 marks are unchanged,
only the table layout.

## C2 — typed write ports (accepted)

Built by codex-21: futon1b 528f606, futon3c 34ab34b + 35261e6 (both merged
to master), warrant `C2-warrant.edn` (futon4 8639cd4). Live futon1b and
futon3c JVMs not reloaded or restarted (warrant; both JVMs' start times
predate 2026-11-08).

Review (claude-13), planted bad cases in a futon1b review worktree at 528f606:
- (a) generic route accepts arxana/* types → caught by the existing
  conditional-route suite (15/24), not by the new C2 suite;
- (b) a new assessment overwrites the earlier one (keyed by subject) →
  caught: `conflicting_assessments_coexist` FAIL;
- (c) replayed message id writes again → caught, but as an uncaught
  `arxana-immutable-record-conflict` that aborts the C2 runner rather than a
  named failure. Content-addressed ids make an identical replay harmless;
  the abort is still a failed run;
- (d) evidence citing an unknown 'clusion accepted → caught: the
  unknown-clusion check FAIL.
- Lean-mirroring names present in both suites (`append_keeps`,
  `attest_keeps_claims`, `assess_keeps_claims`, `assess_keeps_evidence`,
  `two_interpreters_one_claim`, `conflicting_assessments_coexist`).
- Not re-run: the 86-test Arxana suite (warrant shas match).

## C3a — index and bounded reads (returned for rework)

Built by codex-21: futon1b a84ddc1 + f01a817, futon3c 269b09e, warrant
`C3a-warrant.edn` (futon4 9990a06). Warrant shas match. Not accepted:

1. **Every read scans the whole store.** `query-edges` and `attestations`
   call `current-watermark`, which reads and EDN-parses every row of
   `arxana_clusions` and `arxana_evidence` and hashes all their ids
   (futon1b_arxana_index.clj:27-38, 83, 94). The index answers in O(limit),
   but its freshness check is O(n) per read, which is the full scan DERIVE-core
   §15 rules out on the prompt path. Each write also triggers a full rebuild.
   The measurements ran on 100 records, where this cannot show; VERIFY S1's
   scale was 50k 'clusions and 100k attestations.
2. **Traversal can report `:complete` after dropping neighbours.**
   `graph_read/traverse` asks each frontier node for at most 200 edges and
   does not record when a node has more, so a hub with more than 200 edges
   yields a `:complete` result that is missing nodes. Lean `Read` requires
   truncation to be visible.
3. Traversal follows edge ids as the next frontier ('clusion-to-'clusion
   only); it never steps out through a 'clusion's other endpoints. State
   which traversal C3 provides, or provide both.

Planted-bug checks deferred to the reworked version.
