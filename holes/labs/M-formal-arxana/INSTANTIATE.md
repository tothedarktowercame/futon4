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

### C3a rework (accepted)

codex-21: futon1b ad1f68a, futon3c 364e714, warrant updated (futon4 c846cae).
The three findings are fixed: freshness is an O(1) source sequence advanced
in the same transaction as each indexed record (`source-sequence`,
futon1b_arxana.clj:125), with CAS and up to 32 retries for concurrent
writers; the index catches up incrementally from ordered index events, and
a test checks incremental == full rebuild; fan-out beyond a node's query
limit is a typed `:fan-out` truncation; `:endpoint-expansion` traversal
steps endpoint → 'clusions → their other endpoints.

Review (claude-13), planted bugs:
- (a) traversal ignores max-nodes → caught (`read-bounded--mirrors-lean-read_bounded`, maxNodes-2 example);
- (b) visited check removed → caught (`read-nodup--…`, three-cycle example);
- (c) stale index answers silently → caught (formal-c3a-test);
- (e) hub fan-out ignored by the traversal → caught (hub test);
- **(d) reverse-role lookup ignores the role → NOT caught by any suite;**
- **(f) the real store never reports fan-out → NOT caught**: the hub test's
  stub hard-codes `:truncated?`, so it tests the traversal, not the store.
- Also found: `graph/query` returned `:result/status :complete` for 55
  matches under LIMIT 50, dropping the store's `:truncated?` and `:total`.

Fixed in review (claude-13, futon3c f1d34fe, merged): `graph/query` reports
`:truncated` with `:result/truncation :limit` and `:result/total`; the
integration test (real store over HTTP) now checks that, and checks role
sensitivity with an endpoint used only as a turn. Re-planted (d) and (f)
against futon1b ad1f68a: both caught. Arxana suite 93 tests, 582
assertions; clj-kondo and check-parens clean.

Measurements at VERIFY S1's scale: recorded in VERIFY.md, Storage
(substrate) row. All under the 5 s budget. Note for clients: during a write
burst, reads say `:stale-index` for about 3 s rather than answer.

## C3b — attempts, external references, aggregate (accepted after review fixes)

Built by codex-21: futon1b d157125 + 80ad3f9, futon3c e73db6b, warrant
`C3b-warrant.edn` (futon4 d1bb67c). Tests run against the real private
stack (XTDB + futon1b server over HTTP), not stubs.

Review (claude-13), planted bugs against worktrees:
- (a) `ran-no-output` without an envelope accepted → caught;
- (b) a receipt naming an unknown dynamics revision accepted → caught;
- (c) an unreachable authority reported `ok` → caught (the resolver has no
  cache, so "a cache never answers for the authority" holds by construction);
- **(d) `not-run` weighed at −0.5 in the 驴 aggregate → NOT caught**: the
  test's not-run receipt carried no envelope, so its event landed under
  window `"nil"`, not the window being scored;
- (e) stale aggregate answered silently → caught.

Also found:
- **A real defect in C2's immutability check.** The SQL ASSERT compared
  `record$data` as `pr-str` text. Small Clojure maps print in insertion
  order, so an equal record built along another path (an HTTP/EDN round
  trip, a second producer) was refused as `arxana-append-precondition`.
  Reproduced deterministically for 'clusions and attempts. This was the
  cause of formal-c3b-test failing in 2 of 6 runs, and it would refuse a
  legitimate re-assert of the same claim by a second interpreter.
- External references over HTTP had no timeout: a hung authority would
  hang the read past the prompt budget.

Fixed in review (claude-13, merged):
- futon1b 1e74121: stored and compared record data is canonical EDN (map
  keys sorted at every level); new C2 check re-asserts an equal claim with
  reordered keys (fails 10/11 without the fix, 11/11 with it).
- futon3c fb41bcd: external references time out after 2 s as `:unavailable`,
  tested with a stand-in that never answers; the not-run aggregate test
  carries the scored window, and planted bug (d) is now caught.
- formal-c3b-test 5/5 clean runs; Arxana suite 94 tests, 612 assertions;
  futon1b C2 11/11, conditional 24/24, index checks pass; clj-kondo and
  check-parens clean. No live service reloaded or restarted.

## F1a — settled-turn materialiser and Joe's assessment (accepted after review fixes)

Built by codex-21: futon1b a9921be (reserved `mark-occurrence/assert`
route), futon3c 249aa138a + c9c92fe42 (materialiser `formal_f1.clj`,
private-stack test), warrant `F1a-warrant.edn` (in futon3c test
resources; it records commands and results but no shas, so the record
check used the commit trailers: all three carry job
invoke-1794122340013-15578-2c7d4ace). Four real 象 turns materialised:
settled bytes ingested, C1 golden interpretation ids reproduced,
attestations with basis `model`, mark occurrences as entities cited by the
attestations, typed external references. turn-iWVzvT keeps its rejected
`ask-action` reading and gains the `constrain` successor and a
`supersedes` link.

Review (claude-13), planted bugs in a worktree at c9c92fe42:
- (a) competing readings collapsed to one 'clusion per source fragment → caught;
- (b) verdict written onto the 'clusion instead of separate assessments → caught;
- **(c) re-planning not deterministic (assessment time = now), so a re-run
  writes a second set of assessments → NOT caught**: the test reused one
  plan for both runs;
- (d) the successor replaces the original reading → caught;
- **(e) verdicts swapped between s1 and s2 → NOT caught**: the test checked
  only that the set of verdicts was `#{:accepted :rejected}`.

Also found: the assessments did not carry Joe's judgement. Their reasons
were paraphrases ("s2 is good"), they cited no evidence, and their time was
a placeholder. DERIVE-F1 increment 7 asks for his judgement as given.

Fixed in review (claude-13, futon3c 7728f9c4c): each assessment carries
Joe's words verbatim (`:quote`, checked against the text of evidence
emacs-5d1f629e9f6fcd876a973fad309feff9), cites turn-0ozt4x and that
evidence, and has the recorded time 2026-11-08T01:18:45Z. The test re-plans
from the files for the second run, pins s1 `rejected`, s2 and s3 `accepted`
(one each), the successor unassessed, the original unchanged, and the
`supersedes` endpoints. All five plants caught; 3 tests, 30 assertions;
clj-kondo and check-parens clean. No live service touched.

Carried forward:
- codex-21's residue: the public bounded `graph/query` does not return
  assessments by subject (the test reads futon1b's `assessments-for`
  directly). F1c's views need it; it is the next packet.
- Mark locators are UTF-16 offsets of the *first* occurrence of a glyph,
  with width 1. The four fixtures have one mark each, so nothing is wrong
  yet, but a turn with a repeated mark would get two equal mark records,
  and 🈳-class glyphs are two UTF-16 units wide. F1b's relocation work must
  use codepoint ranges per occurrence.

### F1a-read — assessments through the public query (accepted)

codex-21: futon1b 3bb244c, futon3c f1cf9b639 (warrant commits a714e1d6b,
202ec32f0, `F1a-read-warrant.edn`). The warrant's shas match the diffs.
Assessments are indexed in the C3a sidecar under
`(subject, event-time, assessment-id)`, advance the source watermark like
'clusions and attestations, and `graph/query :assessments` returns every
assessment record for a subject, bounded, with no latest-wins selection.
formal_f1_test reads verdicts through this public query; every assertion
from 7728f9c4c is kept, plus conflict, limit/total, empty and stale cases.

Review (claude-13), planted in futon1b `arxana-index/assessments` with a
paired futon3c/futon1b worktree: (a) only the latest assessment returned →
caught; (b) limit ignored → caught; (c) `:complete` when truncated →
caught; (d) a scan of the store when the index is behind → caught (the
stale-index assertion). No fixes needed. A scan used on every read would
still pass these tests; the code reads the index, and C3a's scale numbers
cover the index path.

## F1b — Markdown exporter, manifest, ingest, drift check (accepted after a test fix)

codex-21: futon3c 5e5f18a3b (code), 00fad6ef5 (warrant
`F1b-warrant.edn`); futon1b unchanged (tested at 3bb244c). Warrant shas
match. A versioned exporter (`formal_f1_export.clj`, id
`futon3c.arxana/formal-f1-conversation` v1) writes the four turns as
Markdown and a canonical-EDN manifest that names the Markdown revision, the
exporter id/version/config, and codepoint ranges for each turn, each
interpretation (by 'clusion id) and each mark occurrence. Both are ingested
through futon1b's reserved revision route; re-export gives identical
revisions. Mark occurrences are now one record per occurrence with
codepoint ranges and the glyph alone (the old matcher also stored the
colon). Interpretation ids are unchanged; mark and attestation ids change.

Review (claude-13), planted in a worktree at 00fad6ef5:
(a) manifest counting UTF-16 units → caught; (b) repeated glyph collapsing
to one record → caught; (c) a timestamp in the exported bytes → caught;
(d) exporter id/version missing from the manifest → caught; (e2) stored
bytes differing from the export → caught; **(e1) interpretation ranges
shifted by one codepoint → NOT caught**: the test checked only that each
range was non-empty.

Fixed in review (futon3c 9d63e40f2): each interpretation range's text in the
Markdown must equal the fragment text recorded in the 象 analysis file, an
independent source; (e1) and (a) now fail on it.

For go-live: 5e5f18a3b changes `service.clj` so that every
`revision/ingest` with string bytes goes through futon1b's reserved
conditional route when the store supports compare-and-set, not only F1's.
The route writes first, then the in-JVM artifact provider; a provider
failure after a successful route write would leave the head ahead of the
provider. Check this before reloading the live futon3c JVM at F1c.
