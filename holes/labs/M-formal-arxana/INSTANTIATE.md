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

## F1b2 — relocation across an edited revision (accepted after review fixes)

codex-21: futon3c 0a64e20b6 (code, `formal_f1_relocation.clj`), 1586c676b
(warrant `F1b2-warrant.edn`); futon1b unchanged. Warrant shas match. One
content-addressed F0 anchor strategy per exported interpretation, captured
on the first Markdown revision with no context and no fuzzy matching; an
edited successor is ingested with the first revision as expected head;
each resolver run is appended as an evidence attestation that cites the
resolver (`arxana.resolve/anchor` v1, strategy revision) and both revision
ids. Original 'clusions and manifest ranges are unchanged. Cases: offset
shift → `resolved` at a later range; duplicated text → `ambiguous` with
both candidates; deleted text → `missing`; historical expected head →
`stale` before any finder runs; a second write on the old head refused.
Recovery across deletions is deferred (it needs edit-hunk provenance,
VERIFY S2).

Review (claude-13). Planted bugs:
- (a) relocation written as a new interpretation 'clusion instead of
  evidence → caught;
- (b) ambiguity resolved by picking the first candidate → caught;
- **(b2) the stored attestation keeps only the first ambiguous candidate →
  NOT caught**: the test checked the in-memory resolver result, never the
  stored records;
- (c) fuzzy matching on / a deleted fragment reported `resolved` → caught;
- (d) stale check removed → caught;
- (e) futon1b's head ASSERT ignoring the expected revision (paired
  futon3c/futon1b worktree) → caught.

Also found: a `missing` relocation was stored with standing
`:contradicted`. Deleted text is absence, not evidence against the
reading. And the test passed the "actual head" as a literal, so the stale
check was never fed by the authority.

Fixed in review (futon3c 05994a2ba): `missing` → standing `:unverified`,
coverage `:none`; the test reads the stored relocation attestations (both
ambiguous candidates; standing/coverage per case; source and target
revisions; no `:contradicted`) and takes the actual head from futon1b.
(b2) and a re-planted `:contradicted` now fail. 1 test, 25 assertions;
clj-kondo and check-parens clean. Golden interpretation ids: the
materialiser is unchanged, so they cannot have moved.

## Go-live facts for F1c (checked 2026-11-08)

- futon4a talks to `http://127.0.0.1:7084/arxana/v1`, served by a separate
  JVM (pid 250790, started 2026-11-07 05:32) built by
  `futon3c/src/futon3c/arxana/demo.clj` over `store/mock-store` and an
  in-memory artifact provider. It is not the 7070 Agency JVM. The mock
  store does not offer compare-and-set, so F1b's `service.clj` ingest
  change does not reach it.
- Live futon1b (:7073, pid 2589910, started 2026-11-07 01:54) predates
  every C2–F1b2 futon1b commit: it has no reserved Arxana tables for
  'clusions, evidence, assessments or marks, and no sidecar index.
- So the F1 demo in Emacs needs (i) the 7084 service running current
  master against a futon1b with the reserved route, and (ii) a decision on
  WHICH futon1b: the live corpus store, or a private store for the demo.
  Writing F1 records into the live store is permanent; Joe decides.

## F1c-1 — three JVM projections (accepted after review fixes)

codex-21: futon1b 063e2505 (public `record-by-id` read; attestation queries
accept `:subject-id`), futon3c 5b08367ff (code) and c4f43fbc0 (warrant
`F1c1-warrant.edn`). Warrant shas match. Three registered operations on the
JVM service (`formal-f1/linear`, `/outline`, `/gcc`) built only from public
reads (`entity/get`, `graph/query`, `revision/get`); `entity/get` over HTTP
had been refusing every read and is now backed by the reserved record read.
The same interpretation ids appear in all three views; every fragment's ids
resolve through `entity/get`.

The job finished at 09:10 but was marked failed at 10:16. **Cause, found in
the job's tool log:** at 08:09:51 codex-21 ran `kill 2589910`, the live
futon1b server, apparently taking it for its own stuck test run. futon1b
exited cleanly, so systemd (`Restart=on-failure`) did not restart it; it was
down until 15:18. Agency could not then write the job's clock activity
decision and failed the job, and the park wake reporting it was refused at
admission. Agency fix: futon3c 14104c3cf (decisions spool to a local outbox
while futon1b is down). Rule added to codex-21's packets: never signal a
process you did not start; check the command line first.

Review (claude-13), planted bugs:
- (c) GCC dropping the constrain successor → caught;
- (d) the projection namespace requiring futon1b → caught (structural test
  added);
- (e) model evidence rendered as an `accepted` assessment → caught after
  the test fix below;
- **(a) a fragment missing its own interpretation id → NOT caught;**
- **(b) standing showing only the latest assessment → NOT caught** (no
  reading in the fixture had two assessments);
- **(g) a truncated related read hidden → NOT caught** (the outline ignored
  read status).

Also found: GCC carried every interpretation, including propose / approve /
qualify, though DERIVE-F1 §2 item 1 limits it to action-bearing fragments;
and the "model evidence never launders" test checked a fragment chosen for
having no assessments, so it could not fail.

Fixed in review (futon3c f6c8e61da): GCC rule version 2 declares its action
intents (DERIVE-F1 §2 amendment below) and shows only those; related reads
report `:related/truncated`; tests now check that fragments name their own
ids, a truncated read is reported, a second conflicting assessment shows
beside the first, `accepted` appears only on s2 and s3, and the projection
namespace has no store dependency. All plants caught; 2 tests, 162
assertions; spine and store-http tests pass; clj-kondo and check-parens
clean.

## F1c-2 — Emacs views and private demo stack (accepted after review fixes)

codex-21: futon3c b02a4db9c (`formal_f1_demo.clj`: in-process XTDB +
ephemeral futon1b, materialise → export → relocate, Arxana HTTP surface on
127.0.0.1:7085 with occupied-port refusal) and 36d70684e (warrant
`F1c2-warrant.edn`); futon4a 814783c (`lisp/futon4a-formal-f1.el`: linear,
outline and GCC buffers; record ids as text properties; standing on three
lines; candidate / accepted / rejected faces; records-at-point and
relocations-at-point; recorded responses; pid-owned launch/stop scripts;
end-to-end script). Warrant shas match. Process safety checked first: the
job log has no kill of any process it did not start, and the :7070, :7073
and :7084 servers kept their pids (521296, 1747159, 250790).

Review (claude-13), planted bugs:
- (a) a fragment rendered without its ids → caught;
- (c) a model-only reading given the accepted face → caught;
- (d) GCC truncation not shown → caught;
- **(b) the three standing dimensions folded into one line → NOT caught**:
  the test matched substrings;
- **(e) the stop script killing by name → NOT caught**: nothing tested the
  stop script against a second same-named process. (Planting `pkill -f`
  unanchored also killed the reviewer's own shell, whose command line held
  the same string.)

Also found: a reading with both an accepted and a rejected assessment got
the accepted face, a choice with no named policy (DERIVE-core §18).

Fixed in review (futon4a 08ce812): a contested face for conflicting
verdicts, with a test; the standing test requires each dimension on its own
line; `test/stop-f1-demo-test.sh` checks that only the recorded pid stops,
a same-named unrecorded process survives, and a foreign recorded pid is
refused. All plants caught; ERT 7/7; stop test passes. Not re-run: the
JVM demo tests and the end-to-end launch (warrant shas match).

## F3a — a93J02 fixture, Lean gate, kernel evidence adapter, locators (accepted after a review fix)

codex-21: futon3c 401e41f0b (`formal_f3.clj`, `scripts/formal-f3a-lean-gate.sh`,
two new 'clusion types `[:kernel-builds 1]` and `[:declaration-location 1]`,
recorded kernel run `a93J02-kernel-run.edn`, warrant `F3a-warrant.edn`);
apm-lean pinned at 2dd18b2c (unchanged). Warrant shas match. Process
safety: no kill in the job log; :7070/:7073/:7084 pids unchanged; the
apm-lean primary checkout's local changes date from 2026-11-06, before the
job; the gate checks Mathlib package metadata before and after the run.

Delivered: Lean 4.29 (v4.29.0-rc8) builds `problems/a93J02/lean/Main.lean`
(passed), recorded with toolchain, commit, command, output digest and input
digests; the run enters Arxana as a `kernel-builds` claim plus a basis
`:kernel` attestation from the evidence adapter (a failed run is kept as
`:contradicted`). Ten manifestations ingested as separate artifacts,
including the generated HTML with its latexml_oxide 0.7.5 producer and log.
Declaration identity is the fully qualified name; ranges are
revision-specific parser evidence.

Fixture finding (codex-21): apm-lean's `informal-solution.md` is now
LaTeX-normalised and no longer byte-equal to futon3c's authored copy
(MAP-Q8 said identical). Both are kept: the historically identical copy at
apm-lean fe78c85d shares the authored revision id; the current file is its
own revision.

Review (claude-13), planted bugs: (a) two manifestations merged under one
artifact id → caught; (b) declaration identity keyed by revision → caught;
(d) a failed Lean run dropped → caught; **(c) kernel evidence written
without the adapter → nothing could catch it**: no rule enforced the
adapter path. Fixed in review (futon3c f29fa8a84): the JVM write port refuses
`basis :kernel` evidence unless the principal is the evidence adapter; a
forged-kernel test added; the guard removed, or the adapter writing under
another principal, now fails. 1 test, 22 assertions.

Note for F3b: the test reads a fixed apm-lean worktree path
(`~/worktrees/apm-lean/formal-arxana-f3a`); it should read the pinned
commit from the apm-lean repository instead, so the test does not depend
on a worktree surviving.

## F3b — a93J02 step-to-declaration correspondences (accepted after review fixes)

codex-21: futon3c 9dacc19be (`formal_f3_correspond.clj`, type
`[:correspondence 1]`; F3a now reads apm-lean bytes with `git show` at the
pinned commit 2dd18b2c, removing the worktree-path dependency) and
cfcbc9535 (warrant `F3b-warrant.edn`). Warrant shas match. Process
safety: no kill in the job log; shared pids unchanged; apm-lean still at
2dd18b2c with only its pre-existing local changes.

Delivered: 27 correspondences (9 steps × 3 candidates), each with mark8's
model attestation and the judge's model assessment (`:candidate` or
`:rejected`, never `:accepted`), plus Joe's human assessment per
candidate: accepted where the judge chose it (e1, e8, e9), rejected
otherwise, all retained. Steps with no judged match (e2–e7) are partial
coverage and ambiguous. DCT vs Abel is a separate
`:methodological-divergence` correspondence, accepted by Joe, never
contradicted.

Review (claude-13), planted bugs: (a) candidates collapsed to the judge's
pick → caught; (b2) Joe's citation removed → caught; (c) divergence
recorded as contradicted → caught; (d) judge's model verdict as accepted →
caught; (e) unaligned declaration dropped from the plan → caught; **(b1)
Joe's quote paraphrased → NOT caught**: the test compared the constant
with itself.

Also found: the divergence attestation named an invented producer
("formal-f3/divergence-adapter") with basis `:human`; the reading is
claude-13's and codex-21's (DERIVE-F3), i.e. model work. Its target was
the Mathlib lemma rather than the Main.lean declaration. Joe's time was a
second off the turn record.

Fixed in review (futon3c 6852e8299): divergence evidence basis `:model`,
standing `:unverified`, citing DERIVE-F3, targeting
`tendsto_sum_alternating_harmonic` (Mathlib lemma kept as content); time
01:27:30Z; an independent test pins the quote to the evidence text. All
plants caught; 2 tests, 168 assertions.

Carried to F3d: the unaligned declaration
`tsum_alternating_harmonic_mul_pow` and the per-step ambiguity live only in
the plan and in attestation content; they are not stored as records a view
can read (no resolution-observation 'clusions, unlike F1). F3d's views
need both.

## F3c — generated informalisation and the labelled synthetic contradiction (accepted after review fixes)

codex-21: futon3c ac85c41e5 (`formal_f3_generate.clj`; types
`generation-run`, `synthetic-mutation`, `formalization-coverage`; fixture
resources) and eda345ed3 (warrant `F3c-warrant.edn`); apm-lean witness on
branch commit 249dd03e (`problems/a93J02/lean/SyntheticContradiction.lean`,
not merged into apm-lean's primary checkout, which has someone's
uncommitted work). Warrant shas match. Process safety: no kill in the job
log; shared pids unchanged; apm-lean primary still at 2dd18b2c.

Delivered: a generated informalisation of Main.lean (model "OpenAI
GPT-5/Codex", serving snapshot not exposed; full prompt and configuration
recorded), kept as its own artifact and only a candidate correspondence; a
synthetic mutation (`log 2` → `log 3`), labelled SYNTHETIC in id, media
type and records, with its diff; a Lean 4.29 proof that the partial sums do
not tend to `log 3` (`alternating_harmonic_ne_log_three`, from the genuine
theorem and strict monotonicity of log), recorded through the evidence
adapter; codex-21's fixture-author assessment marking the synthetic
correspondence rejected / contradicted, citing the kernel attestation.
F3b's ambiguous steps and the unaligned declaration are now stored records.

Review (claude-13), planted bugs: (c) kernel evidence under another
principal → caught (by the F3a write-port guard). **NOT caught:** (a) the
synthetic artifact unlabelled; (b) the contradiction without its kernel
citation; (e) the generated text ingested into the authored artifact;
(d) the genuine divergence relabelled contradicted, which could not even
take effect because the test never stored F3b's records. The test read
in-memory values and did not check ingest responses.

Fixed in review (futon3c 3b8b78b98): the test materialises F3b first, requires
every F3c ingest to succeed under its own id, checks the SYNTHETIC label on
ids and media types, reads the stored contradiction and its citation back,
and requires the divergence to keep only Joe's acceptance; the two
placeholder times are replaced by the code commit time. All five plants
caught; 1 test, 59 assertions.

For F3d: the contradiction's `:standing :contradicted` is a field on an
assessment; the F1 views read evidence standing from attestations, so F3d
must show it explicitly.

## F3d — relocation and F3 views (accepted after review fixes)

codex-21: futon3c 4c9638986 (`formal_f3_relocation.clj`,
`formal_f3_projection.clj`, `formal_f3_demo.clj` on :7086) and 893b423e2
(warrant `F3d-warrant.edn`); futon4a 511e47b (`futon4a-formal-f3.el`,
faces incl. SYNTHETIC, records-at-point, pid-owned scripts). Warrant shas
match. Process safety: no kill in the job log; shared pids unchanged; no
leftover demo process; ports 7085/7086/7196 free; apm-lean untouched.

Delivered: an authored-prose successor via CAS; F0 relocation with
resolved, ambiguous (all candidates) and missing outcomes, stored as
attestations and read back; correspondences, Main.lean's revision and the
declaration-location record unchanged; two registered views
(`formal-f3/correspondence`, `formal-f3/manifestations`) on public reads;
Emacs buffers with the same ids. Tests read records back from the store,
as packet 14 required.

Review (claude-13), planted bugs: (c) SYNTHETIC label dropped → caught;
(d) divergence shown as contradicted → caught; **(e) a fragment without its
own id → NOT caught.** Design finding: the synthetic class and the
manifestation SYNTHETIC label came from a manifest the caller writes by
hand, not from stored records, so a wrong manifest could unlabel the
contradiction; and the view omitted the ambiguity records and the
unaligned declaration F3c stored for it.

Fixed in review: futon3c b8557381a — labels from stored records (claim
`:fixture/label`, revision media type); per-correspondence resolution from
resolution-observation records; unaligned declarations as coverage
fragments (rules v2); tests for own-id-first, a forged manifest, ambiguity
and coverage in the view. futon4a 8f69072 — the Emacs F3 view shows
resolution and the unaligned declarations, with an ERT. All plants caught
(1 test, 396 assertions; ERT F3 3/3, F1 7/7). Not re-run: the end-to-end
Emacs run (warrant). The recorded ERT fixture predates the coverage field.

**F3 is built and reviewed through F3d; its acceptance is Joe's.**

## F5a — Lean geometry and coordinate-free records (accepted after review fixes)

codex-21: futon5 95ea4435 (`holes/labs/M-formal-arxana/f5-lean/`, Lean
4.29.0-rc8, Mathlib pinned at apm-lean's 5ee50502, `.lake/packages`
symlinked to apm-lean's prebuilt packages); futon3c 71765908a
(`formal_f5.clj` gate + records, `formal_f5_query.clj`, eight
`:geometry/*` types, registered `:geometry/fibre`) and 34d69e0f1 (warrant
`F5a-warrant.edn`). Warrant shas match. Process safety: no kill/pkill in
the job log; :7073 (1747159) and :7084 (250790) pids unchanged; no file
under apm-lean `.lake/packages` newer than 17:00 (Mathlib not rebuilt);
apm-lean's dirty files date from 2026-11-06, not this job.

Delivered: `square_fibre_neg_one` ({z : Circle | z² = -1} = {i, -i}),
`inversion_is_conjugation`, no sorry; geometry entities whose identity is
`:entity/id :entity/kind :theory/version` (no coordinates); map, symmetry,
projection, preserves, satisfies, fibre-of and member relations; kernel
evidence through `formal-f3a/evidence-adapter`; `:geometry/fibre` answers
{i, -i} from stored member records and cites the kernel attestation.

Review (claude-13). Findings:
- The cube fact was not proved: the file had `card (rootsOfUnity 3 ℂ) = 3`
  and a negation equation, never connected to cubing on the circle.
- The gate passed any exit-0 run, and Lean exits 0 on `sorry`.
- The test read the Lean file from codex's worktree, and only from a
  recorded run, without checking the run covered those bytes.
- Planted bugs: (a) coordinate in point identity → caught (records refuse
  it); (b) fibre returned as a fixed {i, -i} → **NOT caught**; (c) kernel
  evidence from another principal → caught; (d) sorry in the Lean file →
  **NOT caught**; (e) Mathlib rebuilt → only the recorded flag was tested.

Fixed in review: futon5 582d7a8 — `cube_fibre_neg_one_card`:
{z : Circle | z³ = -1}.ncard = 3 (via `nthRootsFinset` and unit norm of
every cube root of -1). futon3c c8dfd92a6 — the gate's outcome needs exit
0, a sorry/admit-free source and output, and unchanged Mathlib metadata;
kernel run regenerated (pass, empty output, Mathlib unchanged); the test
reads the futon5 checkout, checks the run's input sha equals those bytes
and that the cited theorems exist, and a planted extra member record must
appear in the fibre answer; a sorry unit test. Plants (b) and (d) now
caught (2 tests, 49 assertions).

Left for F5c: `:geometry/fibre` resolves only `:square` over `:neg-one`
(it recomputes those ids from constants); the cube map needs the query to
find the map by stored records. The Mathlib-rebuilt plant has no automated
test (it would need a fake `lake`); the gate compares path/size/mtime
metadata of the whole package tree before and after.

## F5b — two-panel geometry projector (accepted after review fixes)

codex-21: futon3c 562799e55 (`formal_f5_render.clj`, `formal_f5_projector.clj`,
`formal_f5_demo.clj` on :7087; types `[:geometry/value 1]` with exact
rational turns and `[:geometry/placement 1]`; registered `:geometry/render`)
and 068623e04 (warrant `F5b-warrant.edn`); futon4a c1837bf
(`futon4a-formal-f5.el`: EWW and external-browser open; pid-owned
launch/stop scripts). Warrant shas match. Process safety: the job log has
only `kill -0` probes; :7073 (1747159) and :7084 (250790) unchanged;
nothing on :7087.

Delivered: one deterministic page with a Cartesian circle and an angular
projection (cut at 0 turns, 2θ shown per point); both read only through
`:geometry/fibre`, entity/get and graph/query; positions computed from the
stored `geometry/value` turns; panel-local offsets as placement records;
record-id lists under each panel for EWW.

Review (claude-13). Findings:
- **The current placement was picked by sorting content ids**, so a second
  drag of the same point could be ignored. futon1b's role-reverse index
  orders by the record's `:event-time`, which placements did not carry.
- The "drag changes no geometry id" check compared the in-memory plan with
  itself.
- Planted bugs: (a) renderer drawing a fixed subset of points → **NOT
  caught**; (b) a Cartesian placement leaking into the angular panel →
  caught; (c) placement ids entering the geometry id set → caught after the
  fix; (d) panels listing different id sets → caught; (e) unsorted output →
  not a reachable bug (store order is deterministic; the write-order test
  below covers order dependence); (f) stop script killing by name → **NOT
  caught** (the test only had a foreign recorded pid).

Fixed in review: futon3c 6be9ab21b — a placement's `:placed-at` is part of
its identity and its `:event-time`; the renderer takes the latest in
ascending time (`geometry/placement` revision 1 amended in place: it was
never written outside test stacks); tests for a second drag winning, ids
compared from rendered output, every member drawn in both panels, and the
same bytes when records are written in the opposite order; plant "no
event-time" caught. futon4a d7584c8 — stop test with a same-name twin (as
F1); an ERT rendering each panel through shr checks every record id is in
the text. All plants caught (renderer 2 tests, 107 assertions; F5 ERT 2/2;
C1 + F5a 9 tests, 158 assertions). Not run: the launch script end to end
(warrant's HTTP smoke on :7198).

Left for F5c: `:geometry/fibre` still resolves only `:square`/`:neg-one`.
The recorded futon4a page fixture predates the placement change (it has no
placements, so its content is unaffected).

## F5c — square → cube, end to end (accepted after review fixes)

codex-21: futon3c b8a573214 (maps and values found through stored
`geometry/named` records; `geometry/fibre-of` revision 2 naming its Lean
declaration and cardinality; `geometry/selection`; `:geometry/select`;
cube fibre at exact turns 1/6, 1/2, 5/6; typed unknown-map/value and
cardinality-mismatch refusals; the Lean gate run fresh in the test and in
demo preparation) and 8f9fe20ba (warrant `F5c-warrant.edn`); futon4a
2ada0ccf (`futon4a-formal-f5-select-map`). Lean source futon5 582d7a8.
Warrant shas match. Process safety: only `kill -0` probes; :7073 and :7084
pids unchanged; nothing on :7087; nothing under apm-lean `.lake/packages`
newer than the job.

Review (claude-13). Findings:
- The angular panel's exponent came from `(case map-id :square 2 :cube 3)`,
  a renderer table keyed by name rather than the stored map.
- Selection times were integers (the seed 1000; Emacs sent epoch ms).
  futon1b orders `:event-time` as text, so differing widths order wrongly.
- The current selection and placement were the last of the *oldest* 200
  records by time (ascending query with a limit).
- Planted bugs: (a) map resolved from a constant → caught; **(b) the cube
  fibre returned as a fixed three → NOT caught** (the store held exactly
  those three); (c) cardinality mismatch not refused → caught; (d) one
  panel only → caught; (e) stale Lean run → caught (input sha); (f) the cube
  reusing the square's fibre → caught.

Fixed in review: futon3c 5f42d4c1c — the exponent read from the stored
`:map/definition`; `:geometry/select` refuses anything but
`yyyy-MM-ddTHH:mm:ss.SSSZ`; newest-first queries for selection and
placement; tests for exponents 2 then 3, every pre-switch record unchanged
and the square fibre still answering, an epoch time refused, an
earlier-timed later write not winning, switching back, and a second store
whose cube fibre has other members. Plants (b), "exponent by name" and
"oldest first" are caught. futon4a 6a71c03 — Emacs sends the ISO form,
pinned by an ERT. C1 + F5a/b/c + spine: 18 tests, 469 assertions; F5
ERT 4/4.

Not changed: `geometry/placement`'s `:placed-at` format is not validated,
since placements have no write operation of their own. Member lists over
200 would be truncated (the cardinality check would refuse such a fibre).

**F5 is built and reviewed through F5c; its acceptance is Joe's.**

## F2a — patches, sequencer, conflicts (returned for rework as F2a-2)

codex-21: futon3c ef4cdbf52/4d9ab4f0b/3d3b586ec (spike S2 rebase promoted to
`text_rebase.clj`), d37b1e2e4 (`formal_f2.clj`; types `text/patch`,
`text/conflict`, `text/alternative`, `text/choice`; `:text/submit-patch`,
`:text/choose`), e25a0bd54, warrant 42df1dcf7. Process safety: no kill in
the job log; :7073/:7084 pids unchanged; no leftover test JVM.

Review (claude-13). The rebase module is sound: plants (a) partial overlap
as disjoint, (b) alternatives sharing one side's bytes and (f) UTF-16
offsets were all caught (text-rebase + F2 tests).

Findings that send it back:
- The sequencer decides from an in-JVM atom (heads, which patch moved base
  to head, conflicts, alternatives). Only patches/conflicts/alternatives/
  choices are stored; the base→result application is not. After a restart
  or from another instance, stale patches cannot rebase and stored
  conflicts cannot be chosen: a second sequencer beside futon1b's CAS.
- Rebase works only when the base is the head's direct parent; a writer two
  revisions behind is refused. That would decide F2's offline-replay test
  (the CRDT question) by an implementation limit.
- Tests read the head from the atom; the third-writer race is a stub of
  ingest; after a choice nothing checks the head's text.
- 3d3b586ec deleted `SPIKE-f2-rebase.md`, which VERIFY.md cites.

Re-dispatched as packet 19 (F2a-2): application records, head from futon1b,
rebase through the stored chain from any ancestor with a typed chain-gap
refusal, conflicts recording the head they were computed against, choose
from the store with CAS, tests with two instances sharing only the store.

## F2a-2 — stored sequencer (accepted after review fixes)

codex-21: futon1b 2213797 (`:artifact-head`, `:revision-by-id` reads),
futon3c ec6cc6123 (`[:text/application 1]`; head and bytes from futon1b;
breadth-first walk of stored applications from any ancestor, composed;
typed `:arxana.text/application-chain-gap`; conflicts carry
`:computed-against/head-revision-id`; choose from stored records with CAS;
`SPIKE-f2-rebase.md` restored under `docs/arxana/`), warrant eaff10141
(shas match). Process safety: no kill/pkill in the job window of the
Codex log; :7073 pid 1747159 and :7084 pid 250790 unchanged; the one new
JVM (giraffe reread-cli) is a systemd timer job, not the agent's.

Review (claude-13). No atom remains in `formal_f2.clj`. Plants, each run
against the F2 tests:
- (a) head cached in a namespace atom, updated by submit! — caught (20 failures);
- (b) chain walk limited to one application — caught (three-behind case);
- (c) choice writes no application record — caught;
- (d) choice ingested against the current head with no stale check — caught
  by the stale-choice test; with the stale check kept and only the CAS
  target re-read, it was NOT caught (no choice race test);
- (e) chain gap treated as no chain — caught (raw-ingest gap).

Fixed in futon3c 6e2a2881c:
- The choice's application record was a whole-text replacement, so any
  older offline patch would conflict with it. It now records the minimal
  differing codepoint range; a test reads the stored hunk back.
- `choose!` takes the same `:text/before-cas` seam as `submit!`; a test
  lands a write between the head check and the CAS, and the choice is
  refused with the head left at the concurrent write. Plant (d') is now
  caught.
- `text_rebase/compose` refuses a later change that edits inside text an
  earlier change inserted. A stale patch crossing such a chain came back
  as `:arxana.text/invalid-patch`. It now gets
  `:arxana.text/chain-not-composable` with the stored patch id; a test
  pins this for an offline patch made before a conflict and its choice.

Focused: formal-f2, text-rebase, formal-c1 — 15 tests, 3312 assertions,
0 failures; clj-kondo and check-parens clean.

Open, for F2b: (1) chains that edit inside earlier insertions cannot be
composed, which includes every choice — the offline replay test must
either compose them or show why it cannot; (2) a conflict reached across
several applications names only the last change as its left side;
(3) a crash between a successful CAS and the application write leaves a
head with no application, which later stale patches meet as a chain gap.

## F2b-1 — offline replay; the CRDT question (accepted after a test fix)

codex-21: futon3c d492ce558 (`text-rebase/compose-all`: composition over
symbolic codepoints anchored on surviving original characters, total for
valid sequential patches including edits inside earlier insertions;
far-apart hunks stay separate; each hunk carries `:source-op-ids`;
`[:text/conflict 2]` with `:conflicting/change-ids`;
`docs/arxana/F2b1-offline-replay.md`), warrant 18aa2cdd5 (shas match;
futon1b unchanged at 2213797). Process safety: no kill/pkill after 19:09
in the Codex log; :7073 pid 1747159, :7084 pid 250790 unchanged; no test
JVM left.

Experiment (two service configs sharing one store): 11 online applications
from two authors, including an edit inside an earlier insertion, a deletion,
a conflict and its choice; 5 offline hunks from the original head, replayed
as one patch and as five. One patch: a conflict whose two alternatives each
carry the four non-conflicting hunks. Five patches: 4 rebased, 1 conflict
with 2 alternatives. Lost intent: 0.

**Verdict (DERIVE-F2 clarification 4): a single CAS sequencer suffices for
offline replay; no intent was lost, so the CRDT re-arm condition was not
met.** Open: a crash between a successful CAS and the application write
leaves a head with no application (later stale patches get the typed
chain-gap refusal).

Review (claude-13). Plants: (a) composition collapsed to one anchor — caught
(11 failures, property and replay); (b) a hunk inside an earlier insertion
dropped — caught (108); (c) attribution to the last change only — caught.
Not caught before my fix: (c2) attribution to every change in the chain
(the test used `some`), and (d) alternatives missing the non-conflicting
offline hunks (the one-patch case checked only `[OFF-4]` in some
alternative) — so the "lost intent: 0" claim was not pinned for the
one-patch form. futon3c b8c0b21d4 requires every alternative to carry
`[OFF-1]` `[OFF-2]` `[OFF-3]` `[OFF-5]` and the conflict to name exactly
the deletion; both plants now fail and the clean run passes (formal-f2,
text-rebase, formal-c1: 17 tests, 3960 assertions).

## F2b-2 — semantic markup follows collaborative edits (accepted after a fix)

codex-21: futon3c d1f0728ba (`formal-f1-relocation/relocate-application!`,
called synchronously by `:text/submit-patch` and `:text/choose` after the
application record is stored; reuses the F1b2 exact-anchor strategies, no
second resolver; attestations name original, previous and target
revisions, the application and its hunks, and every candidate;
`docs/arxana/F2b2-semantic-relocation.md`), warrant f72e761b8 (shas match;
futon1b unchanged). Process safety: no kill/pkill after 19:28; :7073,
:7084 and the :7085–:7087 demos unchanged; no test JVM left.

Outcomes as documented: untouched scopes `resolved` at the shifted range;
an edited scope `:missing` with the responsible hunks kept as evidence (the
hunks are recorded, not used to guess a successor); a deleted scope
`:missing` with the interpretation unchanged; pasted copies `:ambiguous`
with both candidates; after a choice, relocation targets the chosen head.

Review (claude-13). Plants: (a) relocation evaluated against the previous
revision but labelled with the new one — caught; (b) `:missing` reported as
`:resolved` — caught; (c) ambiguity cut to the first candidate — caught;
(d) attestation naming the old revision — caught; (e) relocation from RAM —
no atom in the path; it reads revisions, manifest and records through
`bounded-query!`/`get-entity`.

Fixed in futon3c 6ac73ec39: relocation runs after the CAS and the
application record, but a relocation exception was caught by
`submit!`/`choose!` and returned as an error although the head had moved.
It is now reported beside the new head (`:relocation {:result/status
:failed ...}`); a new test makes the resolver throw and reads the moved head
and the application back (4 failures + 1 error before the fix). The
untouched-scope test now also checks the relocated range holds the scope's
own text, shifted by the inserted line. Focused: 20 tests, 4067 assertions.

Open: nothing re-runs a failed relocation yet (a fresh instance can, from
the stored application); `relocate-application!` is wired to the F1
conversation artifact id only.

## F2c — Emacs collaborative client (accepted after substantial review fixes)

codex-21: futon3c 60cf93967 (`formal-f2-demo`, private stack that
materialises the F1 records and ingests the conversation), warrant
a20fdb1e0/3f7ef2efc (shas match); futon4a e147ba1/b7d56b2/18e4ebf
(`futon4a-formal-f2.el`, `launch-f2-demo.sh`/`stop-f2-demo.sh` with a
same-name-twin test, ERT, an e2e script). Process safety: the only `kill`
strings after 19:47 are the stop script's text being written; all shared
and demo pids unchanged.

Review (claude-13). The e2e covered only two clients merging, checking that
both strings appeared somewhere. Driving more clients against a private demo
found:
- **Scopes were never drawn.** The client read `location → start`; the
  resolver nests `location → range → start`, so every scope became an empty
  range. The ERT stubbed the record shape and passed.
- **Choosing could not work.** `special-mode` kills buffer-locals, so the
  conflict buffer forgot its conflict: `futon4a-formal-f2-choose` always said
  "No current conflict".
- After a choice, the editor that met the conflict kept its losing edit on
  the old base (the next C-c C-c would resubmit it).
- Alternative text was not UTF-8 decoded (mojibake; mismatch with the head).
- A missing scope has no place in the new text; nothing showed it.
- Plant (b), submit against the current head instead of the buffer's base,
  passed the e2e.

Fixed in futon4a 3d5e597: range nesting; conflict context survives
`special-mode`; the originating editor reloads at the chosen head; decoding
in one place; header line counts resolved/ambiguous/missing and
`futon4a-formal-f2-missing` lists them; linear diff. The e2e (ephemeral
port, futon3c master, head via `revision/get`) now pins exact text after a
rebase, scopes drawn resolved, an edited scope counted and listed missing,
a conflict leaving the head and the losing client's edit alone, and the
choice making the head the alternative's text and reloading the client.
Plants (b) wrong base, (c) auto-choice (invisible to the head check, since
re-ingesting the landed side gives the same content-addressed id; caught by
the losing client's buffer), and the `special-mode` reset each fail; (a)
UTF-16, (d) failed relocation shown wrongly, (e) previous revision's
attestations are caught by ERT (4/4).

Demo: F2 on 127.0.0.1:7088 (pid 92211, scope `futon4a-f2-demo.scope`).
Readings appear after the first landed edit: the initial export revision
has no relocation attestations (header says so).

## F2d — VS Code / code-server second writer (accepted)

codex-21: futon6 99606aca3 (`arxana.f2Open`/`f2Submit`/`f2Choose`,
`lib/f2.js`, a fixture captured from a live private relocation record,
`scripts/f2-cross-client.{js,sh}`), warrant futon3c f3affa115 (shas match;
futon3c/futon4a bases 3f7ef2efc/3d5e597). Process safety: no kill commands
after 20:14; all shared and demo pids unchanged; code-server :8097
(pid 1773140, since Nov 7) not restarted.

Review (claude-13). It took the F2c traps on board: offsets read from
`location → range`, missing scopes listed in the status item, the
originating document reloaded after a choice, exact-text comparison. Plants
against `node --test` (35 tests): (a) UTF-16 offsets sent as codepoints,
(b) the hand-written `location → start` shape, (d) a conflict
auto-resolved, (e) a failed relocation shown as landed — each fails one
test. (c) submit against the current head and (f) no reload after a choice
live in `extension.js`, which no test reaches without a VS Code host; read
and correct (`patch.base = state.baseRevision`; `refreshF2(f2Conflict.editor)`
after a choice). The cross-client acceptance (VS Code library + batch Emacs
on an ephemeral demo) passed again here with the warrant's exact head
sha256 f002851c…

Changed in futon6 7078b8d: the extension reads the F2 manifest from the
demo's ready file when the setting is empty, as the Emacs clients do.
Installed into the Arxana demo code-server (:8097, extensions dir
`/tmp/arxana-cs-demo/ext/`, previous copy kept under
`~/notes/claude-13/`); a window reload picks it up.

## F2 — summary for acceptance

Two clients (Emacs, VS Code/code-server) edit one F1 conversation through
one sequencer (futon1b CAS on the artifact head). Stale edits rebase through
stored application records from any ancestor; overlapping edits become
stored conflicts with every alternative, chosen only by an attributable
`text/choose`; F1 scopes relocate onto each new head through F0
(resolved / missing / ambiguous, with the responsible hunks as evidence);
offline replay lost no intent, so the CRDT re-arm condition was not met
(F2b-1). Demo: F2 on :7088 with `futon4a-formal-f2` in Emacs and the
`Arxana F2:` commands at /arxana-code/. **F2's acceptance is Joe's.**

## F4a — typed recursive composite, stage-local criticism (accepted)

codex-21: futon3c 2e642f30c (`formal_f4.clj`: intent / candidate / decision
stages over the F1 ask-action turn, each its own 'clusion carrying the D6
record and id; typed links `[:d6/intends 1]`, `[:d6/compiles-to 1]`,
`[:d6/decides 1]`; `[:d6/composite 1]` with role ports restricted to
declared type revisions, a dynamics reference `formal-arxana/d6-compose` v1
and topology evidence by path+sha; `[:d6/composite-group 1]` nesting;
`write/validate-composite!` before persistence; public `:formal-f4/composite`
standing view), futon5 79c20f9 (a minimal F4 diagram: the M-aif-tokamak
diagrams wire signals, not typed record ports), warrant 1b44bb9c3 (shas
match). Process safety: no kill commands after 20:29; all shared and demo
pids unchanged; no test JVM left.

Review (claude-13). Plants: (a) composite type without typed ports — caught
(4 failures); (b) port type revision unchecked — caught; (e) a stage's view
reading another stage's assessments — caught. (d) is covered by the
byte-identical read-back of the source claim, every stage, link and the
composite after a rejection and a contradiction. Ports are matched to roles
by position, which is sound: `records/clusion-key` refuses endpoints out of
declared role order (probed). Cycles: ids are content hashes and a port
target must already be stored, so no write can close a cycle; a forged
self-reference through the public port is refused as
`:arxana.clusion/id-mismatch` before the cycle check (probed). The existing
cycle test calls `validate-composite!` directly with that forged record;
futon3c 6b2f2bbda adds the public-port case. The VERIFY "Recursive schema
(ports)" decline is now discharged: ports are typed.

## F4b — bounded execution behind capability + admission (accepted after a fix)

codex-21: futon3c e8b727849 (`:formal-f4/execute`: a role-reverse
`graph/query` on `[:d6/intends 1]` capped at 10, dynamics
`formal-arxana/bounded-read` v1; `[:d6/capability 1]`, an `:admitted`
decision as a NEW decision stage (F4a's `:deferred` one read back
unchanged); a §13 attempt receipt for every call, the D6 applied receipt
only on success; `[:d6/receipt-stage 1]`; `[:d6/composite 2]` naming the
F4a composite as predecessor), warrant 0d9d1e131 (shas match). Process
safety: no kill commands after 20:39; all shared and demo pids unchanged.

Review (claude-13). Capability and admission are read from the store by id
(the request body carries only ids; the schema refuses other fields).
Plants: capability unchecked (11 failures), admission unchecked (5), a
refused attempt without a receipt (7) — caught; deferred-only, wrong
principal/dynamics, admission for another candidate are explicit cases;
the D6 receipt constructor refuses predicted effects; the F4a composite and
deferred decision are read back byte-identical.

Fixed in futon3c c4d57d85b — **tripwire 4 was not enforced**: the stale
check compared the caller's expected revision with the candidate's own
precondition, never the artifact head, and the candidate was grounded on
the claim's logical `<turn>@settled` id, which is never a head. Now the
candidate is grounded on the turn's stored byte revision, and the executor
reads that revision's artifact head from the store at call time; a test
moves the head after admission and executes with the old revision —
refused, receipted, no output (planted "stored head ignored": 3 failures).
22+1 tests green (formal-f4 92 assertions, formal-c1).

**Found, pre-existing, open:** `service/ingest-revision` sends a revision
through futon1b's CAS only when `:bytes` is a string; a byte array takes the
provider/entity path, whose head `:revision/head` (futon1b) never sees.
F1's materialiser passes byte arrays, so F1 turn revisions never reach
futon1b's head table. Needs its own packet (F1 materialisation and demos
depend on the current behaviour).

## F4c — Emacs composite view on the F4 demo (accepted after a test fix)

codex-21: futon3c 1b76e8a4d (`:formal-f4/composite` gains `:predecessor/id`
and an `:attempts` history read by role from the root composite;
`formal-f4-demo` seeds a rejection on the candidate, a contradiction on the
intent, one refused and one successful execute, on loopback 7089),
warrants a0c323a40/5ec2bac81; futon4a abc7a36 + 84d0ce9
(`futon4a-formal-f4.el`: a line per stage with stage-local faces, attempt
history, RET → `entity/get` on the projected id, predecessor toggle,
confirmed execute that rerenders; pid-owned launch/stop scripts and a
twin/foreign-pid stop test). Warrant shas match. Process safety: no kill
commands after job start (20:57); all shared and demo pids unchanged; the
ephemeral test stack was stopped.

Review (claude-13). The first ERT ran against a real private stack, but its
regexes matched across lines (`:candidate\(.\|\n\)*verdicts`), so two plants
survived: a contradiction attached to the candidate instead of the intent,
and a refused attempt drawn in the success face. Fixed in futon4a b9999d4:
each stage's detail line and face are read on their own; attempt-line faces
are checked per outcome; RET must fetch the projected id and a candidate
record; the predecessor toggle must land on the ready file's F4a id; a
refused execute from Emacs (ready file pointed at the deferred decision) must
leave the buffer unchanged and say `not-admitted`; a confirmed execute must
add a `:ran-with-output` attempt. Plants, all caught against the real stack:
(a) contradiction on the wrong stage, (b1) refused attempt in success face,
(b2) refusal rendered as a result, (c) hand-made record id, (d) predecessor
toggle opens the successor, (e) `special-mode` after the buffer-locals,
(f) attempts read for the successor instead of the root.

Seen, not changed: the source-act stage shows `verdicts=(:rejected)` — that
is F1's own stored assessment of the interpretation, not demo seeding. The
attempt history is keyed by the F4a composite, so the predecessor view also
lists attempts (receipt stages appear only in successors).

Demo started for Joe: `systemd-run --user --scope --unit=futon4a-f4-demo
setsid bash scripts/launch-f4-demo.sh` from futon4a master, futon3c
5ec2bac81, port 7089, ready file `/run/user/1000/futon4a-f4-1000/ready.edn`.
Batch smoke (open, RET on candidate, toggle) read-only; execute left for Joe.

## Byte-array revisions through futon1b's head CAS (accepted)

codex-21: futon3c 2390fcc03 (`service/ingest-revision`: when the store has
`:entity-compare-and-set`, a byte array is decoded with a strict UTF-8
`CharsetDecoder` (REPORT) and sent through the same conditional write as a
string; malformed bytes are refused `:invalid`
`:arxana.service/ingest-bytes-not-utf8` before the route; `:duplicate?`
read from the route response; the F4 test and demo seed bytes as F1 does),
warrant 10616bc63 (shas match). Process safety: no kill commands after job
start; all shared and demo pids unchanged; nothing written to :7073.

Contract found: futon1b treats nil and `:absent` alike (assert no head); a
replay with the same message id returns the original outcome with
`:duplicate? true` and does not move the head; a different message id with
`:absent` after creation is `:stale` with the current head.

Review (claude-13). The new `bytes_cas_test` runs over a real futon1b http
store and reads heads through `:revision/head`; F1 materialisation makes
each turn's byte revision the head, and a second run is all duplicates with
heads unchanged. Plants, all caught: (1) byte arrays bypassing CAS (6
failures), (2) lossy `new String` decoding (6), (3) invalid bytes falling
back to the non-CAS path (2), (4) the duplicate flag read from the wrong
map (2).

Seen, not changed: F1's materialiser still recovers a stale ingest by
reading `[:error :error/detail :current/revision-id]`; the CAS refusal
carries the head under `:futon1b/error :context :current-head`, so that
branch no longer fires. Replays use stable message ids and come back as
duplicates, so nothing reaches it today. The running demos (:7085–:7089)
still run the pre-change code until restarted.

## F6a — argument records; move vs re-support (accepted after a test fix)

codex-21: futon3c 84023b732 (`formal_f6.clj`; types `[:f6/item 1]`,
`[:f6/contains 1]`, `[:f6/supports 1]` (roles `supported-claim`,
`supporting-content`; method, evidence, actor, reason, time),
`[:f6/re-supports 1]` (successor, predecessor, actor, reason),
`[:f6/placement 1]` (zone, order, offsets; `:placed-at` in identity and
event-time); operations `:formal-f6/argument`, `:formal-f6/move`,
`:formal-f6/re-support`), warrant 80b220b11 (shas match). Fixture: the F5
unit circle: two claims (square fibre, cube fibre), the power-map
definition, the diagram (naming the stored F5 panels and scene), the cut
sidenote; three supports. Process safety: no kill commands after job
start; all shared and demo pids unchanged.

Review (claude-13). Move writes only a placement; the diagram record and
the supports set read back identical. Re-support to the current claim is
idempotent (nothing minted). Layout keys inside item content are refused.
Plants caught as delivered: a move that mints a supports record, the old
support hidden from the read, no layout check, re-support attributed to the
fixture instead of the caller. Two survived:
- **current placement chosen by id order** (ascending) passed — the
  delivered test would catch it or not depending on content hashes;
- **the old support still marked current** passed — the test checked only
  that the new one was current.

Fixed in futon3c c2f54a4a2: the current placement is checked after each move;
a fourth, back-dated move (written last, placed earlier) must not become
current; the old support must be non-current and the new one current; the
`re-supports` record is read back with successor, predecessor, actor and
reason. Plants now caught: id order ascending and descending, latest
written, old support still current, no supersession record.

Seen, not changed: "current support" is one per supporting item (latest by
time), so an item cannot currently support two claims; re-support ignores
`:argument/id` (it is allowed in the body but not checked against the
items). Both are fine for the fixture; F6b's page shows one current
support per item.

## F6b — the argument page, in Emacs, on :7090 (accepted after two rounds and review fixes)

Round 1, codex-21: futon3c 9591aecb4/94d86aa76, futon4a 4a02f9e. Sent back
(packet 30): the diagram was a hard-coded SVG of two dots, not F5's render;
the ERT stubbed the request function with a hand-written shape (the F2c
failure); the only real-store page test checked that ids appeared
somewhere; the Emacs commands asked Joe to type sha256 record ids; the
outline listed zone and id only. The stop scripts and their twin test were
good, and `:formal-f6/page` read only through `:formal-f6/argument`.

Round 2, codex-21: futon3c 8a15144cb (the page asks the public
`:geometry/render` for the panels the diagram item names and embeds its SVG
and record ids; numbered support marks naming the claim by text; former
supports carry the `re-supports` actor and reason) and warrant 37d0366e1;
futon4a c773da3 (`formal-f6-e2e.sh` runs ERT against a private stack on a
free port ≥7199; outline grouped by zone with kind and text and the support
history; `RET` shows the record; move toggles the diagram; re-support offers
claims by text and asks for a reason). Warrant shas match. Process safety:
no kill commands after job start; all shared and demo pids unchanged.

Review (claude-13), fixed:
- futon3c 4becb4dbf — the write-order check re-asserted records already in
  the store, which writes nothing; now two stores get the records in
  opposite orders and must give the same bytes. Nothing checked the diagram
  was F5's render; now its block must contain F5's own cartesian and
  angular SVG and every record id `:geometry/render` names. Column order
  is checked after a move that makes id order and placement order differ
  (the fixture's ids happen to sort in placement order).
- futon4a e886b47 — JSON `false` reaches Emacs as `:json-false`, which is
  non-nil, so the outline labelled every support "current"; the ERT now
  requires a former-support line, checks the diagram changes zone and
  nothing else moves, and re-supports to each claim in both orders,
  reading the diagram's single current support back through the argument
  view.
- futon4a c497d62 — `futon4a-arxana--post` left the HTTP body as raw bytes,
  so every non-ASCII string reached every view mangled ("S¹" in the
  definition); the client now decodes UTF-8 before parsing (F2c had worked
  around this per view). Outline support lines name the supporting item.
  futon4a unit ERTs (arxana, F1, F2, F3, F5) still pass.

Plants caught: fixed SVG; diagram kept in main; former support drawn
current; order ignored (by id) and unsorted; argument id lost to
`special-mode`; move to the same zone; re-support ignoring the chosen claim
(first or last); `:json-false` read as true; UTF-8 not decoded; ERT with
no demo running. Not a bug: dropping the sort gives the same bytes from
both stores (the store's order does not depend on write order).

Seen, not changed: the page renders F5's default map (square) for the
diagram, whichever claim it currently supports.

Demo started for Joe: `systemd-run --user --scope --unit=futon4a-f6-demo
setsid bash scripts/launch-f6-demo.sh` from futon4a master, futon3c
4becb4dbf, port 7090. Read-only smoke: outline and page text.
