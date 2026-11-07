# M-formal-arxana MAP — Q7 Convergence policies

Surveyed 2026-11-07 (zai-10). Read-only: file reads + greps only; no live
probes needed (all policies are visible in code and tests).

## Answer (≤15 lines)

There is exactly one compare-and-set policy in the stack — the futon1b
conditional arxana route (XTDB SQL ASSERT on head + message-id idempotency,
atomic whole-transaction abort, stale-refusal 409) — and it is
implemented-and-exercised. Everything else converges by *not conflicting*:
exclusive-create files (象 turn records and analyses: a colliding write is
refused or a new id is minted, so records coexist rather than merge),
append-only ledgers (write-log, journal), and unique-name allocation
(Emacs `make-temp-file` turn records). The one true last-write-wins surface
is the generic futon1b entity/hyperedge write: POST replaces the whole
document at a stable id with no expected-revision check — the policy Emacs
Arxana inherits, since its persistence layer is exactly that POST /entity.
No CRDT or OT implementation is present or vendored anywhere; the only
mention is the deliberate negative in `futon1b_arxana.clj:22` ("No
sequencer, no CRDT: atomicity stays in the store").

## Evidence

### futon1b (entity PUT/store, port 7073)

- **Generic entity write = whole-document last-write-wins.**
  `write-entity!` builds a fresh doc from the payload and puts it
  (`futon1b/futon1b_graph.clj:595-606`; doc built in `build-entity`
  :554-592 — a new map, not a merge). Same route: `POST /api/alpha/entity`
  (`futon1b_server.clj:1048-1055`). No expected-version field exists in the
  payload schema (searched `expected` in futon1b_graph.clj write path: only
  the arxana envelope has one). Absent ids are resolved by name+type
  (`ensure-entity-id`, futon1b_graph.clj:546-568), so same-name same-type
  writers converge onto one id → silent overwrite.
- **Reserved-namespace refusal.** Generic writes refuse `arxana/*` types
  (`gate-reserved-namespace!` at `futon1b_graph.clj:573-575` and the
  hyperedge builder `futon1b_server.clj:119-121`); retraction likewise
  (`futon1b_graph.clj:449`).
- **Batch entity write** is all-or-nothing at validation, but commit is
  "indeterminate on error, read back by id" (docstring,
  `futon1b_graph.clj:606-628`) — no CAS, read-back verification only.
- **Hyperedges coexist by construction**: stable id `hx:<type>:<sorted
  endpoints>` = idempotent upsert (API-CONTRACT.md:407); no-op write guard
  skips identical re-puts (API-CONTRACT.md:413); minted acts refuse an
  idempotency key naming a different act — `:idempotency-conflict`
  (`futon1b_server.clj:305-327`).
- **Write log is append-only** (locked, one EDN map per line,
  `futon1b_write_log.clj:34-40`).
- **JVM Arxana conditional route (CAS + sequencing):** one XTDB tx does
  ASSERT head (or ASSERT NOT EXISTS), ASSERT no stored outcome for the
  message id, put immutable content-addressed revision, move head, append
  journal + outcome (`futon1b_arxana.clj:260-297`). ASSERT failure aborts
  the whole tx; loser gets a stale refusal naming current head, submitted
  base and recovery choices (`stale-refusal`, :222-237; catch path
  :298-315); duplicate message id returns the stored outcome with
  `:duplicate? true` (:242-247, :299-305). Header comment: "No sequencer,
  no CRDT" (:22).
- **Exercised:** `futon1b/test_arxana_conditional.clj:80-111` — two
  concurrent writers on one expected head, exactly one commits, loser is
  409 `:arxana/stale-head`; retry-by-message-id duplicate test :111-113 and
  :202-211.

### 象 turn store (futon3c JVM + Emacs, same file layout)

- **Turn records: exclusive creation, never overwritten.** Random id via
  `Files/createFile`; a colliding id is retried up to 20 times
  (`futon3c/src/futon3c/xiang/turn_store.clj:84-99`). Deterministic-id
  variant returns the existing record, `:created? false`, never replaces
  (:101-113). Writes atomic (temp file + ATOMIC_MOVE, :64-78).
- **Analysis: publish-once refusal.** `CREATE_NEW`; second publication
  throws `{:reason :analysis-exists}` (:141-156). Exercised:
  `futon3c/test/futon3c/xiang/turn_store_test.clj` `analysis-is-published-once`
  (:32), `ids-are-validated-before-they-touch-a-path` (:26).
- **Record updates: whole-file replace (last-write-wins within a record),
  no CAS** — `update-record!` read→f→write-json! (:121-129). The `happened`
  note and status flags attach through it (http.clj:12421-12430).
- **Drafts / pattern candidates / quotes: freely replaceable by design**
  (docstrings, :160-195) — deliberate coexistence of authority (analysis)
  vs recompute (draft).
- **Emacs writer:** unique filename per turn via `make-temp-file`
  (`futon3c/emacs/session-turn-analysis.el:473-475`) — append-only by
  unique name; analysis completion refuses accidental replacement by
  exclusive hard-link creation (`scripts/session_turn_analysis.py:397-405`).
- **Conformance:** `packages/turn-seam/conformance.py` tests the
  JVM-writes-what-Emacs-writes claim (turn_store.clj docstring :13-18).
- Bridges are best-effort, refusal-safe, never raise into the transport
  (`scripts/xiang_turns.py` module docstring :19-23).

### Emacs Arxana (futon4/dev)

- Persistence is exactly futon1b's generic POST /entity
  (`futon4/dev/arxana-links.el:223-227` `arxana-links--persist-entity!`,
  "fail fast on Futon write errors"); link strategies, voiced links and
  surface forms all persist through it (:319-491). **Policy inherited:
  whole-document last-write-wins, no client-side CAS or conflict handling.**
  The vsatarcs modules (`arxana-vsatarcs-xtdb-clicks.el` etc.) are read-only
  projections. No local store of its own found (searched `write\|put\|POST`
  across `futon4/dev/arxana-*.el`).

### CRDT/OT

- **None present or vendored.** `grep -rni "crdt|automerge|yjs|
  operational.transform"` over futon1b, futon2/src, futon3c/src, futon4,
  futon4a (clj/el/py) returns one hit: the negative statement
  `futon1b_arxana.clj:22`. (Note per packet rules: also searched in-namespace
  `def name` forms via `grep -n "defn \|def "` on each file, not just
  `^def ns.name`.)

## Ready — no new code / Missing — actual work

| Record type | Policy | Where enforced | Test |
|---|---|---|---|
| arxana/* revisions+heads (futon1b) | CAS (ASSERT head) + msg-id idempotency; loser stale-refused, whole tx aborts | futon1b_arxana.clj:260-315 | test_arxana_conditional.clj:80-111 ✅ |
| arxana journal/outcomes (futon1b) | append-only in same tx | futon1b_arxana.clj:272-295 | same file ✅ |
| Generic entities (futon1b) | whole-doc replace, last-write-wins, no CAS | futon1b_graph.clj:595-606 | batch round-trip tests exist; **no concurrent-writer test — Missing** |
| Hyperedges (futon1b) | coexistence by stable id (idempotent upsert) + no-op guard; minted acts refuse conflicting idempotency key | futon1b_server.clj:305-327; API-CONTRACT.md:407-413 | test_hyperedges_batch.clj (batch path) ✅; idempotency-conflict — **not found as a test, Missing** |
| Write log (futon1b) | append-only, locked | futon1b_write_log.clj:34-40 | test_write_log.clj ✅ |
| 象 turn record | exclusive create; existing id never replaced | turn_store.clj:84-113 | turn_store_test.clj ✅ |
| 象 analysis | publish-once refusal (:analysis-exists) | turn_store.clj:141-156 | turn_store_test.clj:32 ✅ |
| 象 record updates (happened etc.) | whole-file replace, no CAS | turn_store.clj:121-129 | round-trip test only — **no concurrent update test, Missing** |
| 象 drafts/candidates/quotes | freely replaceable (recomputable) | turn_store.clj:167-195 | none dedicated — prose+code, low risk |
| Emacs turn records | append-only by unique filename; analysis exclusive link | session-turn-analysis.el:473-475; session_turn_analysis.py:397-405 | turn-seam/conformance.py ✅ |
| Emacs Arxana entities/links | inherited last-write-wins via POST /entity | arxana-links.el:223-227 | none in futon4 — **Missing** |
| CRDT/OT | absent by design ("No sequencer, no CRDT") | futon1b_arxana.clj:22 | n/a |

## Open doubts

- I did not run a live two-writer probe (rules forbid writes to live
  stores); the race behaviour rests on test_arxana_conditional.clj, which
  runs against a real XTDB node (hermetic, in-repo).
- The generic entity route's PUT-vs-POST wording: futon1b exposes POST;
  "PUT replaces the whole document" in the mission packet matches the
  futon1a compat surface described in API-CONTRACT.md (`src/futon1a/...`).
  I surveyed futon1b's live route (POST); futon1a's legacy PUT was not
  separately re-read.
- Whether `update-record!`'s read→f→write gap has ever lost a `happened`
  note in practice is not testable from code alone; no sorry/ticket found
  (searched none — stated as absence of search rather than absence of bug).
