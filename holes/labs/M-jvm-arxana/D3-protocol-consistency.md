# D3 — shared protocol and enforceable consistency

Mission: `M-jvm-arxana`. Phase: DERIVE. Inputs: P3, P5, D1, D2 and ARGUE
obligations O-D3a/O-D3b. Status: design checkpoint.

## Decisions

**D3-DR1.** IF clients currently disagree on positions, freshness and errors;
HOWEVER their useful behaviours must survive; THEN expose one versioned
operation envelope and typed domain results, with client coordinate conversion
outside the authority; BECAUSE shared field names without shared meaning would
reproduce P3's collisions.

**D3-DR2.** IF futon1b lacks CAS and subscriptions; HOWEVER stale writes must
be refused atomically; THEN a conforming deployment must provide either a
store-side conditional transaction or a durable single-writer sequencer;
BECAUSE check-then-unguarded-write has a race and client equivalence supplies
no enforcement.

**D3-DR3.** IF no legitimate concurrent multi-writer need is observed;
HOWEVER sequential clients and offline delay already produce staleness; THEN
use revision preconditions, refusal, journalling and resubmission rather than
merge operations; BECAUSE these meet observed requirements while retaining the
recorded CRDT re-arm condition.

## Envelope

```clojure
{:protocol/version "arxana.v1"
 :message/id "globally unique idempotency key"
 :operation :scope/resolve
 :principal/id "..."
 :sent-at "RFC3339"
 :body {...}}
```

Responses echo version, message ID and operation and contain exactly one of
`:ok` or `:error`. Unknown fields are rejected in v1. Errors carry
`{:category :invalid|:unsupported|:missing|:stale|:conflict|:forbidden|:overload|:failed,
  :code qualified-keyword, :detail ..., :recovery ...}` and preserve futon1b's
layer/reason/context beneath `detail`.

D2's `stale-input` remains a successful evaluator result meaning named bytes
failed integrity verification. Protocol `stale` is a refused operation whose
expected revision is no longer current. `conflict` means the precondition was
current but another invariant prevents the operation. These are never aliases.

## Operations

| Operation | Required body | Result |
|---|---|---|
| `entity/get`, `graph/query` | IDs or bounded typed query | canonical records + read revision/cursor |
| `scope/resolve` | artifact revision + strategy revision | D2 result/evidence algebra |
| `correspondence/query` | manifestation/revision IDs | D4 neighbourhood |
| `action/compile` | intent + grounded IDs/revisions | D6 candidate action |
| `revision/ingest` | artifact ID, canonical bytes/digest, expected head | immutable revision + new head or stale refusal |
| `assertion/promote` | result digest, evidence, optional acceptance | durable IDs or typed refusal |
| `subscription/open` | topics, filters, optional resume cursor | subscription ID + snapshot cursor |
| `subscription/ack` | subscription ID + cursor | acknowledgement |

Locations are
`{:artifact/revision-id ..., :unit :unicode-codepoint, :range {:start n :end n}}`:
zero-based, half-open, Unicode scalar values. Line/column, UTF-16 and file-index
forms are client projections and must carry their unit when sent as hints.

## Revision and write contract

Every mutation-shaped operation supplies:

```clojure
{:target/id "stable logical resource"
 :expected/revision-id "sha256 revision or :absent"
 :command {...}}
```

The authority performs `compare expected → validate → append journal → write
new immutable revision → move head → append outcome` as one serialized
decision. Duplicate message IDs return the original outcome. Stale refusal
returns current head, submitted base, a recoverable command/journal reference
and safe choices `refresh`, `rebase-and-resubmit`, or `export-patch`; it never
silently retries against a new base. Undo is a new revision-producing command.

## Enforcement alternatives for D7

| Mechanism | Atomicity locus | Strength | Cost/risk | Verdict |
|---|---|---|---|---|
| futon1b conditional transaction | store route compares head and writes revision/head/journal in one XTDB transaction | one storage truth; simplest failure recovery | requires new futon1b capability and route | conforming candidate for architecture 1 |
| durable headless sequencer | one JVM process serializes commands; durable log/head reconstructed before serving; futon1b writes idempotent by command ID | isolates evolving protocol/evaluator from store | split-brain unless exclusive lease/fencing token is durable and checked | conforming only with lease/fencing proof; candidate for architecture 2 |
| client-hosted evaluator/write | each editor checks then writes | cheap reuse | no atomic refusal, duplicated discipline, divergent answers | non-conforming for writes; architecture 3 may remain read-only only |

A memory-only mutex, optimistic GET-then-POST, or client promise is explicitly
non-conforming. D7 must select and tripwire one conforming mechanism.

## Subscription model

Events are derived from the authoritative journal and carry monotonically
ordered cursor, event ID, cause message ID, topic, affected IDs, before/after
revision IDs and semantic payload digest. Delivery is at-least-once; event IDs
deduplicate. Opening returns a snapshot cursor, then events strictly after it.
Clients ack cursors and may resume within retention. An expired/unknown cursor
returns `resync-required`, causing a fresh snapshot rather than guessed replay.
Ordering is guaranteed per authority journal, not across external domain feeds.

Subscriptions are not correctness prerequisites: a disconnected client still
receives stale refusal on write. They are latency and projection-maintenance
mechanisms.

## State machine

`received → duplicate-returned | validated → stale-refused | conflict-refused |
journalled → applied → published`. Recovery after failure replays journalled
commands by message ID; an applied command without a published event is
republished, never reapplied. No success is returned before the durable outcome
exists.

## Required traces and tripwires

1. Two commands expect R1: exactly one creates R2; the other is stale and R2
   is unchanged.
2. Replaying either message ID returns byte-equivalent semantic outcome.
3. The 2026-08-08 snapshot-restore incident: out-of-band head replacement is
   refused or becomes an explicit administrative revision, never invisible.
4. The 100-minute-stale page: edit is refused with current head and exported
   recoverable command.
5. Crash after apply/before publish: restart republishes one deduplicable event.
6. Resume from retained cursor returns ordered missing events; expired cursor
   returns `resync-required`.
7. D2 wrong bytes returns result `stale-input`; an edit based on old head
   returns error `stale`; an invariant collision returns `conflict`.
8. Emacs line/column and VS Code UTF-16 projections round-trip to the same
   authority codepoint range or fail explicitly on invalid boundaries.

## CRDT re-arm condition

Reopen merge semantics only when two independently legitimate concurrent or
offline edits both must survive and stale refusal plus recoverable resubmission
would destroy unrecoverable work. Record the incident, artifact class and
required convergence property before choosing CRDT/OT. Protocol v1 reserves a
future consistency-mode capability but defines no merge operations.

## Obligations

- **O-D3a narrowed:** two conforming enforcement mechanisms are specified; D7
  selects one and proves its atomicity/fencing.
- **O-D3b closed:** evaluator `stale-input`, mutation `stale`, and invariant
  `conflict` have distinct meanings and traces.
- D5 must map current typed WYSIWYG refusals/journal into this algebra.
- D7 must assign the journal, head, lease/fencing and event-retention owner.

No architecture is selected by D3.
