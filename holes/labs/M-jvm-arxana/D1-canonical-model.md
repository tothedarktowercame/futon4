# D1 — canonical model and storage boundary

Mission: `M-jvm-arxana`. Phase: DERIVE. Baseline: mission commit `aac8544`.
Inputs: MAP reports P1, P2, P4 and P6. Status: design draft for D2–D7.

## Boundary decision

Futon1b remains the durable graph store. The Arxana authority may be deployed
inside or above it—the architecture comparison has not yet selected that
boundary—but every option talks to storage through the same adapter defined
here. Editor state, resolved positions and protocol envelopes are not silently
promoted to durable graph identity.

**D1-DR1.** IF futon1b already enforces and live-serves entity and hyperedge
records; HOWEVER its current shapes omit revisions, salience and Plexus and its
scope-family producer bypasses the ordinary census; THEN the initial storage
contract preserves the two observed record families and adds new Arxana types
through their existing extension fields; BECAUSE replacing the live waist
would create migration risk without improving the authority boundary.

**D1-DR2.** IF a revision must remain referable after an artifact changes;
HOWEVER a mutable entity plus ignored `as-of` parameters cannot provide that
identity; THEN an artifact revision is an immutable content-addressed entity,
not an entity query parameter; BECAUSE correspondence, resolution and stale
refusal all need the same revision to be named independently of XTDB API
version support.

**D1-DR3.** IF scope resolutions can be recomputed from strategy, revision and
text; HOWEVER they may also serve as evidence for correspondences and actions;
THEN an ordinary resolution response is derived and protocol-only, while a
resolution becomes durable only when cited by a durable assertion or receipt;
BECAUSE persisting every navigation would confuse cache traffic with evidence.

## Record classes

| Type | Origin | Persistence | Identity rule | Revision/staleness rule |
|---|---|---|---|---|
| Graph entity | authored or ingested | existing `:entities` | existing `:entity/id`; aliases remain futon1b's concern | mutable metadata; never substitutes for artifact revision |
| Hyperedge | authored or derived | existing `:hyperedges` | caller ID or existing stable type+sorted-endpoint ID | temporal validity where supplied; endpoint IDs immutable within one assertion |
| Artifact | ingested or authored | entity `arxana/artifact` | stable repository-qualified resource identity | points to revisions; does not contain current mutable text as identity |
| Artifact revision | ingested | entity `arxana/artifact-revision` | `arxana:revision:sha256:<digest>` of canonical bytes | immutable; a new digest is a new entity |
| Manifestation | authored or derived | entity `arxana/manifestation` | stable ID for semantic role within an artifact family | each observation names an artifact revision |
| Scope strategy | authored | entity `arxana/scope-strategy` | preserve deterministic legacy strategy ID where imported | versioned by content digest in props; edits create a new strategy revision |
| Finder declaration | authored | embedded in strategy initially; entity only if independently cited | strategy ID plus ordered ordinal or explicit ID | immutable as part of strategy revision |
| Scope resolution | derived | protocol-only by default | request-local result ID | valid only for its named artifact and strategy revisions |
| Resolution assertion | derived and admitted | entity plus evidence hyperedges | content-addressed from strategy revision, artifact revision, candidates and outcome | immutable; may later be marked superseded/stale by another assertion |
| Correspondence assertion | derived or authored | entity plus endpoint hyperedge | content-addressed from manifestations, revisions, method and evidence | immutable evidence; validity is explicit, never inferred from current files |
| Intent | ingested or authored | durable only when admitted into an argument/action trace | source act/evidence ID plus interpretation ID | interpretation may be revised without rewriting the source act |
| Candidate action | derived | protocol-only until cited/admitted | compilation result ID | names all input revisions; stale if any required revision changes |
| Admission decision | authored/institutional | durable entity/hyperedge outside core policy | decision ID | append-only decision with policy/version provenance |
| Action receipt | derived from attempted operation | durable | operation/request ID | append-only; names before/after revisions and outcome |

Salience and Plexus are not canonicalized by this table. They require separate
missions or later schema proposals with a live writer, reader and invariant.
The storage adapter must not claim support merely because legacy clients send
those fields.

## Namespaced shapes

New records use the live entity envelope:

```clojure
{:name "human-readable label"
 :type :arxana/artifact-revision
 :external-id "arxana:revision:sha256:..."
 :source "repository-qualified artifact id"
 :props {:arxana/revision-algorithm :sha256
         :arxana/content-digest "..."
         :arxana/media-type "text/x-tex"
         :arxana/byte-length 1234}}
```

The server supplies `:entity/id`/`:xt/id`. Properties owned by Arxana use the
`arxana/` namespace. The live `mission-scope` family retains its observed
`prop/*`, `scope/*` and `mission-scope/*` vocabulary until a producer and
migration can be named; reads normalize it, writes do not create a third
variant.

New relation assertions use the live hyperedge envelope:

```clojure
{:hx/type :arxana/corresponds
 :hx/ends [{:entity-id "manifestation:A" :role :source}
           {:entity-id "manifestation:B" :role :target}
           {:entity-id "revision:A1" :role :source-revision}
           {:entity-id "revision:B1" :role :target-revision}]
 :hx/endpoints ["manifestation:A" "manifestation:B"
                "revision:A1" "revision:B1"]
 :hx/props {:arxana/method :source-map
            :arxana/direction :source-to-target
            :arxana/evidence-id "correspondence-assertion:..."}}
```

Roles in `:hx/ends` are authoritative when endpoint order has meaning;
`:hx/endpoints` remains the compatibility index. Binary relations may use the
existing relation route only when no third evidence/revision endpoint is part
of the assertion.

## Relation catalogue

| Relation | Arity | Durable? | Required roles |
|---|---:|---|---|
| `:arxana/has-revision` | 2 | yes | artifact, revision |
| `:arxana/manifests` | 3 | yes | entity, manifestation, artifact |
| `:arxana/observed-in` | 2 | yes | manifestation, revision |
| `:arxana/strategy-has-finder` | 2 | only if finder is independently identified | strategy, finder |
| `:arxana/resolution-evidence-for` | 3+ | when assertion is cited | assertion, strategy revision, artifact revision; optional evidence items |
| `:arxana/corresponds` | 4+ | yes | two manifestations and their two observed revisions; evidence assertion |
| `:arxana/interprets-act` | 2+ | when intent is retained | intent, source act; optional interpreter/model |
| `:arxana/compiles-to` | 2+ | when candidate is cited | intent, candidate action; grounded target/scope inputs |
| `:arxana/admission-of` | 2+ | yes | decision, candidate action; policy/version |
| `:arxana/receipt-for` | 3+ | yes | receipt, admitted action, before/after revision(s) |

Consumer-owned relations remain namespaced to their domains. In particular,
activation scores, probe verdicts, want locators, sorry trajectories and
application-order evidence do not become `arxana/*` fields merely because they
use the common hyperedge envelope.

## Storage adapter

```clojure
(defprotocol ArxanaGraphStore
  (get-entity [store entity-id])
  (find-entities [store query])
  (put-entity! [store expected-shape])
  (get-hyperedge [store hyperedge-id])
  (find-hyperedges [store query])
  (put-hyperedges! [store expected-shapes])
  (retract-hyperedge! [store hyperedge-id valid-from])
  (capabilities [store]))
```

`capabilities` is evidence, not aspiration. The current adapter must report:

```clojure
{:entity-read true
 :entity-write true
 :entity-as-of false
 :entity-compare-and-set false
 :hyperedge-read true
 :hyperedge-write true
 :hyperedge-valid-time true
 :subscription false
 :scope-family-census false}
```

An architecture may wrap missing CAS or subscriptions above the adapter, but
must not advertise them as store guarantees. Adapter errors preserve futon1b's
layer/reason/context data and add an Arxana category; they do not flatten typed
refusals into `nil`.

## Compatibility and migration rules

1. Existing entities and compat hyperedges round-trip without adding required
   fields or changing IDs.
2. The `hx|mission-scope|...` family is readable through a normalizer that maps
   `prop/*` into a typed view while preserving the original document for writes
   and evidence.
3. No migration runs until its producer is located and the census defect has a
   regression test. Until then the family is read-compatible, write-frozen.
4. Legacy strategy/link/surface-form/scholium JSON is retained byte-for-byte in
   an import property and projected into typed D2 values. Unknown finder types
   become `unsupported`, not discarded.
5. MD5 legacy IDs remain aliases for imported strategies and voiced links;
   new immutable revision/assertion IDs use SHA-256.
6. Client-only salience keys are reported as unsupported on write rather than
   silently accepted. A compatibility client may omit them.
7. The inactive 7071 default is not part of the contract. Endpoint discovery
   is deployment configuration and a health/capability handshake is mandatory.

## Checkable D1 invariants

- Two artifact revisions with different canonical bytes cannot share an ID.
- Every stored correspondence names the exact revision of every manifestation
  endpoint.
- A protocol-only scope resolution cannot be referenced by a durable receipt
  until promoted to a resolution assertion.
- Normalizing and denormalizing an observed legacy record preserves every
  original key and value.
- A capability absent from the live adapter produces `unsupported`, never a
  simulated success.
- Domain extension fields cannot occupy the `arxana/` namespace.
- No new write targets the mission-scope family before producer and census
  tripwires pass.

## Inputs supplied to later DERIVE work

- D2 receives immutable strategy revision and artifact revision identities,
  plus the resolution/assertion distinction.
- D3 receives the capability handshake, typed error preservation and the rule
  that every artifact-dependent operation names a content revision.
- D4 receives manifestation and correspondence assertion identities.
- D5 receives a projection boundary: clients own view state, not identity.
- D6 receives durable intent/action/decision/receipt distinctions and the
  namespaced consumer boundary.
- D7 must choose where the adapter and derived-result promotion run, but may
  not weaken these storage semantics.

## Remaining D1 questions for architecture synthesis

1. Whether strategy finders merit independent durable entities depends on D2's
   reuse and evidence requirements.
2. Whether resolution assertions use entity-plus-hyperedge or one hyperedge
   document depends on query examples and futon1b indexing behaviour.
3. A transaction/CAS mechanism above futon1b belongs to D3/D7; the adapter
   truthfully exposes its current absence.
4. Subscription event persistence belongs to D3 and must not be conflated with
   XTDB valid time.

These are bounded architecture decisions, not missing definitions of record
meaning. D1 is ready to serve as the shared vocabulary for D2–D6.
