# D2 — strategy evaluator and resolution semantics

Mission: `M-jvm-arxana`. Phase: DERIVE. Inputs: P2 census, D1 canonical
model. Baseline: `9c8b515`. Status: design checkpoint.

## Purpose

D2 defines a deterministic evaluator over immutable artifact bytes. It ports
the useful three-stage anchor recovery algorithm, repairs its silent ambiguity
and unverifiable-fuzzy behaviour, and gives new executable meaning to finder
declarations without pretending the legacy declarations ever had such meaning.

## Decisions

**D2-DR1.** IF legacy strategies persist a list of five finder forms;
HOWEVER P2 found no executor, fallback ordering or ambiguity semantics; THEN
legacy strategy records are imported as `declaration-only` and must be
explicitly translated to a versioned executable strategy; BECAUSE treating
list position as historical execution semantics would manufacture behaviour
that never existed.

**D2-DR2.** IF the existing anchor cascade is algorithmically pure and already
has stability evidence; HOWEVER it silently takes the first duplicate and its
fuzzy tier can return a non-hash-verifying span; THEN preserve its three
recovery stages but enumerate all candidates and return ambiguity or
unverified-fuzzy evidence explicitly; BECAUSE deterministic first-match is not
the same as a justified unique resolution.

**D2-DR3.** IF resolution must be identical across clients; HOWEVER editors
hold mutable buffers and local filesystem views; THEN the evaluator accepts
bytes only from an `ArtifactProvider` by content-addressed revision and verifies
the digest before evaluation; BECAUSE an editor buffer cannot be the shared
authority's implicit input.

**D2-DR4.** IF a finder is meaningful only inside one ordered strategy;
HOWEVER some finder evidence may later be cited independently; THEN finders
are embedded values identified by `(strategy-revision-id, ordinal)` by default
and are promoted to entities only when independently cited; BECAUSE creating a
graph entity for every private step adds identity without reuse.

**D2-DR5.** IF a resolution is normally recomputable; HOWEVER cited results
must remain queryable with their exact evidence; THEN ordinary results remain
protocol values and promoted resolution assertions use an entity for the
outcome plus typed hyperedges to strategy, artifact revision, candidates and
evidence; BECAUSE one overloaded hyperedge would hide outcome fields and make
multi-role querying dependent on property scans.

## Inputs

```clojure
{:request/id "uuid-or-idempotency-key"
 :artifact/revision-id "arxana:revision:sha256:..."
 :strategy/revision-id "arxana:strategy-revision:sha256:..."
 :subject/id "optional semantic entity or manifestation id"
 :options {:max-candidates 100
           :allow-unverified-fuzzy? false}}
```

The evaluator resolves both revision IDs before running. An absent artifact or
strategy is `missing`; bytes whose digest differs from the named revision are
`stale-input`; an unknown semantics version or finder type is `unsupported`.
No evaluator request accepts anonymous current-buffer text.

### Artifact provider

```clojure
(defprotocol ArtifactProvider
  (revision-metadata [provider revision-id])
  (revision-bytes [provider revision-id]))
```

`revision-bytes` returns canonical bytes and media type. The evaluator computes
the digest again. Deployment choices—repository reader, immutable blob store,
or a headless service cache—belong to D7, but all must implement this contract.
A client may ingest bytes as a new revision before resolution; it may not
override bytes for an existing revision ID.

## Executable strategy grammar

```clojure
{:arxana/semantics :arxana.resolution/v1
 :arxana/strategy-id "stable logical strategy id"
 :arxana/strategy-revision-id "content-addressed revision id"
 :arxana/scope {:repository "..." :roots ["..."] :artifact-kinds #{...}}
 :arxana/cascade-mode :first-unique
 :arxana/finders
 [{:arxana/finder-id "<strategy-revision>#0"
   :arxana/type :arxana.finder/anchor
   :arxana/on #{:missing :ambiguous}
   :arxana/config {...}}]}
```

Finder order is vector order and is included in the strategy revision digest.
Each finder states which prior outcomes permit continuation. Supported cascade
modes are:

- `:first-unique` — stop at the first finder producing one verified candidate;
- `:accumulate` — run every applicable finder, deduplicate candidates and
  decide uniqueness after deterministic ranking;
- `:all-evidence` — like accumulate, but retain duplicate observations as
  separate evidence items for diagnostic/audit use.

There is no implicit fallback mode. A translated strategy must name one.

### Legacy classification

| Legacy finder | v1 classification | Required translation |
|---|---|---|
| `explicit` | executable | name artifact revision or artifact-selection rule and an anchor/range; offsets alone are insufficient |
| `symbol-as-term` | executable when configured | pin definition patterns, language and term normalization; results retain every matching definition/mention |
| `filename-mention` | executable when configured | pin repository revision/root, path normalization, include/exclude rules and mention grammar |
| `embedding-proximity` | advisory-only by default | becomes executable only with pinned model ID/digest, vector-space version, threshold, tie rule and immutable neighbour input |
| `surface-form-hybrid` | translated composition | expand into named exact/surface/optional embedding steps with explicit cascade mode; the legacy tag alone is unsupported |

An imported legacy record retains its original JSON and receives
`:arxana/semantics :arxana.resolution/declaration-only`. Translation produces a
new strategy revision, a provenance link to the legacy ID and a human- or
tool-authored translation receipt. Unknown types remain visible and return
`unsupported`; they are never dropped from the digest.

## Anchor finder v1

Input anchor fields preserve the legacy content hash, content length,
context-before/context-after, hint offset and hint line. Positions are Unicode
codepoint half-open ranges `[start,end)` within decoded canonical text; byte
offsets, UTF-16 units and line/column projections are computed separately by
D3 clients.

Stages run in this order:

1. **Hint/hash.** If the hinted range is in bounds and its canonical text hash
   matches, emit one `verified-exact` candidate.
2. **Context/hash.** Enumerate *all* occurrences compatible with before/after
   context and content length; retain every hash-verifying candidate.
3. **Fuzzy context.** Enumerate bounded before/after context pairs satisfying
   configured distance and length tolerances. Mark each `unverified-fuzzy` and
   include similarity features; never report it as verified.

If a verified stage emits one candidate, the finder resolves. If it emits more
than one, it is ambiguous and does not silently select the first. If no
verified candidate exists and fuzzy is disabled, it is missing. If fuzzy is
enabled, one fuzzy candidate yields `resolved-unverified`; multiple yield
ambiguous. Policy may refuse to promote an unverified result, but the evaluator
does not relabel it missing.

Candidate ordering is stable and non-semantic: verification class, descending
declared score, start codepoint, end codepoint, then candidate digest. Ordering
never turns a tied or multiple set into uniqueness.

## Result algebra

Every response has the common envelope:

```clojure
{:request/id "..."
 :artifact/revision-id "..."
 :strategy/revision-id "..."
 :result/status :resolved
 :result/candidates [...]
 :result/evidence [...]
 :result/diagnostics []}
```

Statuses are:

| Status | Meaning | Candidates |
|---|---|---|
| `resolved` | exactly one verified candidate satisfies the strategy | one |
| `resolved-unverified` | exactly one explicitly allowed fuzzy/advisory candidate | one |
| `ambiguous` | multiple viable candidates remain; ranking is evidence, not selection | two or more |
| `missing` | inputs exist and strategy ran, but no candidate exists | empty |
| `stale-input` | supplied bytes or dependent data do not match named revisions | empty or diagnostic-only |
| `unsupported` | semantics/finder/media type/capability is not implemented | empty |
| `invalid` | strategy or request violates its schema/invariants | empty |
| `failed` | evaluator encountered an operational failure after valid input | empty; typed cause |

`stale` is not used as shorthand for `missing`. `unsupported` is not a
successful no-op. `failed` is not used for ambiguity.

### Candidate

```clojure
{:candidate/id "sha256:..."
 :location {:artifact/revision-id "..."
            :unit :unicode-codepoint
            :range {:start 120 :end 137}}
 :verification :verified-hash
 :score-components {:context 1.0}
 :matched-text-digest "sha256:..."
 :finder/id "<strategy-revision>#0"}
```

Matched text may be returned under an explicit disclosure option; its digest
and bounded context are the default evidence. D3 controls protocol redaction.

### Evidence event

Every attempted finder appends an immutable event containing finder ID/type,
configuration digest, input revision IDs, stage, candidate IDs, outcome,
duration and diagnostics. Events are ordered by evaluator sequence and include
no editor-local marker. Operational duration is diagnostic and excluded from
candidate/result digests.

## Cascade algorithm

```text
verify request and revision digests
for finder in declared vector order:
  if prior outcome permits finder:
    validate support and configuration
    evaluate against immutable input
    append evidence event
    merge or replace candidates according to cascade-mode
    if first-unique and exactly one verified candidate: resolve
derive final status without selecting among multiple viable candidates
return result and complete evidence
```

Deduplication keys are `(artifact-revision-id, start, end,
matched-text-digest)`. Evidence from multiple finders is preserved even when
candidate locations deduplicate.

## Promoting a resolution assertion

Promotion requires `resolved` or an explicit caller acknowledgement of
`resolved-unverified`/`ambiguous`; it stores:

- an immutable `arxana/resolution-assertion` entity containing outcome,
  candidate digests, evaluator/semantics version and evidence digest;
- `:arxana/resolution-evidence-for` hyperedges to artifact revision, strategy
  revision, subject (when supplied) and cited evidence records;
- the original result payload or content-addressed blob reference.

The assertion ID is the digest of semantic inputs and outcome, excluding
request ID, timing and transport fields. Repeating the same computation is an
idempotent upsert. A later artifact revision creates another assertion; it does
not mutate the earlier evidence.

## Failure and ambiguity examples

1. **Unrelated prefix edit.** New bytes create revision R2. Hint/hash fails,
   context/hash finds exactly one verified candidate: `resolved`; R1's prior
   result remains evidence about R1.
2. **Duplicate paragraph inserted earlier.** Context/hash emits two verified
   candidates: `ambiguous`; unlike legacy code, the earlier duplicate does not
   silently win.
3. **Inside-anchor edit.** Hash tiers fail; one fuzzy context pair is found.
   Default result is `missing` with fuzzy diagnostic; with explicit allowance,
   `resolved-unverified`.
4. **Wrong bytes under revision ID.** Digest check fails before finders:
   `stale-input`.
5. **Legacy embedding finder.** No pinned model/digest: `unsupported`, not an
   empty successful result.

## Tripwire tests

- Port the existing ERT fixtures for exact, inserted-prefix, multiline,
  buffer-start and orphaned cases using string input and golden codepoint
  ranges.
- Add unrelated suffix edit: hint/hash still resolves.
- Add an earlier duplicate: result must be `ambiguous`, never first-match.
- Add fuzzy-only recovery: verification must be `unverified-fuzzy`; promotion
  without acknowledgement is refused.
- Permute finder order: strategy revision ID and, where applicable, result
  evidence order must change.
- Replay identical input: semantic result and evidence digests match despite
  different duration/request IDs.
- Supply mismatched bytes/revision: no finder runs.
- Import every legacy finder type: none is lost; declaration-only execution is
  refused with a translation requirement.
- Run two client adapters over one response: authority candidate IDs and ranges
  remain identical even if UI projections use different coordinate units.

## D2 outputs to later work

- D3 receives the result/error algebra, Unicode codepoint location basis,
  request idempotency input and evidence envelopes.
- D4 receives explicit partial/ambiguous/unverified outcomes and immutable
  revision pairing for correspondence.
- D5 receives golden authority responses; editor coordinate conversion stays
  in clients.
- D6 may cite promoted resolution assertions, not ephemeral request IDs.
- D7 must choose the `ArtifactProvider`, evaluator deployment locus and
  evidence storage, while preserving evaluator semantics.

## Closed and open obligations

- **O-D1a closed:** finders are embedded and ordinal-identified unless cited
  independently.
- **O-D2a closed:** dispatch, ordering, ambiguity, legacy classification and
  failure semantics are explicit.
- **O-D2b narrowed:** artifact access has a fixed interface and digest check;
  D7 still selects its deployment implementation.
- **O-D1b recommendation:** promoted resolution uses entity plus hyperedges;
  D7 must validate query/index cost against futon1b.

No architecture is selected by D2.
