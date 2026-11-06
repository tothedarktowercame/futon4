# D6 — inference-ready action boundary

Mission: `M-jvm-arxana`. Phase: DERIVE. Inputs: P6 and D1–D5. Status: design
checkpoint.

## Decisions

**D6-DR1.** IF four consumers already share ID-addressed graph reads and typed
hyperedge writes; HOWEVER their scores, verdicts, locators and ordering rules
are domain-owned; THEN compile only a common grounded candidate-action core and
permit namespaced extensions; BECAUSE standardizing policy would turn Arxana
from correspondence authority into an institution.

**D6-DR2.** IF an operator turn may suggest an action; HOWEVER interpretation,
admission and execution have different authorities; THEN represent source act,
intent, candidate action, admission decision and action receipt as distinct
records; BECAUSE collapsing them makes description look like authorization.

**D6-DR3.** IF compilation depends on resolutions and correspondences;
HOWEVER those can be ambiguous, unverified or stale; THEN compilation either
retains unresolved alternatives/guards or refuses with typed obligations; it
never silently selects; BECAUSE action grounding cannot be stronger than its
evidence.

## Compiler input

```clojure
{:operation :action/compile
 :source-act/id "evidence-id"
 :intent {:intent/id "..." :intent/type :edit/replace
          :target-ref {:entity/id "..."}
          :proposed-change {...}}
 :grounding {:artifact/revision-id "..."
             :resolution-assertion/id "..."
             :correspondence-assertion/ids ["..."]}
 :compiler/version "arxana.action/v1"
 :extensions {:circus/... ...}}
```

The compiler verifies all IDs, revisions, assertion evidence and extension
namespaces. Ephemeral D2 results must first be promoted; ambiguous/unverified
promotion must carry the D2 acceptance record. Unknown extension namespaces
are preserved only as opaque input evidence and cannot alter core semantics.

## Candidate action

```clojure
{:candidate-action/id "arxana:action:sha256:..."
 :operation {:type :artifact/replace-range
             :target {:manifestation/id "..."
                      :artifact/revision-id "..."
                      :range {:unit :unicode-codepoint :start 10 :end 20}}
             :payload {:replacement "..."}}
 :preconditions [{:type :revision/head-equals :revision/id "..."}
                 {:type :resolution/accepted :assertion/id "..."}]
 :predicted-effects [{:type :revision/creates :artifact/id "..."}
                     {:type :correspondence/stales :ids ["..."]}]
 :warrants [{:type :intent :id "..."}
            {:type :resolution :id "..."}
            {:type :correspondence :id "..."}]
 :risks [{:type :derived-artifact/rebuild-required}]
 :unresolved []
 :compiler {:version "arxana.action/v1" :input-digest "sha256:..."}
 :extensions {}}
```

The ID digests canonical semantic inputs, including compiler version, but
excludes request identity and presentation. Compilation is deterministic and
idempotent. A candidate is immutable and non-executable: it contains no bearer
credential, admission token or implicit consent.

## Result algebra

- `compiled` — one candidate with satisfied grounding requirements.
- `conditional` — candidate retains explicit alternatives or unresolved
  preconditions; no executor may treat it as admitted.
- `ambiguous` — multiple materially different candidate actions remain.
- `stale-input` — a grounding assertion/revision is no longer applicable.
- `unsupported` — operation/compiler/extension requires undefined semantics.
- `invalid` or `failed` — schema/invariant or operational failure.

Alternatives have stable IDs and shared provenance. Ranking may be attached by
a consumer namespace but does not change core ambiguity.

## Admission and execution boundary

An admission authority consumes a candidate and emits a separate immutable
decision: admitted, refused, deferred or consent-required, naming policy and
policy revision, actor, reasons, conditions and candidate ID. Arxana defines
the interoperable shape but owns no institutional policy.

Execution requires an admitted decision plus D3 expected-revision precondition.
It emits an append-only receipt naming request/operation ID, candidate and
decision IDs, before/after revisions, actual effects, deviations, outcome and
evidence. A receipt never retroactively mutates the prediction.

## End-to-end draft8 example

Intent: “replace the section title ‘The Object’ with ‘Objects and Rules’.”

1. Source act is retained verbatim with evidence ID.
2. Intent interpretation identifies `:edit/replace` and the stable D4 section
   manifestation.
3. D2 resolution assertion grounds the TeX range in one exact source revision;
   D4 correspondence identifies the HTML manifestation and notes its build-local
   `S2` locator.
4. Compiler emits `artifact/replace-range`, expected head, replacement, a
   predicted new TeX revision, stale/rebuild effects for HTML correspondence,
   warrants and the unresolved obligation to run the renderer.
5. No file is changed. A later policy may admit; a D3 executor may apply and
   receipt it; a rebuild may establish a new correspondence assertion.

If the positive D4 fixture has not been repaired, compilation is
`stale-input`/`conditional` rather than pretending the contradictory hashes
ground the edit. This example becomes executable evidence only in INSTANTIATE.

## Consumer attachment

| Consumer | Core use | Namespaced extension (not standardized here) |
|---|---|---|
| REPL Circus | source act/intent IDs, target graph IDs | recall/activation scores and reasons |
| Ways of Working | warrants, admission/receipt links | probe verdict, institution/policy status and freshness window |
| Giraffe | candidate target, preconditions, unresolved dependencies | want locator, readiness and application-order evidence |
| War Machine | candidate/decision/receipt inspection | selector reading record, sorry trajectory and runtime certificate |

Extensions may add evidence and constraints. They cannot redefine target IDs,
revision preconditions, operation type, core outcome or provenance. Promotion
of an extension into the core requires a later witnessed schema revision.

## Query and write waist

Required reads are ID-addressed entity/hyperedge lookup, bounded neighbourhood,
resolution/correspondence lookup and current revision. Durable writes are
typed hyperedges linking source act→intent→candidate→decision→receipt plus the
records themselves. Batch endpoints contain IDs only; evidence/properties stay
on typed records and namespaced fields.

## Tripwires

1. Same semantic input yields the same candidate ID and semantic digest.
2. Removing an exact revision or warrant changes/refuses compilation.
3. Ambiguous resolution yields alternatives, never first-candidate selection.
4. Candidate cannot be submitted to D3 execution without a separate admitted
   decision and current expected revision.
5. Consumer score/order changes do not alter core candidate ID.
6. Unknown extension cannot override a core field.
7. Receipt records actual deviations without rewriting predicted effects.
8. Contradictory draft8 negative fixture cannot produce `compiled`.

## Obligations

- **O-D6a closed at design level:** end-to-end fixture example and consumer
  attachments are specified; demonstration waits for repaired positive fixture.
- D7 must assign intent interpreter, compiler, admission-adapter and executor
  boundaries without assigning institutional policy to Arxana.
- ARGUE must test whether the candidate-action vocabulary is genuinely common
  or exceeds P6's witnessed waist.

No admission policy, ranking model, executor or architecture is selected by D6.
