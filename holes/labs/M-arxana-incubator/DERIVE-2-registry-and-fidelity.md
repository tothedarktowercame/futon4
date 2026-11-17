# DERIVE-2 — registry entries, fidelity contract, open rules (M-arxana-incubator)

claude-5, 2026-11-17. Continues `DERIVE-1-types.md`. Joe asked for the items on
claude-5's side to be pushed through before his corrections, which wait for the wiring
diagram (`futon5/data/missions/arxana-incubator-exotype.edn`, futon5 d45ecee).

## 1. Wiring diagram

`futon5/data/missions/arxana-incubator-exotype.edn`, in futon5's mission-diagram format:
4 inputs (landscape, manifestations at commits, Joe's input, forms as the only
constraint), 8 components (register, requirements, Nipper, edit, project, local
evaluation, global evaluation, render), 5 outputs (views, form text, alignment margin
notes, requirement list, typed absences), 25 typed edges.
`futon5.ct.mission/validate` passes all eight checks (completeness, coverage, no orphan
inputs, type safety, spec coverage, I3 timescale ordering, I4 exogeneity, I6 closure).
Planted bad case: an edge from A-eval-local writing to I-forms is refused by the I3 check
(":A-eval-local (social) writing to constraint :I-forms (glacial)"), i.e. the loop
cannot satisfy a requirement by rewriting where it came from.

## 2. Type-registry entries (proposed)

In the form of `futon3c/src/futon3c/arxana/type_registry.clj` (`clusion-type id revision
roles content-fields`). Content fields are identity fields; attribution and time are
included in identity for acts (an edit made twice is two acts, survey A Q2 / correspond
D4 note). Every act record also carries `:event-time`, so its valid time is the act's
time (futon1b 5147aa8).

```clojure
;; E3 content: new writing; identity = text digest + author + time.
(clusion-type :arxana/content 1 []
              [:content/sha256 :content/text :actor :event-time])

;; E4 projection: ordered entries; static (pinned clusion ids) or live (query).
(clusion-type :arxana/projection 1 []
              [:projection/name :projection/entries :projection/applied
               :projection/mode :projection/as-of :actor :event-time])
;;   :projection/entries  [{:span {...R1...}} | {:content <clusion id>}]
;;   :projection/applied  [<edit clusion id> ...]   (static), or
;;                        {:query {...}}            (live)
;;   :projection/mode     :static | :live

;; R2 edit over spans in projections, with scope.
(clusion-type :arxana/edit 1
              [{:role :replacement :endpoint/kinds #{:clusion}}]   ; content, or none for delete
              [:edit/targets :edit/op :edit/scope :edit/reason :actor :event-time])
;;   :edit/targets  [R1 span refs]   :edit/op  :replace | :insert | :delete
;;   :edit/scope    #{projection names} | :every-derived

;; R3 supersede v2.
(clusion-type :supersedes 2
              [{:role :successor :endpoint/kinds #{:clusion :entity}}
               {:role :predecessor :endpoint/kinds #{:clusion :entity}}]
              [:actor :event-time :reason :scope] :cycle :acyclic)

;; R4 link (kinds: :comment :benefit :beneficiary :continues-in :computed-from :prev).
(clusion-type :arxana/link 1
              [{:role :from :endpoint/kinds #{:clusion :entity :manifestation-scope :external-ref}}
               {:role :to   :endpoint/kinds #{:clusion :entity :manifestation-scope :external-ref}}]
              [:link/kind :link/from-span :link/to-span :actor :event-time])

;; R5 requirement (evaluator kinds, one published enum: :word-limit :predicate :deadline
;; :correspondence :attestation).
(clusion-type :arxana/requirement 1
              [{:role :subject :endpoint/kinds #{:clusion :entity}}]
              [:requirement/criterion :requirement/evaluator :requirement/params
               :actor :event-time])

;; R6 nip. Two seed kinds (zai-13's candidate (c), accepted by Joe 2026-11-17: "the "two
;; kinds of seed" are good b/c they can support forward and backward reasoning"):
;;   :nip/seed {:kind :memory :from <m40 memory / turn span>}      ; backward: "already said"
;;           | {:kind :gap :requirement <R5 id> :evaluation <E5 id or nil>}  ; forward: "still needs"
(clusion-type :arxana/nip 1 []
              [:nip/seed :nip/query :nip/result-spans :nip/accepted-by :nip/accepted-at
               :event-time])

;; R7 alignment (after plop-alignment-v1); statuses live in evaluations (D3).
(clusion-type :arxana/alignment 1
              [{:role :informal :endpoint/kinds #{:manifestation-scope :external-ref}}]
              [:alignment/kind :alignment/pattern :alignment/lean :alignment/clojure
               :alignment/docs :alignment/text :actor :event-time])

;; Unwire: withdraw a 'clusion from where it is wired (a projection's applied set, the
;; requirement list) without editing or superseding it (Joe, 2026-11-17: "perhaps by
;; unwiring a 'clusion rather than editing it"). Re-wiring is a later unwire of the unwire,
;; or the 'clusion named again in a new projection version.
(clusion-type :arxana/unwire 1
              [{:role :clusion :endpoint/kinds #{:clusion}}
               {:role :from :endpoint/kinds #{:clusion}}]       ; projection or requirement list
              [:actor :event-time :reason])
```

Record (non-clusion) types, alongside the existing `:evidence-attestation`,
`:assessment`, `:external-reference`:

```clojure
;; E5 evaluation: subject + pinned inputs + result + evaluator.
{:type/id :arxana/evaluation :type/revision 1 :record/kind :evaluation
 :fields [:evaluation/subject :evaluation/pins :evaluation/result
          :evaluation/evaluator :evaluation/locality :event-time]}
;;   :evaluation/result  :met | :failing | :stale | :unknown
;;   :evaluation/evaluator {:kind :attestation :by "joe"} for informal requirements
;;   :evaluation/pins    {<input id> <revision or digest>}
;;   :evaluation/locality :local | :global

;; E6 typed absence.
{:type/id :arxana/absence :type/revision 1 :record/kind :absence
 :fields [:absence/subject :absence/kind :absence/producer :absence/reason :event-time]}
;;   :absence/kind :session-start :prev-absent :prev-pending :no-source :interrupted ...

;; R1 span ref is a value inside other records, not a record:
;;   {:source <:evidence/id or arxana revision id> :source/digest <sha256>
;;    :range [start end] :unit :codepoint :span/sha256 <sha256>}
```

E1 sources are not a new type: they are futon1b evidence entries referenced by
`:evidence/id` (plus an `:external-reference` for the 象 alias and time basis), and E2
manifestations are existing `arxana_revisions` artifact revisions.

## 3. Fidelity contract (donors)

### `p4ng/plop-alignment-v1` → R7 alignment + E5 evaluations + R3

| Donor field (71 notes) | Count | Becomes |
|---|---|---|
| `id` | 71 | alignment record id alias (kept) |
| `heading_contains` + `quote` | 71 | `:informal` endpoint: a span ref into the paper revision (quote-located, then pinned as a range) |
| `kind` (alignment 24, upgrade 38, question 9) | 71 | `:alignment/kind` |
| `pattern` | 71 | `:alignment/pattern` |
| `date`, `by` | 71 | `:event-time`, `:actor` |
| `text` | 71 | `:alignment/text` |
| `lean[{module decl revision status}]` | all | `:alignment/lean [{module decl}]` + one E5 per entry: subject = the decl at `revision`, result from status (PROVED → met, STATED → failing, ABSENT → failing with absence) |
| `clojure[{repo path symbol revision status}]` (VALIDATED 35, EXISTS-UNVALIDATED 83, REFUTED 16, ABSENT-OR-LOST 10) | all | `:alignment/clojure [{repo path symbol}]` + one E5 per entry (VALIDATED → met, EXISTS-UNVALIDATED → unknown, REFUTED → failing, ABSENT-OR-LOST → failing + E6) |
| `revision_text` | 38 | the upgrade's proposed paper text: an E3 content record + an R2 edit on the quoted span, scope `#{"plop-2026"}` |
| `supersedes` | 5 | R3 `[:supersedes 2]` from this alignment to the named one |
| `for`, `blocking` | 9 | `:requirement` on Joe (`:for "joe"`), `:blocking` as a requirement parameter |

Preserve: every field. Adapt: statuses move from the note into evaluations (D3), with the
original status string kept in the evaluation. Drop: nothing. Tripwire test: import all
71 notes, export them back to `plop-alignment-v1`, and `inject-plop-alignment.py`
validates the round-tripped file exactly as the original.

### F2 `[:text/patch 1]` → R2 edit

Preserve the hunk form `{:start :end :replacement}` in codepoints, `:author`, operation
id. Adapt: the base is span refs in a projection, not an artifact head; replacement text
becomes a content record. Drop: CAS on a head (D2). Tripwire: F2's own tests stay green
(nothing in F2 changes).

### F6 placements → projection entries

Preserve order and attributed, dated placement. Adapt: placement is per projection +
entry (survey A found F6's history keyed per item only). Tripwire: one item placed in two
projections keeps two histories.

### Variants JSON (`para_diff.py` / Mojira `vd`) → renderer output

Preserve the shape the views read (paragraphs, ops, sources). Adapt: emitted from
projections rather than reconstructed by diff; ops come from R2/R6; sources by reference
with text fetched by digest. Extend Mojira's closed op set deliberately (survey B Q8)
only if a new op is needed. Tripwire: the current views render the emitted JSON for draft
2 without change.

## 4. Open rules from the coverage check

- **Partial deletion of a linked span (T3).** IF an edit deletes part of a span a link
  attaches to, HOWEVER Nelson keeps the link on "any part of the content it connects which
  survives" (X:46) and relocation with empty context reports the whole span missing
  (survey A Q7), THEN a link resolves to the surviving characters of its span in the
  projection, and an E6 absence records the deleted part, BECAUSE that is Nelson's rule
  and keeps the deletion visible rather than silently shrinking the link.
- **"Not yet used" for pullacross (T4).** IF a writer wants material not yet used,
  HOWEVER "unused" is relative to a projection and to a set of sources, THEN it is a query:
  spans of the sources named by the projection's nips (or by a requirement) minus the
  spans the projection's entries cover, BECAUSE both sides are already records, so the
  query needs no new type.
- **A reading as an entity (G05).** IF an animal's reading (象, 馬) is evidence other
  records rely on, HOWEVER today it is fields on a turn record, THEN a reading is an E2
  manifestation revision (the animal's output at a digest) so corrections can supersede it
  (R3), BECAUSE claude-13 needs reading corrections and that requires an addressable
  reading.
- **Precedence of prediction over observation (G03).** IF simulation needs "predicted
  before run", HOWEVER a date field is not evidence of order (gap analysis C08), THEN
  precedence is an E5 evaluation over two records whose pins include their commit ids,
  evaluated by commit ancestry, BECAUSE ancestry is checkable where a timestamp is not;
  out of scope to build here (simulation is deferred), specified so the type is ready.
- **Relocation beyond F1 (G13).** IF the relocator only runs for the F1 artifact, HOWEVER
  projections and links need it for any source, THEN relocation is a global evaluator
  (A-eval-global) over any span ref whose source revision moved, writing E5 results,
  BECAUSE the existing relocator already records its candidates as attestations and only
  its artifact filter (`formal_f1_relocation.clj:181`) is F1-specific.

## 5. Revisions from ARGUE's pattern survey (2026-11-17)

Survey: `~/notes/claude-5/argue-pattern-survey-2026-11-17.md` (library patterns that bear
on DERIVE-1/2; six cut against it). Applied here; they supersede the parts of §2–§4 they
name.

1. **Act identity is minted, not derived** (象/行有定名). Every act record (E3, R2–R7,
   unwire, E5, E6) gets an opaque id minted at write time; content digests stay as fields;
   resending uses an explicit idempotency key. Two identical acts are two acts. Replaces
   §2's "content fields are identity fields; attribution and time are included in
   identity for acts". To check against futon1b's id derivation before WP4.
2. **A failing requirement stays failing until discharged**
   (apparatus/open-conditions-live-in-a-ledger, test-registry/bind-the-subject). The
   requirement list shows an R5 as failing until an E5 `:met` with current pins (and
   pinned to the R5 version, 象/制度有时) follows the last `:failing`; a later `:unknown`
   or `:stale` does not discharge it. View test: no sequence of appends reduces the
   open count without a discharge.
3. **Attestations carry independence** (aif/no-self-certification, 象/同构方为同). An
   `:attestation` E5 records `:independent?` (false when the attester authored what is
   attested); the list shows self-attested and independent results apart. An edit to
   attested material marks the attestation stale and puts it on the attester's list
   (apparatus/pin-moves-with-the-population).
4. **Closed vocabularies** (measurement/ghost-as-typed-sorry, memory/surface-the-closed-set).
   E6 `:absence/kind` is the fixed set `#{:session-start :prev-absent :prev-pending
   :no-source :interrupted :unresolved-relocation :not-yet-evaluated}`; an untyped absence
   is a validation error. Evaluator kinds are the five-member enum above. E5 `:unknown`
   splits into `:not-yet-evaluated` and `:cannot-evaluate-here`.
5. **Absences carry their search** (process/spec-silence-pair). E6 gains
   `:absence/search` (the query, candidates, repo/commit searched). "No source" without a
   search is not recorded as a finding.
6. **Alignments state the claim and a falsifier** (contracts/holder-states-the-claim,
   every-entry-has-a-falsifier, declare-the-reductions; 象/引非读). R7 gains
   `:alignment/claim` (`:states :implements :proves`), optional `:alignment/falsifier` and
   `:alignment/reductions`; R7 evaluations record the grade `:named :read :witnessed`.
7. **Imported statuses name their donor** (hygiene/observe-the-authority). E5s created from
   `plop-alignment-v1` have evaluator `{:kind :imported :from "p4ng/plop-2026-alignment.json"
   :at <commit>}`, distinct from a fresh Lean build or Clojure validation.
8. **Memory seeds resolve to turn spans** (memory/name-a-reusable-artifact,
   象/翻译契约). A minus40 memory can seed a query, but a nip's result spans are landscape
   spans; a memory citing no turn cannot be accepted as a span.
9. **The two seed lanes are not merged into one score** (aif/currency-before-merge,
   declare-the-conditioning). Memory-seed and gap-seed candidates are offered as two
   lists; Joe's acceptance arbitrates (the 3/2 reading). `:nip/query` records what each
   candidate was scored against.
10. **Renderers declare loss and refuse typed** (translation/declare-what-is-lost,
    route-the-untranslatable, writing-coherence/name-what-you-drop). Each renderer lists
    what it drops (the form-text renderer drops links, margin notes, provenance); a field
    over its limit is refused with a typed reason, never truncated; rendering re-runs
    local checks (hygiene/receipt-then-gate); "rendered for the form" and "submitted" are
    separate records (象/两种规格, 象/提交非生效).
11. **Views are rebuildable from records** (象/视图出于史). Emitted variants JSON is
    periodically rebuilt and compared; evaluator runs are recorded as acts; D7's
    file-backed store is append-only, not a mutable current state.
