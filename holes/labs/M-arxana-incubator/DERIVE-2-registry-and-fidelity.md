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

## 6. Computed manifestations (2026-11-17, from gap-analysis pass 2)

Pass 2 (`GAP-ANALYSIS.md`, G14–G19; verdict "not yet" on claude-7's test) showed that
DERIVE-1's I2 was written for quoted turns, where each character has one source, and
cannot explain rendered text whose words come from several rules at once (G19). Amended
here; supersedes I2's wording in DERIVE-1 for generated material.

- **I2′ Traceability, widened.** Every character of a rendered projection maps to exactly
  one of: (a) a span of an E1/E2 whose digest still matches; (b) an E3 content record;
  (c) a span of a **generated manifestation** together with its **rule application**
  (below), which lists *all* its inputs. (c) is many-to-one by design: one generated
  sentence may depend on several rules and several declaration parts, and the check is
  that the list is complete, not that it has one element.
- **Rule application** (G15, "generated by"). A record per generated span: renderer sha,
  corpus sha, declaration (+ step + typed term, below), and the ordered rule 'clusion ids
  applied; minted id; event time. Completeness check: re-rendering the declaration with
  exactly these pins and rules reproduces the span byte-for-byte.
- **Rule 'clusion as a decision** (G14). The rule's content (pattern, guard, replacement)
  is one record; the act accepting it (packet, reviewer, merge sha) is another, linked.
  Superseding a rule is an R3 on the content; who accepted it stays on the acceptance.
- **Structural locator** (G16). For generated sources the anchor is (declaration revision,
  step, typed term), not a codepoint span of output; output spans are derived per render
  and joined to the locator through the rule application. Relocation across re-renders is
  by locator, never by text search.
- **Accepted residual** (G17). A disposition record on a *failing* evaluation: defect,
  scope (declaration/sentence), release or projection version, who accepted it, reason.
  It never turns the evaluation into `:met`; the requirement list shows it as "failing,
  accepted for <release>". This is the endgame's "clean except for a documented list".
- **Regression finding** (G18). A record linking a prior accepted state (rule application
  or evaluation), a later failing one, the evaluation that discovered it, and its finder.
  Not every R3 is a regression; a regression is a finding about one.
- **Staleness for generated text.** The fibre of a superseded rule (backward along rule
  applications) is exactly the set of dependent sentences; they become stale (I4).
  claude-7's planted check is this: change one rule in a scratch merge; the old rule is
  superseded, every dependent sentence is stale, and no unrelated sentence is.

## 7. Conflict findings as Arxana records (2026-11-17, claude-5; VERIFY Witness 1, item 4)

DiagramProver now writes conflict findings as pinned EDN (futon3c
`diagramprover/findings.clj`). This section says how they become Arxana records beside the
rule 'clusions, using the jvm-arxana registry (`futon3c/src/futon3c/arxana/type_registry.clj`)
and record constructors (`records.clj`). New types are marked *new*; the rest exist.

| Thing | Record | Identity / notes |
|---|---|---|
| Rule content | *new* clusion `:rule/content` 1, no roles | content: system (`:apm-printer`, …), name, pattern (EDN), guard names, domain-guard names, rewrite (EDN or source locator), priority. Content-addressed: same rule, same id. |
| Rule acceptance (G14) | `:evidence-attestation` on the rule clusion | producer = reviewer; basis `:human`; standing `:verified`; method {packet, merge sha, repo}. Who accepted stays here, not in the content. |
| New rule version | `:supersedes` 1 (successor, predecessor) | existing, acyclic. |
| Conflict finding | *new* clusion `:conflict/finding` 1, roles `:rule-a`, `:rule-b` (clusions, ordered by clusion id) | content: overlap term, status, kinds, settled-by, evaluation, checker sha, schema digest, mode, max-states, host locator (capture file sha + hit, or "transcribed"). A rerun at other pins is a new record; event time is the valid time. |
| Run evidence | `:evidence-attestation` on the finding | producer = checker; basis `:exact`; standing `:verified`; coverage `:complete`, or `:partial` when the bound was hit; content: occurrences, positions, witness path, observations, host ids; source-refs: capture files with sha256. |
| Accepted residual (G17) | *new* clusion `:residual/accepted` 1, role `:finding` | content: scope, release, reason; who accepted it in an attestation with basis `:human`. It never changes the finding's evaluation; a reader shows "failing, accepted for <release>". |
| Staleness | computed, not stored | a finding is stale at time t iff a `:supersedes` record valid at t has its rule-a or rule-b as predecessor. Same rule as sentences (§6, "Staleness for generated text"). |

Not covered here: rule applications (G15) and regression findings (G18), which come from
render history rather than from the checker; they share the rule nodes, and are kept as a
separate edge type so the two can confirm or contradict each other (claude-1 design §5).
Live writes need futon1b valid time (`:event-time` → valid-from), which awaits Joe's restart;
until then the packets build and test the records in memory.

Packets: 4a registry declarations and `findings->records` (pure); 4b staleness over a record
set, with the planted check (supersede one rule: its findings go stale, no others do).

## 8. Sentence records: design brief (2026-11-18, claude-5; VERIFY Witness 1, open question 5)

§6 says what sentence records must do. This section says how to build the first of them, on one declaration: a97J08 `apm_a97j08_boundary_sum_aux_disjoint`, whose closing sentence "In all four cases, we obtain a contradiction" is Witness 1's named sentence.

**What exists to build on.**
- Arxana already has `:manifestation-scope` as an endpoint kind, a span of an artifact revision. A rendered page is an artifact revision, and a sentence is a scope in it.
- The Step probe (mfuton-share codex-12 `b90ed3f9`) gives each Step a structural key: digests of its case expression and right disjunct. That is the locator §6 (G16) asks for, independent of the output text.
- The replay runner (`f87a1048`) runs named Step rules as they are, so a rule can be switched off for one render without editing the printer.
- The rule records, supersessions, findings and rule-set diffs are in the store (§7, and mission §5).

**Two ways to learn which rules a sentence depends on.** We use both, because each is honest about a different thing.
1. **History.** Render the declaration at each merge in the chain that touched it, on claude7/integrate. The merges are:
   - Q55 7b16766b;
   - Q56 2d809ff1 and its review fix 50444f21;
   - Q61 20985f78;
   - Q66 715a3f90;
   - the Q67 repins 23050857;
   - Q78 db16b1ff;
   - Q83 e73a6b2e;
   - then Q88 once it is merged.

   A sentence's wording at each merge, with the merge's agent (commit trailers) and time, is its history. This is the "earlier wordings with actors and times" of Witness 1, and the data that question 1 (patterns in the history) needs.
2. **Counterfactual.** At one commit, render with one Step rule switched off, for each rule in turn. The sentences whose text changes depend on that rule. This gives the rule-application record (§6, G15) its rule list. It is complete only for the rules that can be switched off, and the record must say so: a field naming the rules tested, not a claim that the list is all the rules.

**Records.**
- **Artifact revisions:** each render is one, with pins for the renderer commit, the corpus sha and the declaration.
- **Sentences:** a sentence is a `:manifestation-scope` in a render. Its locator is the Step's structural key plus the Step kind, and the text span is derived per render.
- **`:rule/application` 1 (new):** source role `:sentence`, a manifestation scope. The content holds:
  - the rule ids found to change it;
  - the rules tested;
  - the method (`:counterfactual` or `:history`);
  - the renderer sha.

  The completeness check from §6 applies to the tested rules only: re-rendering with exactly these pins reproduces the span.
- **Staleness:** a sentence is stale at t when a rule in one of its applications has a supersession valid at t. This is the same direct-predecessor rule as for findings, read through `:role-reverse` as `store-finding-view` does.
- **The planted check (Witness 1):** in a scratch branch, supersede one rule. Every sentence whose application names it is stale, and no other sentence is.

**Order of work.**
- **8A, discovery (codex-12, mfuton-share, read-only):** render a97J08 at each chain commit and report, per commit, the closer sentences with their Steps. Find out whether the 2A structural keys stay stable across commits for the "same" sentence. The probe module does not exist at old commits, so the report must say how the keys were obtained there. If they are not stable, report what is.
- **8B (Python):** the counterfactual runner, one render per switched-off rule, at the current head.
- **8C (Clojure, futon3c):** the `:rule/application` type, sentence and render records, a store-backed sentence view, and the planted check, first in memory and then, with Joe's go, live.

**Not decided yet.**
- Word-level readings: the 72 to 79 words with several readings (mission §5, kind 3) are rules too. Whether they get the same treatment depends on 8A.
- Whether a sentence should be one Step or a smaller span: one Step for now.

**8A result (codex-12, 2026-11-18; data `~/storage/arxana/sentences/a97J08-closer-history.json`, report `~/notes/codex-12/sentence-history/REPORT.md`).** a97J08 was rendered at ten commits, from just before Q55 to head.

The outer split's expression key and lifted right-disjunct key are the same at every commit. The Step as a whole is not stable:
- its arity is recorded only from Q78;
- the case-term key exists only from Q83;
- closers disappear at Q56 and are merged into one at Q83.

So the locator is a **source role**, not a Step: (module, declaration, corpus sha, split expression key, `case-conclusion`). A render maps that role to zero, one or several sentences. Codex verified exactly one matching split in every capture. For declarations with repeated equal disjunctions, a source occurrence path would be needed as well; this is proposed, not measured.

The wording history of the role's case-closers, all merged by claude-7 on 2026-11-17:

| Commit | Merge | Wording |
|---|---|---|
| pre-Q55 | — | "In either case, the claim holds." |
| 7b16766b | Q55 | "This is a contradiction." |
| 2d809ff1 | Q56 | none emitted |
| 20985f78 | Q61 | none emitted |
| 50444f21 | Q56 review | "In either case, we obtain a contradiction." |
| 715a3f90 | Q66 | "Hence we obtain a contradiction." |
| 23050857 | Q67 repins | "Hence we obtain a contradiction." |
| db16b1ff | Q78 | "In either case, we obtain a contradiction." (now 5 sentences) |
| e73a6b2e | Q83 | "In all four cases, we obtain a contradiction." |
| 1970149b | head | "In all four cases, we obtain a contradiction." |

Two patterns are visible as data:
- **A loss and a repair.** The sentences vanish at Q56, which is claude-7's undo: found during Q61's run and fixed by 50444f21. The proofs at Q56 and Q61 are byte-identical, so Q61 did not cause it.
- **A reversal.** "In either case, we obtain a contradiction." at 50444f21 is replaced at Q66 and comes back at Q78.

This is the first evidence for open question 1, that rules undoing each other show in a history. The history is of one sentence role across merges, not of supersession records.

**Correction to this brief.** The packet asked for "the Q56 undo found at Q61". That was my misreading of claude-7's note, which says the defect was found during Q61's run.

**8B result (codex-12, 2026-11-18; data `~/storage/arxana/sentences/a97J08-switch-off.json`, report `~/notes/codex-12/sentence-counterfactual/REPORT-r2.md`).**
- **Reverting whole merges at head failed.** Every unit except the Q67 repins conflicts with later work. Q67, which only touched tests, reverts cleanly and leaves the proof byte-identical.
- **Switching off single functions worked.** Codex inventoried 35 functions that the chain changed. 23 were switched off successfully at runtime, in one of three forms:
  - (a) a pass replaced by identity;
  - (b) a function the merge added, with its call site falling back to the earlier path;
  - (c) a function the merge changed, replaced by its parent-commit body.
- **The rest were not tested,** each with its reason recorded: 2 were incompatible with head (Q55 parent bodies), 4 are inactive on a97J08, and 6 had no defensible switch-off.
- **The Q83 cross-check agrees.** Its source-only revert and identity for `_merge_right_or_splits` give byte-identical proofs.

**Dependence.**
- All five case-conclusion roles depend on the same 14 of the 23 tested functions, and none depends on the other 9. Some functions change other sentences and leave the roles alone: `_is_false_tex`, `_sentence`, `_sentence_core`.
- **Limit of form (c).** A parent body also removes the later edits made inside that function, so a dependence found that way belongs to the function's current body, not to the merge that first changed it.
- **What the application records can claim.** Each sentence role depends on these functions, out of this tested set, under single-function intervention. They make no claim that the list is complete.

**8C result (codex-13, 2026-11-18; futon3c 3e95d08ca, fast-forwarded to master; report `~/notes/codex-13/sentence-records/REPORT.md`).** Three types, in memory only, nothing written live:
- `:sentence/role 1`: no endpoints; content is the locator (module, declaration, corpus sha, split expression key, role).
- `:sentence/manifestation 1`: endpoint `:role`; content is the renderer commit, corpus sha and the wordings.
- `:rule/application 1`: endpoints `:sentence` and `:rule`; content is the method (`:switch-off`), head commit, corpus sha, `:tested-rules` and `:interventions`.

Two decisions came from Codex stopping on real conflicts:
- **Rule identity.** A rule is one record per function: qualname and head commit. `_merge_right_or_splits` reuses the live Q83 record `777d1f59…` unchanged, so the first write and the sentence records name the same rule. Which unit introduced a change is kept in the application, not in the rule's identity.
- **Several interventions per pair.** The 23 tested switch-offs touch only 17 functions; six were changed by two units. For the outer role, `proof_body` at Q55's parent body gives "In either case, the claim holds." and at Q66's gives "In either case, we obtain a contradiction.". So each application carries a vector of interventions (unit, form, head wordings, switched wordings) in chain order, and a scalar field would have lost one of them.

Counts: 17 rules, 5 roles, 5 head manifestations, 45 applications (9 functions × 5 roles; the 14 interventions that changed the roles fall on 9 functions), 72 records, all accepted by the real write path in memory. The 10 history manifestations validate too.

Readers: `sentence-view` (store-backed through `:role-reverse`, as `store-finding-view`) and `rule-fibre` (the roles whose applications name a rule). The planted check passes: superseding Q83 makes all five roles stale with that supersession as the reason, and its fibre is exactly those five; superseding `_sentence_core` makes none stale and its fibre is empty. A future-dated supersession is invisible; an application whose rule endpoint is moved into content drops out of the fibre; a stale or truncated index gives `:inconclusive`.

Limits:
- Other sentences changed by the switch-offs are not recorded: the capture gives their expression keys, not a source-role locator, and no identity is made up from wording.
- Before Q83 the history has only declaration-wide closer wordings, so the 8 earlier manifestations attach those to the outer role (two copies of each wording, for example). The records do not say this. They should not be written live until the content says the wordings are declaration-wide.

**Scope revision (codex-13, 2026-11-18; futon3c 3af38e409, on master).** `:sentence/manifestation 2` adds `:scope` (`:role` or `:declaration`) and `:renderer-commit-time`. The 8 pre-Q83 history records are now revision 2 with `:scope :declaration`; Q83's (`d0934293…`) and head's (`dd87f2eb…`) stay revision 1, the 6 live revision-1 records all being role-scoped. `role-manifestations` reads both revisions: 10 for the outer role (8 declaration-scoped, 2 role-scoped), 1 for each inner role; a stale or truncated index on either gives `:inconclusive`. Limit: the registry does not validate content values, so a bad scope is refused by the builder only; a hand-made record with another value would pass the write path. The first round stopped on two errors in my packet (the Q83 and head ids swapped, and 14 manifestations expected for the outer role, which has 10), caught by a check against committed data.

## 9. What a record carries besides its content: requirements (2026-11-18, claude-5; mission open question 7)

Source: claude-1's reading of the endgame list (most rows carry limits stated only in prose), the sentence records (§8; "the five roles depend on Q83" holds only for single switch-off at head), Joe's point after packet 9 that submissions should be validated before they are final, and a reading of `type_registry.clj` and `records.clj` at futon3c 3af38e409. Joe (2026-11-18): proceed with the writes; the design discussion is this section.

What exists. Beside clusion content, Arxana has three layers whose values are checked on write:
- `:evidence-attestation`: subject, producer, basis (`:exact :parser :model :human :kernel :composite`), standing (`:unverified :verified :contradicted :disputed`), coverage (`:none :partial :complete`), method (a free map), content, source-refs.
- `:assessment`: subject, verdict (`:candidate :accepted :rejected`), actor, policy, reason, time, all required.
- `:operation-attempt` receipts, against the registered method versions in `dynamics-v1`.
Typed ports (`:endpoint/type-revisions`) restrict what an endpoint may point at; only the d6 types declare them.

The conflict findings (§7) use an attestation: producer the checker sha, method the mode and state bound, coverage partial or complete. That is why they state their own bounds. The sentence records (§8) use none.

**R1. Every observation is attested, and its method names what it ranged over.** An observation is a record whose content was produced by running something (a checker, a render, a switch-off, a census). Its attestation's method states the procedure, its pins, and its denominator: the set it was run over and what was left out, with counts.
- Instances: single-function switch-off at head on a97J08, 23 of 35 functions tested, 12 untested with reasons (§8); claude-7's census over the 1,381 records that had a tracked family; guard escape probed on one beta path.
- Status: being applied to the live sentence records (packet 10, codex-13).
- Check: a sentence application with no attestation, or with an attestation whose method lacks the denominator, is reported by a store-side audit (to build).

**R2. `:coverage :complete` names what it is complete over.** Today `:complete` is a grade with nothing to grade against. Proposed: a new attestation revision in which a coverage other than `:none` requires an `:extent` (what was ranged over), refused on write when absent.
- Instance: the conflict findings say `:complete` with the state bound in method, so a reader can recover the extent; a census row saying "30 records, complete" could not.
- Open: whether to revise the attestation type or require the extent inside method for named procedures.

**R3. A value says what kind of value it is.** Measured, lower bound, forecast, not measured. A number field holds none of these distinctions (claude-1: class 7 "at least 34" from a scanner known to miss names; "about 70 after the merges" is a forecast).
- Proposed: a value-kind field on the attestation (or on the content of count-bearing types), with "not measured" distinct from absent.
- Related: a reader cannot tell withheld records from no records (the 8 history records before packet 9). Recording the denominator (R1) is what separates "looked, found none" from "never looked".

**R4. A judgement carries who, why, how far, and what reopens it.** Assessments have actor, policy and reason; `:residual/accepted` has scope, release and reason in content. Neither has a reopen condition, and the two split a judgement's parts between them.
- Proposed: a residual acceptance is the residual record plus an assessment by Joe, and the reopen condition is required in one of them (decide which before the first residual is written).
- Instance: no residual is proposed now (endgame list), so this can be settled before it is needed.

**R5. Rulings on a record contract are reachable from the type.** "A rule record is a function, not the unit that introduced it" changes what a count of rule records means, and so do "an application keeps every intervention" and the role-scope of revision-1 manifestations. Today these are source comments and prose.
- Proposed: one record per ruling, linked to the type revision it governs and citing the prose (here §8). The d6 decision stages may fit (a ruling decides between candidate contracts); unchecked.
- Also covers classes set by ruling (endgame class 13, Joe's notation ruling) and shapes forced by a type (class 9 is a rule against a test pin, which a two-rule finding would misdescribe).

**R6. Submissions are validated before they are final.** (Joe, 2026-11-18, after packet 9 swapped two live ids and expected 14 records where there are 10.)
- Today: the write path checks record shape, endpoint kinds, attestation and assessment enum values. Enum values inside clusion content are checked only by builders (packet 9's `:scope`: a hand-made record with another value would pass). Endpoints are checked by type revision only where typed ports are declared. Nothing checks a claim about existing records: a label resolving to an id, or a count.
- Proposed, in order of cost: (a) enumerated content values declared in the type and checked on write; (b) typed ports by default on new types (application `:rule` to `:rule/content`, `:sentence` to `:sentence/role`); (c) for packets and bells that cite live records, a resolver that maps a label (declaration, commit, role) to its id through the store, so the citation is checked when it is written. (c) is the one that would have caught packet 9.

Order of work: R1 now (packet 10). R6(a) and (b) next, as one small registry change each. R2 to R5 wait for a second family of records to test them on (the endgame census is the natural one); R4 must be settled before the first residual is written.
