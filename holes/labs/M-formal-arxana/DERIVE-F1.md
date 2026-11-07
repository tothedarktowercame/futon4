# DERIVE-F1 — from a settled operator turn to an action the War Machine can take up

claude-13, 2026-11-07. Draft for Joe. Built on MAP Q1–Q10
(`MAP-Q*.md` in this directory; table in the mission §2).

## 0. Operator decisions recorded this turn (Joe, 2026-11-07)

- **Lean substrate: v4.29** (the apm-lean toolchain,
  `leanprover/lean4:v4.29.0-rc8` with Mathlib master). LeanEuclid material
  (v4.19.0) is to be adapted to it rather than kept on a second pin.
  Applies to F3 and F5.
- **Demoable and usable in Emacs.** Every fixture must be viewable in Emacs
  through futon4a's client for the JVM Arxana backend
  (`futon4a/lisp/futon4a-arxana.el`, arxana.v1 envelopes). This does not
  change the semantics below; it adds a projection requirement to each
  acceptance (§6) and means futon4a, not the legacy `futon4/dev/arxana-*.el`
  client, is the Emacs surface. (MAP-Q5/Q7: the legacy client's `arxana/*`
  writes are refused by futon1b's reserved-namespace gate anyway.)

## 1. What F1 has to show

An operator turn becomes, through attributable records, a candidate action
that the War Machine's selector can see and that an admission authority can
admit or refuse, with a receipt when it is carried out. The same record ids
appear in three projections: the linear conversation, a graph view, and a
candidate-action (GCC) view. Changing the prose of a turn relocates; changing
its mark or its reading makes new records and leaves the old ones.

MAP-Q3 found every compiler piece present and tested, and none of the links
between them wired. MAP-Q2 found no record that confirms or rejects a
reading. F1 is therefore mostly wiring plus two new record kinds, not a new
compiler.

## 2. The chain and its records

| # | Record | Exists? | Identity | Authority (who may write it) | Convergence |
|---|---|---|---|---|---|
| R1 | Source act: chat-turn evidence `emacs-<md5>` | yes (Q2) | evidence id | the Emacs/REPL turn writer | append-only, duplicate id idempotent |
| R2 | Reading: 象 analysis of the turn record (`turn-<id>.json.analysis.json`, settled as `e-xiang-turn-<id>`) | yes (Q2) | turn id + interpretation version + source sha256 | 象 seat (model interpreter) | publish-once (turn_store.clj:141-156) |
| R3 | **Reading assessment** | **new** | content digest over subject, verdict, actor, policy, evidence | operator, or an interpreter named in a trust profile | append-only; several may coexist |
| R4 | Intent (`arxana.compile.intent/make-intent`) | shape exists, no producer (Q3) | `arxana:intent:sha256:` over semantic fields | the intent producer (§3), recording the R2 interpreter as `:intent/interpreter` | content-addressed, idempotent |
| R5 | Candidate action (`compiler/compile-action`) | shape exists; only `:edit/replace` supported | `arxana:action:sha256:` | the compiler (pure) | content-addressed, idempotent |
| R6 | Admission decision (`admission/make-admission-decision`) | shape exists, never called | `arxana:admission:sha256:` | an external policy authority; for F1, Joe | append-only |
| R7 | Receipt (`admission/make-receipt`) | shape exists, never called | `arxana:receipt:sha256:` | whoever carried the action out | append-only, never rewrites predictions |
| R8 | GCC links between R1–R7 | registered types, count 0 (Q3) | caller-supplied `:hx/id` (Q1: default id sorts endpoints and loses roles) | the writer of the later record | idempotent upsert by id |

### R3 — the reading assessment (new)

IF a reading is to feed an intent, HOWEVER 象/释义非授 says a reading never
creates, ends or moves a commitment, and today readings stay
`human_approved: false` forever with no route to change it (Q2), THEN add an
assessment record whose subject is a reading version, with verdict
`:accepted | :rejected | :amended`, actor, policy, evidence ids and, for
`:amended`, the replacement fragment, BECAUSE that gives an attributable
place for "yes, that is what I meant" without letting the reading itself
become authority, and it matches the assessment shape correspond.clj already
uses (verdict, actor, policy, evidence ids, content-addressed id).

- Subject is typed: `{:subject/kind :xiang/reading :turn/id … :interpretation/version … :source/sha256 …}`.
  This closes, for this record kind, the Q6 gap that assessment subjects are
  untyped strings.
- Accepting a reading is evidence acceptance, not action admission (D6). R6
  stays separate.
- Default when no assessment exists: the reading is `:unassessed`, and any
  intent drawn from it is compiled at most `:conditional` (the existing
  trust-profile rule does this if the 象 seat is not in the profile).

## 3. R2 → R4: which fragments become intents

Only fragments whose intent is an act on the work produce an intent:
`ask-action`, `delegate`, `propose`, `prioritize`, `constrain`, `redirect`,
`defer`, `withdraw`, `approve` (when its target is an earlier proposal or
question). `gist`, `report`, `explain`, `clarify`, `qualify`, `verify`,
`unresolved`, `collect`, `explore`, `continue`, `extend`, `retract` do not;
they are context for the intents near them.

IF every action-bearing fragment becomes an intent, HOWEVER a reply mark in
colon form (`🈳:`) answers the other party's paragraph rather than declaring
the operator's own act (reply-mark colon form, 2026-10-04), THEN the producer
takes the intent type from the reading's fragment intent, never from the
glyph, and records the glyph and its form (colon/no colon) as evidence,
BECAUSE the fixtures below have exactly this case: `🈳: Please don't get
distracted … send that to claude-19` is read as `delegate`, and `🈯: Please
send out the rest of MAP` as `ask-action`, while the glyphs mean "answering
your 🈳/🈯".

Intent fields:
- `:intent/source-act-id` — R1 evidence id.
- `:intent/type` — `:turn/<fragment-intent>`, e.g. `:turn/delegate`.
- `:intent/target-ref` — the fragment's target, resolved where possible to a
  record id (mission `M-…`, agent id, job id, turn id); the unresolved
  target text is kept as `:target/text`.
- `:intent/proposed-change` — the fragment text and its codepoint span in
  the source turn.
- `:intent/interpreter` — the R2 seat, model and interpretation version.
- `:intent/evidence` — R2 id, R3 ids if any.
- `:intent/alternatives` — other fragment readings of the same span, when
  seats disagree (Q2: classical draft vs model reading).

## 4. R4 → R5: compiling a conversational intent

The compiler supports one intent type, `:edit/replace`, grounded by an
artifact revision and a resolution assertion. Conversational intents do not
target a byte range.

IF F1 needs candidate actions from `:turn/*` intents, HOWEVER the compiler's
grounding is defined for artifact edits and its result must stay exactly as
strong as its weakest evidence, THEN add one operation derivation per
supported `:turn/*` type and one grounding kind, `:grounding/reading`, whose
evidence is the R2 reading (with its source sha256) and any R3 assessments,
classified as: accepted assessment → grounded; no assessment → weak
(obligation `:reading/assessment-required`); rejected → invalid; amended →
grounded on the amended fragment, BECAUSE this reuses the existing result
algebra and obligation mechanism instead of a parallel compiler, and keeps
"unassessed reading" visible as an obligation rather than silently trusted.

First supported types, chosen because today's fixtures need them:
- `:turn/delegate` and `:turn/ask-action` → operation `:agency/dispatch`
  `{:to <agent or pool> :work <target-ref> :packet <ref or nil>}`.
- `:turn/constrain` and `:turn/propose` (accepted) → operation
  `:mission/record-decision` `{:mission … :decision <fragment>}`.
Everything else returns `:unsupported` explicitly, as now.

## 5. R5 → the War Machine (revised after claude-6, 2026-11-07)

How the field is built (claude-6, citations checked by claude-13):
`futon2/src/futon2/aif/outer_observation.clj` `giraffe-field` (:216) /
`outer-observation` (:322). The field is the pinned Giraffe pass order
restricted to open registry entries (missions, excursions, tickets loaded by
`target-authority`/`open-entries`, :89-97, :150-154), each needing a source
declaration (`wm/construction_inputs.clj:60`). Nothing else enters it:
declarations and cascade proposals only enrich an already selected task
(`futon2/scripts/futon2/report/war_machine.clj:6620-6623`). Tickets with an
operator Gate line are excluded from automated selection (TICKET-GATE-1,
futon2 `f2c73fb55`). Stopping scores ~10.8-12.2 against 0.8-2.9 for chosen
items, mostly a fixed altitude term (`futon2/resources/wm/outer-abstention-altitude.edn`),
so a ready item in the field is in practice acted on.

The first draft put candidate actions into the field as a new kind of item.
That is withdrawn: there is no seam for it, and because stopping almost
never wins, a field item would carry nearly a command's force without the
record of one. Instead, a candidate is routed by what it is:

| Candidate | Route | Why |
|---|---|---|
| Work Joe asked for and an agent did directly (both fixture dispatches) | No WM step. R6 records the operator's direct admission (`operator-direct/v1`), R7 the receipt. | The operator already chose; the WM would only be re-deciding. |
| Concrete work not yet done | The persistence step writes a ticket `T-*.md` in the target repo's `holes/tickets`, with the operation as its Status, a "What would close this" section, and the R4/R5 ids, warrants and obligations in its body. | This is a document the registry already loads; Giraffe reads it, the loader admits it, and it competes with a pinned source. No new WM machinery. |
| Concrete work whose reading is not yet assessed (obligation `:reading/assessment-required`) | Same ticket, plus an operator Gate line. | TICKET-GATE-1 then keeps the WM off it until Joe's R3 assessment lands and the Gate line is removed. The compiler's obligation and the WM's gate say the same thing. |
| A stated want about what should count as progress (`constrain`, accepted `propose` about outcomes) | Recorded as `:mission/record-decision` and offered as evidence for C learning; not a field item. | C holds preferences over outcomes and is learned (2026-10-28); one turn should not edit it directly. How this evidence enters C learning is left to ARGUE. |

IF intents are to reach the WM, HOWEVER the field accepts only registry
documents and treats a ready item as near-certain to be acted on, THEN
concrete work reaches it as a generated ticket, gated until assessed, and
stated wants go towards C, BECAUSE this uses the existing loader, source
pinning and Q7 audits unchanged, and the Gate line keeps "wait for me"
explicit. Before many intents are routed this way, the altitude term should
be looked at (claude-6's caution); F1 routes at most a handful.

## 6. Acceptance fixture (real, from 2026-11-07)

Three operator turns from claude-13's session `ed69fb38-…`, already settled:

| Turn | Evidence (R1) | Reading (R2) | What happened after |
|---|---|---|---|
| `turn-4qzbnC` | `emacs-f64715501cdad01207ad16917e72298a` | `delegate` → "futon1b allocator investigation" | claude-13 belled claude-19, job `invoke-1794074631008-15333-c06b3d64`; fix futon1b `649d2a1` |
| `turn-pHxXNI` | `emacs-0a4b1b98bc8da17eacc0068f5b50c79c` | `ask-action` → "remaining MAP questions" | four jobs `invoke-1794075673…`–`…675358…`; MAP-Q4/Q8/Q9/Q10 commits |
| `turn-PdGx4U` | `emacs-157d13f34a4e0c84f451c3aadac70a7d` | `propose` (Lean 4.29), `approve` (start DERIVE F1), `qualify`, `constrain` (demoable) | §0 of this note |

F1 passes when, from these retained records alone:

1. R3–R8 are produced for each action-bearing fragment, with no hand-written
   ids. `turn-4qzbnC` and `turn-pHxXNI` yield `:agency/dispatch` candidates;
   `turn-PdGx4U` yields two `:mission/record-decision` candidates and one
   approval linking to claude-13's earlier question.
2. Before any R3 exists, every candidate is `:conditional` with obligation
   `:reading/assessment-required`. After Joe's accepting assessment, the
   same candidate id stays the same (assessment is evidence, not identity)
   and becomes `:compiled`.
3. The dispatches that actually happened are recorded as R6 (actor joe,
   policy `operator-direct/v1`) and R7 (outcome `:applied`, evidence = job ids
   and commit shas), after the fact. The fixture tests that a receipt can
   point at real effects; it does not pretend the WM chose them.
4. Three projections show the same ids: the 象 stepper or REPL buffer
   (linear), a futon4a buffer rendering the R1→R8 chain from JVM Arxana
   (graph), and a candidate-action list (GCC). Each rendered item can be
   traced back to its record id (Q4 requirement).
5. Editing the prose of `turn-pHxXNI` (a new source revision) relocates the
   fragment through the F0 resolver; the old intent remains, the new one
   cites the new revision. Changing the reading of a fragment (say, `🈯` taken
   as `clarify`) makes a new R2 version and new R4/R5, and the old candidate
   is marked superseded by an R3 `:rejected`, never deleted.
6. One not-yet-done concrete intent produces a gated ticket; a Giraffe run
   reads it and `giraffe-field` lists it with the Gate exclusion. After an
   accepting R3 and removal of the Gate line, the next run lists it as
   selectable. (The two fixture dispatches take the no-WM route.)

## 7. Where records live

IF R3–R7 need a durable home, HOWEVER the JVM correspondence repository is
in-memory only (Q7), the generic entity route is last-writer-wins, and the
futon1b arxana route supports only `:revision/ingest` (Q5), THEN write
R3–R7 as evidence entries with their content-addressed id as the evidence id
(append-only, duplicate id is an idempotent no-op), and R8 as hyperedges of a
`gcc/*` type (not `arxana/*`, which the generic route refuses) with an
explicit `:hx/id` built from the ordered, role-named endpoints, BECAUSE
these are the two routes that already have the right convergence behaviour
for immutable records, and neither requires the arxana conditional route to
grow new operations first. Moving them under the arxana route is a later
choice, once that route supports more than revision ingest.

## 8. Increments for INSTANTIATE (one packet each)

1. R3 record: constructor, validator, digest id, tests (pure; futon3c
   `arxana/compile/`).
2. Intent producer: settled turn → R4 records per §3, pure function over a
   turn record + analysis; tests on the three fixture turns' files copied
   into test resources.
3. Compiler: `:grounding/reading` and the two operation derivations; tests
   for each status transition in §6.2.
4. Persistence: write R3–R7 as evidence and R8 as `gcc/*` hyperedges; one
   hook after `settle-evidence!`; hermetic store tests.
5. futon4a view: render the chain for a turn id from JVM Arxana.
6. Ticket writer: generated `T-*.md` per §5, with Gate line while unassessed;
   test against `mission-registry/load-tickets` and one Giraffe run.

Each packet carries the gates (clj-kondo, check-parens, tests) and names its
acceptance item from §6.

## 9. Not in F1

- Choosing or running actions automatically. F1 makes candidates visible to
  the WM; admission stays external.
- Cui bono / fall-through argument structure (F6 and later), though R3's
  assessment shape is meant to serve it too.
- Collaborative editing (F2).
