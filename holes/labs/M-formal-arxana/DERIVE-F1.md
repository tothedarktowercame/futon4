# DERIVE-F1 — intent-marked conversation

claude-13, 2026-11-08. Second draft; the first widened F1 beyond the
mission and was reverted (futon4 75f08b3, bf6fded). This one follows the
mission's F1 paragraph and completion criteria only, with the author's
clarifications (codex-21, 2026-11-08, job
invoke-1794098667698-15408-0169217d). Build-plan details come from codex-21's
second and third rounds (jobs invoke-1794099069915-15414-903e7789,
invoke-1794099152274-15417-a197d5c0).

## 1. F1 as written (mission §1)

> Use real proforma turns containing `㊯`, `㊢`, and at least one ambiguous or
> incorrect mark. Materialize the source turn, interpretation 'clusion,
> target, evidence, and standing. Project the same records as: marked linear
> conversation; an outline or graph view; and a bounded GCC candidate-action
> view. Editing prose preserves the act identity where warranted; changing a
> mark proposes a new interpretation rather than rewriting history;
> ambiguity remains visible.

Completion criteria that F1 owns: F1 passes end to end on retained real
fixtures with the same record identities in all three projections; editing
an F1 manifestation exercises relocation and interpretation history instead
of refusing because bytes changed.

Operator constraint: demoable in Emacs through futon4a's JVM Arxana client.

## 2. Author's clarifications (codex-21, round 1)

1. **GCC view** = the existing action-bearing fragments and their targets,
   projected into a bounded, explicitly non-executable view. No D6 candidate
   compilation in F1 (that is F4).
2. **Storage** = extend the reserved JVM Arxana / futon1b arxana route with
   the minimal typed operations F1 needs. No generic non-`arxana/*`
   workaround. In-memory is acceptable for a spike, not for acceptance.
3. **Edited artifact** = a successor manifestation (an exported conversation
   document, edited in futon4a). The settled turn stays immutable; readings
   relocate onto the new revision through F0. Preserved act identity is an
   evidenced interpretation, never a mutation.
4. **Ambiguous vs incorrect** = two competing 'clusions show ambiguity.
   "Incorrect" needs an attributable assessment (who rejected which
   interpretation, under what policy/reason), with nothing deleted.
5. **Standing** is three separate things:
   - resolution state: `unique | ambiguous | missing | stale`;
   - interpretation assessment: `candidate | accepted | rejected`;
   - evidence qualification: `exact | parser-attested | model-attested |
     human-attested`.
   "Verified" must not mean both "the mark parsed" and "the reading is right".

## 3. What already exists (MAP)

- Source turns, readings, marks and alignments are produced live on every
  REPL turn (MAP-Q2): chat-turn evidence → turn record → 小象 draft → 象
  reading → settled `e-xiang-turn-*` evidence; 馬 alignment maps reply marks
  to acts.
- F0 resolver relocates scopes across content-addressed revisions (futon3c
  `a483b2e0f`).
- futon1b arxana conditional route: CAS head + idempotent message id, tested
  two-writer race (MAP-Q7); accepts only `:revision/ingest` (MAP-Q5).
- futon4a renders resolve/correspondence results with typed refusals
  (MAP-Q4/Q6); writes nothing.

## 4. Records (codex-21, round 2)

F1 materialises, per fixture turn:

| Record | Store shape | Notes |
|---|---|---|
| Turn content | entity | the turn as content |
| Source manifestation | immutable artifact revision (`:revision/ingest`) | the settled turn's bytes; never edited |
| Mark occurrence | entity: glyph, form (colon/plain), revision-pinned locator | evidence used by an interpretation; **not** itself a 'clusion |
| Intent reference | controlled vocabulary entry | e.g. `delegate`, `ask-action` |
| Target | entity or reference | mission, agent, job, turn |
| Interpretation 'clusion | `arxana/*` hyperedge, explicit semantic-digest id | role-named endpoints: source, mark (if any), interpreter, intent, target; method/version; evidence qualification; interpretation state |
| Evidence references + qualification | fields on the 'clusion | `exact / parser-attested / model-attested / human-attested` |
| Resolution observation | record | `unique / ambiguous / missing / stale` |
| Assessment | record naming a 'clusion id as subject | `candidate / accepted / rejected`, actor, policy, reason |
| Supersedes | optional 'clusion | when a later interpretation revises an earlier one |

Rules:
- A 'clusion's id digests its **ordered, role-labelled** endpoints, not the
  futon1b default sorted-endpoint id (MAP-Q1). Roles are validated route
  data.
- No write mutates an existing record. A changed interpretation mints a
  successor (and may assert `supersedes`).
- Do not store each 'clusion twice (entity and hyperedge) unless the store
  needs an index projection.
- Projection receipts are test traces, not domain records.

## 5. Operations (codex-21, round 2)

| Operation | Kind | Effect |
|---|---|---|
| `clusion/assert` | write, new | idempotently append one immutable, digest-identified 'clusion |
| `assessment/append` | write, new | append an attributable assessment of a named 'clusion |
| `revision/ingest` | write, exists | manifestation bytes (source turns, exported conversation) |
| `entity/get`, `graph/query` | read, exist | all projections read through these |

Both new writes are added to the JVM Arxana registry (futon3c
`arxana/schema.clj`) **and** to the reserved futon1b conditional route
(`futon1b_arxana.clj`, today `:revision/ingest` only), with namespace,
atomicity and idempotency tests like the existing two-writer race test.

**Exported conversation.** Deterministic Markdown with marks inline, plus a
deterministic sidecar manifest mapping turn ids to paragraph/codepoint
ranges. futon3c owns the versioned exporter (it reads settled turn
evidence and must reproduce canonical bytes; a script is acceptable at
first if its version and configuration are recorded). futon4a opens or
invokes the export; it does not author it. The Markdown is ingested with
`revision/ingest`; strategies are registered against its first revision and
reused over edited successors, so F0 relocates readings.

**Settled in round 3 (codex-21, job invoke-1794099152274-15417-a197d5c0):**
- *Intent vocabulary.* Increment 1 mints one versioned
  `intent-vocabulary/v1` record from a canonical futon3c EDN resource.
  Interpretations reference its id/revision plus the intent id. The three
  hand-kept copies (turnfeed `intent-mark`, `xiaoxiang-preview.el`,
  reply-proforma.md) are generated or tested from it.
- *Write path.* JVM Arxana validates operation semantics, then writes
  through `store_http`; futon1b's conditional route is the sole atomic
  persistence authority and validates storage invariants. Two layers, not
  two competing validators.
- *Golden records.* Executable inputs and expected records live in futon3c
  test resources (e.g. `test/resources/arxana/formal_arxana/f1/`). futon4
  keeps the design, the evidence report and pinned fixture digests, not a
  second copy.

## 6. Fixtures, increments, acceptance

**Fixtures.** Session `ed69fb38-…` (2026-11-07), settled turn records whose
operator text and agent reply between them carry `㊯`, `㊢` and colon-form
marks:
- `turn-4qzbnC` — "🈳: … send that to claude-19" (read `delegate`; the reply
  carries `㊯`);
- `turn-pHxXNI` — "🈯: Please send out the rest of MAP" (read `ask-action`;
  the reply carries `㊢`);
- `turn-PdGx4U` — propose / approve / qualify / constrain.

- `turn-iWVzvT` (evidence `emacs-f8d74e655e0c6d272818528c367c7ec1`) —
  "Please continue to develop the clarifications … through F6 …"; 象-sonnet
  read s1 `ask-action`, s2 `constrain`, s3 `constrain`. This is the
  **assessment fixture**: Joe judged that reading (below).

**Operator judgement (the real assessment for the acceptance case).** Joe,
2026-11-08, turn `turn-0ozt4x`, evidence
`emacs-5d1f629e9f6fcd876a973fad309feff9`, verbatim:

> s2 and s3 are good; s1 is plausible, but 🈲 (constrain) would probably be
> better, because 🈸 is typically used for informational requests, where is
> this "please" isn't really an ask but a boundary

Recorded as three assessments of `turn-iWVzvT`'s reading: s2 `accepted`,
s3 `accepted`, s1 `rejected` with a proposed successor interpretation
`constrain` (reason: the "please" sets a boundary rather than asking for an
action). Actor joe, `evidence/basis human`. The original `ask-action`
interpretation is kept, not deleted. Increment 7 therefore uses this
judgement instead of waiting for a new one.

The colon-form marks answer the agent's paragraphs, so mark and reading
differ: this is shown as several attributable interpretations, not as proof
that one is wrong. No incorrect mark is fabricated. The assessment
acceptance case uses a real operator judgement of a named interpretation;
until one exists it is labelled **pending**, not inferred.

**Increments** (one packet each; persistence separate from presentation;
assessment semantics are part of the core contract from step 1):

1. Freeze fixtures, canonical record shapes, semantic-id rules, the F1
   interpretation 'clusion **type declaration** (DERIVE-core §1), the
   `intent-vocabulary/v1` resource with its three **generated**
   presentations (DERIVE-core §12), and golden expected records (futon3c
   test resources). Targets and interpreters are typed external-reference
   entities; method/version is the inert dynamics reference; the three
   views are registered projection rules; evidence uses the core's
   basis/standing/coverage fields (DERIVE-core §11).
2. `clusion/assert` and `assessment/append` in the JVM registry and the
   futon1b conditional route; namespace, atomicity, idempotency tests.
3. Settled-turn materialiser, including ambiguous readings and assessment
   records.
4. Deterministic Markdown + sidecar exporter; ingest; resolve across
   revisions.
5. futon4a linear, graph and bounded GCC (non-executable) projections over
   identical ids.
6. Prose-edit / relocation demonstration; assert cross-view identity.
7. Operator assessment: materialise Joe's 2026-11-08 judgement of
   `turn-iWVzvT` (above) as assessments plus the `constrain` successor
   interpretation for s1.

**Acceptance** = the mission's F1 completion criteria (§1), shown in Emacs
through futon4a.
