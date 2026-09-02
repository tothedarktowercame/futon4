# Mission lifecycle × War Machine: the alignment, written down

Claude (claude-1) with Joe, 2026-09-02. Joe: "the mission lifecycle and the AIF
loop are similar… we've got a specific AIF lifecycle with several phases, so
that gives us a map from phases to nodes. What we need is how the mission
lifecycle maps to those phases… all of this is written down in terms of how the
machine operates with the circled numbers inside of the PLoP paper. It's just
not aligned to a mission lifecycle yet."

Sources: `futon4/holes/mission-lifecycle.md` (HEAD → IDENTIFY → MAP → DERIVE →
ARGUE → VERIFY → INSTANTIATE → DOCUMENT); `p4ng/empirics-futon/control-stages.edn`
(the five loop stages PERCEIVE / BELIEVE / EVALUATE / SELECT / ACT with the
assurance band beneath); `p4ng/sec-system.tex` (the ①–㉞ walkthrough);
`futon2/scripts/futon2/report/war_machine.clj:1405` (the fact below);
`futon2/holes/missions/M-zaif-harness-v1.md` IDENTIFY (the phase→node table this
document supersedes in detail).

## 0. The alignment is already half-live, at one specific line

`phase-doability` (war_machine.clj:1405-1415) reads a mission's *lifecycle
phase* and weights its selection value by it — step ⑬'s three-factor mission
value is centrality + strategic fit + **phase doability**, completion-gated and
decayed by non-progress:

    head 0.1 · identify 0.2 · map 0.3 · derive 0.5 · argue 0.6
    · verify 0.8 · instantiate 1.0 · document 0.4 · complete 0.0

So the machine already believes something about this alignment: a mission is
most actionable at INSTANTIATE, barely actionable at HEAD, and worth 0.4 of a
build when only writing-up remains. Everything below makes explicit what that
table takes for granted — that a mission's phase tells the WM what *kind* of
work selecting it would buy.

## 1. Three nested loops, one node for the nesting

R15 ("hierarchy and timescale") is the node the paper gives this structure.

| Loop | Timescale | What cycles | Where the paper writes it |
|---|---|---|---|
| Outer | across missions | selection: enumerate open missions, weight (⑬), rank, select (⑯), abstain if nothing beats no-op (⑰) | EVAL ⑪–⑱ |
| Mission | across a mission's life | the lifecycle phases themselves — the claim of §2 is that these ARE the five stages at mission grain | this document |
| Inner | one work item | a **flight**: obligation read → author dispatched → build gate → independent review → bounded amendment → typed discharge → trace | LOOP ㉓–㉞ |

## 2. The mission lifecycle IS the five-stage cycle at mission grain

| Lifecycle phase | AIF stage | Nodes | What the phase deposits, in node terms |
|---|---|---|---|
| HEAD | PERCEIVE | R2 | operator voice as the highest-precision typed observation (the declared-marks channel, authored at source) |
| IDENTIFY | PERCEIVE→BELIEVE | R8, R1 | **the gap is a present-fit mismatch**: the lifecycle's own words — "what discrepancy between ideal and actual prompted this?" — are R8's definition at mission grain. The gap statement becomes the mission's belief-state anchor (R1); the completion criteria become the mission's C-vector — the targets the risk term will read for every flight flown under it |
| MAP | BELIEVE | R2→R3, R7 | survey as observation-gathering folded into belief ("facts, not decisions" = R3's discipline); the ready-vs-missing table types each source's trustworthiness (R7) and its **missing column is the seed of the candidate action space** |
| DERIVE | EVALUATE | R4, R5 | candidate designs as forward models; IF/HOWEVER/THEN/BECAUSE is a G-comparison over designs with risk and ambiguity named |
| ARGUE | EVALUATE→SELECT | R6 | pattern cross-references are recall warrants entering the posterior — the library speaks for or against the design before commitment |
| VERIFY | assurance band | R9, R20 | independent witness before action: the BoM and structural checks are external adjudication of the design (R9's no-self-certification, applied to plans), tripwires declared (R20) |
| INSTANTIATE | ACT | R16 — and the inner loop runs here | the phase where flights fly: each work item goes through ㉓–㉞ (author ≠ reviewer, bounded amendments, typed discharge). Doability 1.0 is the machine agreeing |
| DOCUMENT | assurance band | R17, TRACE | the learning deposit: what later missions inherit (structure learning), and the durable record |

Reading the doability table against this: the machine pays most for phases
whose stage is ACT, least for phases whose stage is PERCEIVE — selection buys
actuation, not perception. Whether that is *right* (should a WM starved of
observations not sometimes buy MAP work?) is a real question the table
currently answers by fiat; noted, not resolved.

## 3. Joe's question: the entry point — what happens right after selection

Suppose the outer loop selects M-zaif-harness-v1 (the zaif lab's S3 will make
this happen through declared inputs). "The War Machine would have to look at
the IDENTIFY step and say: there is an identified gap. How do we formally wrap
our heads around what this gap actually is?"

The formal answer this alignment gives: **ingest the mission document as typed
observations, phase by phase, into mission-grain state**:

1. **Parse** the mission file's lifecycle sections (they are typed by heading —
   the convention is machine-legible already).
2. **The gap → R8/R1**: IDENTIFY's discrepancy statement is minted as the
   mission's present-fit mismatch; the belief state opens with it. This is the
   "formal wrap": a gap is not prose, it is the mismatch term the mission
   exists to reduce, and progress = its reduction, measurable per flight.
3. **Completion criteria → C**: each testable condition becomes a preference
   target; the risk term of every flight flown under this mission reads them
   (app-zaif commitment 1: G is parameterized by the clocked mission).
4. **MAP's missing column → R6**: the mission's worklist rows are the
   candidate action space at mission grain. (For M-zaif-harness-v1 this is
   literal: `holes/labs/zaif-harness/worklist.edn` rows S1–S4, U7–U9.)
5. **Phase → ⑬**: the current phase sets doability; phase transitions are the
   mission-grain actions the machine can recognise as progress.
6. **Flights fly** (inner loop ㉓–㉞) against the row the tick selects;
   discharges move the gap term; DOCUMENT deposits what R17 keeps.

Steps 1–2 and the ingest do not exist in code today. Step ⑬ exists (phase
read, doability applied). Steps 3–4 exist per-flight only via hand-carried
context. That delta — mission-document ingest into mission-grain state — is
the concrete build the zaif lab's S4 row scopes.

## 3b. What the wiring says happens next — checked (2026-09-02, Joe's question)

Joe: "assume IDENTIFY has happened and is taken as given… that doesn't
presuppose MAP has been done. Is there an AIF analog of MAP that might kick in
next, and is that what the wiring says would happen?"

Theory says: MAP's analog is epistemic action — G's ambiguity/information-gain
side selecting observation before commitment. The wiring says something else,
checked step by step:

1. After an :advance-mission selection, `construct-for-decision`
   (full_loop_runner.clj:1036) dispatches on action type; mission actions fall
   to the :default method (:998-1035), which calls
   `futon2.report.cascade-lane/cascade-lane` (cascade_lane.clj:434) — a
   cascade built for the mission's psi by the PYTHON SIMILARITY CONSTRUCTOR
   (cascade_serve.py / MiniLM cosine, greedy, budget 6): exactly the
   constructor O2/F3 refuse and LA1c §4 recorded as the wrong substrate. If it
   returns nothing, the run throws "No construction for selected decision"
   (full_loop_runner.clj:2628) — the 09-01 log's exact stop.
2. Then the flight (㉓–㉞): an author dispatched with that cascade as bounded
   build context, undifferentiated by the mission's phase.
3. **There is no MAP analog anywhere in this.** The action space is
   {:open-mission :advance-mission :fire-pattern :learn-action-class
   :repair-machine-failure} — every one pragmatic. Nothing reads the mission's
   MAP section; nothing generates a survey; the tick's READ phase (①–⑩)
   perceives the machine's own registries, never the selected mission's field.
   An identify-phase mission and an instantiate-phase mission get the same
   treatment: generic advance, similarity cascade, author flight.

**The unifying hypothesis this exposes** (ties three of today's threads into
one hole): ambiguity moved 0 winners in 674 ticks (the U4 question) not
because the term is broken but because **the action space contains no
epistemic actions for it to favor** — G = risk + ambiguity can only
discriminate among candidates, and every candidate is pragmatic. Joe's
explore/exploit ruling (§5 q1), his actionability question (§5 q-new), and
MAP-has-no-analog are the same absence seen from three sides.

**The concrete fix this implies** (proposed as zaif-harness row S6, pending
Joe): a `:survey-mission` action type — the MAP flight. The lifecycle already
requires each mission's MAP section to enumerate questions Q1–Qn; those are
machine-legible survey obligations sitting in every conforming mission doc.
The action's G is dominated by information gain (finally giving the ambiguity
term candidates it can favor); its discharge is MAP answers as typed
observations folded into mission-grain belief; its natural selection weight
rises exactly when S5's epistemic term says the field is the bottleneck. An
identify-phase mission then generates survey flights the machine can actually
fly autonomously — Joe's actionability concern answered by construction.

## 3c. The cascade catalog — missions read by their cascades, not their prose (Joe, 2026-09-02)

> "Each mission might leave behind a cascade showing how it was worked
> end-to-end, and so maybe we could learn from that catalog… at the MAP phase,
> as we're looking around at other missions, we could look at how those
> missions showed up in terms of their cascades and their build strategies,
> rather than just looking at them as text files."

The design, thought through against what exists:

1. **The record accretes; nobody writes it after the fact.** A mission's
   cascade record is assembled from typed events that already exist or are
   already required: PSR/PUR records (the lifecycle mandates per-mission
   Pattern Selection Records — which pattern, where, how), flight discharges
   (㉚, typed implementation/discharge events), the trace's decision + :shown
   pattern lists per tick, and clocked-on lineage (who was on it, when). The
   DOCUMENT phase — R17 in the §2 table — is where the accreted record is
   sealed as the mission's deposit. The alignment earns its keep here: DOCUMENT
   was already "the learning deposit"; this says concretely WHAT is deposited.
2. **MAP reads the catalog, not the prose.** The survey flight (S6) queries
   kin missions (kinship: shared patterns, shared repos, cross-refs) for their
   cascade records — which patterns, in what order, with what outcomes. The
   ChipWits reading: you learn to wire a new robot by opening saved robots
   from the Warehouse, not by reading their marketing copy.
3. **This is where Q(o|π) at mission grain comes from.** P-validated-R5
   demands Q(o|π) be DERIVED (a playout), never authored. For a mission-grain
   policy there is no simulator — but the catalog IS the playout record:
   the empirical distribution of how cascades-like-this-one actually went.
   G over candidate build strategies gets its forward model from precedent,
   which is also exactly what makes ambiguity discriminate at mission grain
   (a strategy with no precedent is high-ambiguity; surveying reduces it).
4. **The precedent is already running.** Every APM frame leaves an end-to-end
   record (ledger, trace, memories); the zai-scribe mines failed and
   successful attempts into typed memories; the fingerprint audit adjudicates
   what transferred by token witness. The mission cascade catalog is that
   architecture at mission grain, and it should inherit the discipline whole:
   typed use records, witnessed transfer, author≠reviewer on anything
   promoted from a record into the library.
5. **O5 holds.** Catalog reading yields proposals and weights; only authors
   write edges. A build strategy learned from the catalog reaches the library
   as a proposed pattern with provenance, not as an inferred edge.

Carriers today, honestly: PSRs exist in the lifecycle but sparsely in
practice; :shown lists exist per tick but aggregate nowhere; APM records are
rich but domain-local; futon6 pattern-phylogeny-learned.json (LA9''s carrier)
holds 2 co-application edges. The catalog is a query surface plus an
accretion convention away — not a new store.

## 4. What changes in the new construction (post-§1c)

The inner loop as written selects ranked single actions (⑯). Under the
2026-09-02 §1c ruling (pattern ≡ policy at every grain; policy = cascade at
the policy grain), the flight's plan becomes a **constructed cascade**: the
constructor (library rows L6/LA2/LA3) builds it at the policy grain, G is
evaluated at each attachment step, and the flight fires the cascade rather
than executing one ranked action. ⑳–㉒ already gesture at this ("constructs
once, for the selected target"; cascade as bounded build context; enactment
gated separately) — the new construction promotes that from advisory to the
flight's actual shape, with U6-style per-node assertions as flight
instrumentation. Nothing in §2's table changes; what changes is the *type of
the thing selected* at R6 — which is why every U-row keeps candidates opaque
at that seam.

## 5. Standing questions (named, not resolved here)

- Should doability ever prefer PERCEIVE-stage phases (buy MAP when the machine
  is observation-starved)? — **ANSWERED YES (Joe, 2026-09-02, in session,
  dictated)**: "yes, we do need to prefer perceive in some cases… with the MAP
  phase in a mission, we need to be able to do that, and that's similar to the
  explore–exploit dichotomy that active inference is supposed to be quite good
  at… even if you think about a ChipWit exploring a maze — it has to sometimes
  go explore in order to decide what to do next." Design consequence, stated
  here and worked as zaif-harness row S5: the doability table prices only
  exploit (pragmatic value rising toward INSTANTIATE). The AIF-native fix is
  not a bigger fiat number for MAP — it is an *epistemic term in the mission
  value*, so a MAP-phase mission scores by expected information gain about the
  field and wins exactly when uncertainty about the field is what blocks
  everything else. The hand-authored table then becomes what it should have
  been: a prior, not the whole value. (In the iconography: SNIFF and LOOK are
  chips too, and the maze-explorer spends cycles on them precisely when its
  map is the bottleneck.)
- **Autonomous actionability as a selection factor** (Joe, 2026-09-02, on the
  S3 result): "I have to question whether selecting based on centrality,
  without some kind of measure of what can be done about this particular
  mission on an autonomous basis, isn't setting the machine up for success.
  Had it selected expressions-of-interest, I'm not sure what it could have
  actually done." Phase doability gestures at this but measures lifecycle
  position, not machine-workability. A real actionability factor would read
  something like: does the mission have an open, machine-legible worklist
  with unblocked rows the WM's flights can actually fly? (M-zaif-harness-v1
  scores high on exactly that; the incumbent may not.) Kin to S5's epistemic
  term — both say the three-factor value under-measures what selection is
  FOR. The stated test is an end-to-end run, deliberately deferred.
- Is the §2 mapping a formal claim or a working analogy? — carried tension 3
  of M-zaif-harness-v1, still carried; S2's manual step-through is where it
  gets its first test against reality.
- DOCUMENT at 0.4 outranks MAP at 0.3: the machine mildly prefers writing up
  old work to surveying new ground. Probably right, worth a word from Joe
  someday.

Cross-refs: exercised by `futon2/holes/labs/zaif-harness/worklist.edn` (S2
manual step-through, S4 selection→clocking); the phase→node fixture idea in
`M-zaif-harness-v1.md` IDENTIFY; `p4ng/sec-system.tex` for the ①–㉞ walk.
