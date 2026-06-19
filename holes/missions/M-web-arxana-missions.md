# Mission: M-web-arxana-missions — Show a Mission's Semantic Network in WebArxana

**Date:** 2026-06-07
**Status:** IDENTIFY + MAP complete 2026-06-07. **Layer 1 LIVE** — `/ego`
hyperedge-projection shipped (codex-1 `c01d46c`: typed neighbours, no self-loop)
+ `#/focus/<id>` route exists; client `fetch-hyperedges` dedup = small cleanup left.
**Layer 3 DERIVE drafted** 2026-06-07 (§6: reuse Stage-5 NLP on the mission corpus):
Pass 1 base-rate prior **built** (`build_mission_prior.py`, 267 missions); Pass 2
(mission-domain NER kernel) **→ codex-2**.
**Xenotype:** derivation (IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE)
**Repos:** futon4 (WebArxana client/server), futon3c (mission-doc ingest watcher),
futon1a (hypergraph store). Reads the typed decomposition from futon0
(`M-capability-star-map.graph.edn`).
**Owner:** Joe frames; agent TBD (architecture/framing stays with the Claude owner).
**Related:** [[project_webarxana_as_monitor]], [[reference_arxana_store_relation_footguns]],
[[project_interest_network]]; sibling missions `M-webarxana.md`,
`M-web-arxana-ui-improvements.md`, `M-capability-star-map` (futon0),
`M-stack-stereolithography` (owns the posterior render).

## HEAD (Joe, 2026-06-07, verbatim sense)

> "Let's work on Layer 1. My sense is that Layer 3 is the most interesting, but
> we could work towards a viewer that would *show* us the interesting stuff even
> in one example. My sense is that we need an `M-web-arxana-missions.md` mission
> with the above contents in it."

Reading: build the viewer bottom-up. Land Layer 1 (a Mission's relational
network, rendered live in WebArxana) because the data and the renderer already
exist; but aim the whole effort at Layer 3 — *seeing the interesting semantic
structure of a Mission* — and accept a single compelling example as the first
real proof, rather than waiting for full coverage.

## Telos: three layers (the assessment that motivates this mission)

"The semantic network associated to a Mission" resolves into three concentric
layers, at very different distances. This mission walks them L1 → L3.

- **Layer 1 — the relational network around a mission** (this mission ↔ other
  missions via cross-refs; this mission ↔ the code paths it touches). The data
  is already live in the store; the renderer already exists. *Nearest.*
- **Layer 2 — the typed capability decomposition** (the star-map: capabilities a
  mission `:produces`/`:requires`, `:specialises`, `:couples`). Built as data for
  one region on disk, **not in the store, not rendered.** Gated on the
  decomposition maturing. *Medium.*
- **Layer 3 — the semantic network of concepts *inside* a mission** (the ideas a
  mission is about and how they relate). **Does not exist** as data. The
  capability-star-map is the *principled substitute* — capabilities are the
  chosen typed vocabulary instead of open-ended NLP concepts. *Farthest, most
  interesting.*

The dependency Joe flagged holds precisely: L2/L3 depend on the structural
decomposition of Missions; **L1 depends on none of it** and is nearly free.

## 1. IDENTIFY

### The tension (why this mission exists)

We now have (a) a best-yet *typed* decomposition of Missions emerging from
[[M-capability-star-map]], and (b) a large, live mission graph already sitting in
the Arxana store — yet there is **no way to look at a single Mission and see its
semantic network** in WebArxana. The rendering engine is generic and already
live; the data is present; but a mission's neighbourhood does not show up,
because of a specific, identified wiring gap (see MAP). So the stack can *reason*
over mission structure (the WM does EFE over the star-map) but the operator
cannot yet *see* it. This mission closes the see-it gap, starting from the layer
that is already almost wired.

Recursion worth noting: this very mission doc will be ingested by the watcher as
a `mission-doc` node, so it becomes a vertex in the graph it is trying to render.

## 2. MAP — what exists, verified live 2026-06-07

All counts below were probed live through the path the UI actually uses
(WebArxana proxy on :3100 → futon1a store).

**Live mission graph data (already in the store):**
- `code/v05/mission-doc` entities: **219**
- `code/v05/mission-cross-ref` edges (mission ↔ mission): **717**
- `code/v05/file→mission` edges (mission ↔ code): **1608**
- The `capability-star-map` mission resolves as an entity
  (`futon0-d/mission/capability-star-map`, `type mission/doc`,
  `source mission-doc-watcher`) with **25 edges in its neighbourhood**
  (13 cross-ref, 11 file, 1 doc).
- Ingest pipeline: futon3c `watcher/file_ingest.clj` `ingest-mission-doc!` +
  `peripheral/mission_control_backend.clj` `parse-mission-md`; watcher-driven, so
  freshness tracks the running watcher.

**The renderer is live and generic:**
- WebArxana serves (:3100, 200). d3-force renderer in
  `futon4/dev/web/webarxana/src/webarxana/client/graph.cljs`; per-relation-type
  styling already supported (dashes/colour by type). A `mission-search` force
  graph and an `interest-network` view already render.

**THE CRUX GAP — hyperedges do not render via `/ego`** (cf.
[[reference_arxana_store_relation_footguns]]):
- The mission edges are stored as **hyperedges** (`mission-cross-ref`,
  `file→mission`), but the `/ego` endpoint
  (`futon1a/.../compat/futon1_graph.clj`) queries **only binary `:relation/*`**.
- Verified: `GET /api/futon/ego/futon0-d/mission/capability-star-map` returns
  **0 outgoing / 0 incoming**, even though 25 hyperedges touch the node.
- The client has a *separate* `fetch-hyperedges` path
  (`client/api.cljs`) that does pull them — and **flattens them to binary edges**
  for rendering (the n-ary structure is lost).
- So the data is reachable, but a mission's neighbourhood does not appear in the
  main ego/graph view as-is, and faithful n-ary rendering is not yet possible.

**The typed decomposition is NOT in the store:**
- `M-capability-star-map.graph.edn` (futon0): 8 capability nodes, 14 missions,
  **24 typed edges** (`:produces`/`:requires`/`:specialises`/`:couples`),
  WM-region slice only, verified. Lives **only on disk** — the store holds zero
  `capability` entities and zero `:produces`/`:requires` edges. (Layer 2 input.)

## 3. Layer 1 — the active target

Goal: **focus a Mission in WebArxana and see its live relational network**
(related missions + code paths) as a graph.

Provisional approach (DERIVE will choose among these — open question):
1. **Spike first (next car):** pin `futon0-d/mission/capability-star-map` in the
   live graph view and record exactly what renders today vs. what is dropped —
   turn "close" into a precise punch-list. Cheap, high-information.
2. **Close the ego/hyperedge gap** so a mission's neighbourhood populates. Either
   (a) have the mission/ego view route through the existing `fetch-hyperedges`
   path, or (b) teach the ego endpoint to include hyperedge-projected neighbours.
   Binary projection is acceptable for L1 (mission↔mission and mission↔file are
   naturally binary).
3. **Entry point / route:** a way to land on a mission's graph (e.g. a
   `#/mission/<id>` route, or a click-through from the existing mission-search
   surface into the focused graph).

Acceptance bar (provisional): from a running stack, an operator can open a named
Mission and see its related missions and code paths as a connected, navigable
graph — no Emacs, no EDN files.

## 4. Layer 3 — the telos ("show the interesting stuff in one example")

Even before a decomposition pipeline exists, hand-author the concept/relation
set for **one** Mission (or lift it from the capability-star-map typed structure)
and render it, so we can *see* whether the semantic view is worth building at
scale. The capability `scope` relation is naturally **n-ary** (a mission requires
a *set* of capabilities), so Layer 3 (and faithful Layer 2) will press on the
hyperedge-rendering limitation that Layer 1 lets us defer. The single-example
viewer is the experiment that tells us what the full decomposition must produce.

## 5. Open questions (for DERIVE)

- Where does the mission render belong — WebArxana, or M-stack-stereolithography
  (which the star-map doc names as the render owner)? Coordinate, don't duplicate.
- Fix the ego endpoint vs. route the view through `fetch-hyperedges`? (Layer 1
  can use either; Layer 2/3 likely need real n-ary rendering.)
- For Layer 2/3: ingest `M-capability-star-map.graph.edn` into futon1a (mind the
  upsert-REPLACES-props footgun, [[feedback_futon1a_upsert_replaces]]) vs. read
  the EDN directly into a view.

## 6. DERIVE — Layer 3 via Stage-5 NLP on the mission corpus (Joe + claude-3, 2026-06-07)

**Spine (Joe's move):** don't hand-author L3 and don't settle for the
capability-substitute — **reuse the superpod Stage-5 NLP hypergraph system**
([M-prior-mathematics], futon6 `superpod-job.py` / `clean-ner-kernel.py` /
`topic_prior.py`) on the **mission corpus**. Missions are an *easy* "Article type":
the standard eightfold lifecycle = **Octopus.ac-style typed elements smooshed into
one doc** instead of distributed across separate publications.

**Why the Octopus framing is load-bearing (the prize):** the eightfold types the
*nodes* (which phase a concept lives in) AND — because Octopus's types are a
canonical **DAG** (problem→rationale→method→results→…) — it hands us the **relation
roles for free**: a concept in IDENTIFY relates to one in DERIVE as *problem→method*,
not mere co-occurrence. That directed, lifecycle-typed structure *is* the "how the
ideas relate" that L3 needs and flat NER cannot give. And Stage-5 outputs **n-ary
hyperedges** straight into Arxana → it renders through the **same WebArxana
hyperedge path L1 just fixed** (`/ego` projection, codex-1 `c01d46c`) + the deferred
faithful-n-ary extension. L1 and L3 share one substrate.

**Two lexicons, one base-rate (Joe):** every concept gets a **self-representing**
view (native in-stack jargon = our NEs, faithful) and a **projection-into-English**
view (general terms, legible to an outsider) — the audience duality, both derived
off the same prior. (Cf. the eoi-next plain-language discipline.)

**Pass 1 — the base-rate prior. DONE 2026-06-07.** `futon6/scripts/build_mission_prior.py`
(a ~70-line markdown analog of `build_ct_prior.py`): document-frequency
`P(term | mission corpus)` over **267 `M-*.md`** under `*/holes/` → `futon6/data/mission-term-prior.json`
`{n_docs:267, unigram_df (13.9k), bigram_df≥3 (57k)}`. Findings that *scope the
rest*:
- The **self-representing lexicon falls out** of "high-df, not in `/usr/share/dict/words`":
  `invariants instantiate lifecycle aif arxana flexiarg exotype hypergraph psr pur
  handoff xtdb repl hyperedges codebase`.
- The cheap **dict-knockout has two failure modes** (the empirical case for a kernel):
  it **over-knocks English-word NEs** — `futon · pattern · evidence · scope · agent ·
  mission` are dictionary words but *our concepts* ("futon" = furniture, in the dict);
  and **under-knocks generic-tech** — `clj edn git jvm cli json config` (generic
  software, not FUTON-distinctive) + tokenizer crumbs (`doesn isn didn`).
- The **`P` column is SIP for free**: `status` 0.97 / `mission` 0.96 (in ~every
  mission = stack-boilerplate) vs `exotype` 0.22 / `hypergraph` 0.21 (distinctive).
  A *>X%-df knockout* drops stack-generic terms without a dictionary — "statistically
  improbable phrase" = concentrated, not ubiquitous.

**Pass 2 — the mission-domain NER kernel. → codex-2.** The curated arbiter the
frequency-prior can't be: **reclaim** English-word NEs (seed from the typed
registries — capability names, sorry/proof-state ids, pattern/flexiarg names, the
eightfold phase-names), **drop** generic-tech + the >X%-df boilerplate, **fix** the
tokenizer junk; emit `mission-ner-kernel.json` + the cleaned self-representing
lexicon and the projection-into-English knockout. (Per-domain NER specialisation
was always planned; missions are just another domain.)

**Pass 3+ (after):** per-eightfold-**phase** prior (the [M-prior-mathematics] §2b
per-category analog — phase-relative normalcy, global mission prior as the
partial-pooling backstop) → section-typed n-ary extraction → Arxana → WebArxana
render. **Validate** on **one** mission (M-capability-star-map, already the L1
example) cross-checked against `M-capability-star-map.graph.edn` (the typed
capabilities = the gold the NLP concepts should align with).

**Resolves the §5 open questions:** render lives in WebArxana (coordinate the n-ary
extension with M-stack-stereolithography, don't duplicate); the L1 ego/hyperedge
fix is **done** (codex-1 `c01d46c`); for L2/3 the star-map `graph.edn` is the gold
**cross-check**, read directly (no ingest needed for the comparison).

## 6.1 Scope-as-operator: the anatomy is a nested scope tree (Joe + claude-3, 2026-06-08)

**The reframe (Joe):** a *scope* is not a "text span over which NER runs" (that was
backwards — the span is the binder's shadow). A scope is a **logical operator** — in
LISP terms a **named function**, in GOFAI terms a **frame + slots**. "Let X be a
group" *is* a scope; X and `group` are the entities bound *inside* it.

**It is already built (futon6):** `detect_scopes` (`scripts/nlab-wiring.py:1174`)
emits each scope as an **n-ary hyperedge with typed role-slots** (`entity · symbol ·
type · value · condition · quantifier · object`) keyed by a **binder-type**
(`let-binding`, `define`, `exists-binding`, `assume`, `if-condition`, `consider`,
`environment`…), **nested** via `parent_env_id`. That *is* frame+slots, as a
hyperedge. (And `scope` is a node-type in futon6's hypergraph — M-differentiable-code
§scope-grain.)

**This UNIFIES the terminological collision** rather than tolerating it: math (`Let X
be a group`), code (`(defn …)` / `(let …)`), a mission's `:scope-in/out`, and MAP are
**one construct at different grains** — a binder that names a context and holds
entities. Three labels kept distinct in the model so nothing conflates: **mission-scope**
(work boundary) · **ner-span / extraction-scope** (the Stage-5 detection region — the
binder's shadow) · **capability-scope** (the set a capability spans).

**So L3 is a nested scope tree, not a flat concept graph:**
- the **eightfold phases are the mission's top-level scopes** (its "environments");
- **MAP is a scope-of-scopes** — the higher-order binder `(map (λ thing. shows? gap) related)`, parameterised by the IDENTIFY gap;
- **`:scope-in/out` is the frame boundary** (which names are admitted);
- **NER concepts are the bound slot-fillers at the leaves** — not free-floating nodes;
- **n-ary falls out** — a frame's slots / a function's args *are* the hyperedge; no flattening.

The build is therefore: **recover the scope tree, bind concepts into its slots.** A
**mission scope-detector** = the `detect_scopes` analog with *mission* binder-types
(eightfold-phase, scope-in/out, source-material, relationship-to-missions,
capability-scope) emitting n-ary scope-hyperedges; the Stage-5 `ner-span` fills the
slots; the tree renders through the L1 WebArxana hyperedge path.

**Ensemble finding (measured 2026-06-08 — compute-and-see / ehipassiko):** the anatomy
**matured over eras** (`futon6/scripts/mission_anatomy_profile.py`). The *Agency*
family (Feb 2026) is **loose — no eightfold** (`Scope/Motivation/Parts`), concept-dense
(142/1k), sparsely linked (2.3 mission-refs). The *War Machine* family (Apr–May) is
**full eightfold**, process-heavy, **4× more cross-linked** (9.3 refs). The lifecycle
scaffold post-dates Agency; the Daumal door itself became more articulated. **`:scope-in/out`
is the era-invariant rib** (present in every era) — hence the right primitive to build on.
(The material-density proxy is contaminated by the word "agency"; discount it.)

**Scope-tree confirmation (clean, post-review 2026-06-08).** The scope-detector
(`mission_scope_detect.py`, codex-2) makes the topology *structural*, and a review
de-contamination sharpens it (family means): **eightfold-phase 0.0 → 5.3** (the
cleanest signal — early futons have *no* lifecycle scaffold), **relates-to 1.0 → 5.3**
and **map-item 7.7 → 13.3** (late futons lean on the mission-web + surveys),
**capability-scope (distinct, self-excluded) 0.3 → 2.3** — real but *modest*: the raw
`37` for war-machine-tuning was 37× "war-machine" self-reference, caught in review, so
the render must collapse capability-scope repeats and the metric must count distinct.
Net: the anatomy shifted from *loose / concept-terse / sparse* to *eightfold /
survey-rich / densely-linked*, strongest on **structure** and **interconnection**.

### Checkpoint — 2026-06-08 (the FOLD: curation as a repeatable projection; first local BSL instance)

**What was done.** codex-1 ingested the 6 scope-trees into futon1a (`futon3c …
mission_scope_ingest.clj`, `1dc69ea`); `/ego` on `futon3c-d/mission/war-machine` now
returns the full scope-anatomy (381 neighbours: scopes, 62 nesting edges, 200 concepts,
17 *deduped* capabilities). 381 is a hairball, so Joe reframed **curation as a repeatable
projection operator, not a per-mission filter** (his *"design the projection apparatus,
not the list"*). Built `futon6/scripts/mission_fold.py` — the **FOLD**: group neighbours
by `hx/parent` nesting, collapse concept-slots into per-frame counts + a **SIP-ranked**
sample, depth/expand params. Result: **war-machine 266 → 6 frames (98%)**, every concept
≤2 expands away; all six fold to 6–15 frame spines.

**The BSL framing (Joe).** The fold **is the ideal first example of Bayesian structure
learning** (`M-bayesian-structure-learning` §3.4 model-reduction): the simpler model (the
spine) that explains the neighbourhood, with the **SIP prior** as salience (§3.3) and
**expand = expected info-gain**. Honest scope: this is the *front half* (prior salience,
structural reduction). The **dual loop closes** when the **operator's expand/collapse
behaviour becomes the evidence** that updates the salience posterior — local,
operator-in-the-loop, no superpod. That is this mission's role in
**`C-bayesian-structure-learning-family`** (futon0): the **mission-corpus** local instance
(claude-3), run in parallel with claude-1's **WM** instance (`E-efe-education`); the chord
= the same method transferring.

**Test state.** `mission_fold.py` runs clean on all 6 trees; reduction figures reproduce.
Known refinement: the info-gain frontier currently surfaces the trivial root — should rank
below-root frames.

**Dual loop — mechanism-proven (`mission_fold_learn.py`).** The completion-criterion's
second half ("improves its own model") is demonstrated: each operator expand/collapse is a
Bernoulli trial on "want this frame open?", updating a per-frame `Beta(α,β)` seeded from the
SIP prior. On a synthetic trace (operator expands MAP, collapses IDENTIFY), **IDENTIFY flips
EXPAND→fold (0.75→0.43) from interaction alone** — the fold adapts to the operator. Synthetic
events stand in until the live render emits real ones. So the **full Friston dual loop is
shown locally** (structure-learning + self-improvement) — my instance's Campaign deliverable.

**R1 feedback edge — the "one loop" experiment (with claude-1).** The chord is confirmed
(C-family §6): my fold = REDUCTION, the WM = EXPANSION (G-info), bound by precision-learning —
*one Friston loop distributed across two domains*. The load-bearing test: the fold's reduced view
feeds **back** into WM selection. Built my side — `mission-fold-view.edn` (emitter:
`mission_fold_learn.py --emit-view`): one artifact carrying **both poles** — high-`:salience` spine
(my operator view) + per-mission **`:gap-score`** (the WM's cross-mission expansion signal = mean
emptiness of announced-but-unfilled section-frames, intrinsic [0,1], scales to 200) + the low-salience
spine tail (within-mission gap localizer). Global gap-ranking: war-machine-tuning 0.49 → agency-rebuild
0.11. **R4-candidate:** stub frames ≈ the WM's advanceability NAGs (the fold *pre-answers* "articulate
the next hole"). claude-1 builds the WM-side reader on a branch (additive into the EFE G-info term);
**live activation is Joe's consent locus** (the EFE is the nervous system) — wired now, activated when he's back.

**Next.** (1) Live folded render → codex-1 (`?fold=1&depth=N` server-side + cljs
collapse/expand, `mission_fold.py` as spec) — *in flight*; it emits the real expand/collapse
events that replace `mission_fold_learn.py`'s synthetic trace. **Intake built + verified**
(event contract `data/mission-fold-events.jsonl` `{mission,frame,action}` → `load_events` →
`--from-events`): events drive **salience** (behavioural), `:gap-score` stays **structural** —
the render just needs to append a line per expand/collapse (2-line add, fold in at render review). (2) Scale via the resumable
mining loop over ~200 missions (detect + ingest both exist; needs the manifest wrapper —
operator-kicked per Joe). (3) Live "watch a mission unfold" (mission-watcher → scope-detect hook).

### Checkpoint — 2026-06-08 (STANDARD-VERIFY: the chord earned its standard instead of assuming it)

Before activating R1, I scaled gap-score to the **full 194-mission corpus** (pure detect+fold, no store) to
harden it — and it **refuted** claude-1's claim "the chord IS the realignment." The original gap (mean
stub-FRACTION) was **size-dominated**: tiny stubs scored ~1.0 (M-sliding-blackboard 0.96 @raw6),
war-machine-tuning was only rank 93/194, and — decisively — **M-canon-fingerprint-store (the math mission to
steer away from) tied it** (0.458 vs 0.491). Gap could not discriminate math-from-local; the realignment was
the **body-term's** work and gap rode along. claude-1 confirmed **2 of the 3 top mis-steer stubs are OPEN**
(coupling-as-constraint Ready, tpg-coupling-evolution MAP) → gap-as-was **would have been a live regression**;
activation was gated.

**Fix (mine, shipped):** gap redefined as **GROWTH-SURFACE** — the count of announced-but-empty section-frames,
saturated (cap 10), size-floor-gated (raw≥12). The 3 stubs now score **0.0**; war-machine-tuning leads
*within-local* (0.9 > star-map 0.4 > pilot 0.3). **Corrected architecture:** `ascent = domain-selector
(local vs math)` · `gap = within-local expansion-refiner` · `body = boundedness` — each term its own job. The
**chord-as-structure survives** (reduction↔expansion coupling, operator-evidence loop); **gap-as-realignment-
signal did not**. claude-1 adds the ascent-gate (b) + a body-only/body+gap control (c) to attribute correctly;
activation lifts when Joe's back. Contract tests **8/8** (incl. a regression pinning the 3 open stubs at 0).
This is the Campaign's STANDARD-VERIFY working as designed — the cross-check caught a too-clean chord before a
bad activation.

**(b) domain-gate — candidate emitted (2026-06-08).** Growth-surface fixed *size* but not *domain*:
canon-fingerprint-store (math) still scores 0.8 (its §9 holes are real empty sections) ≈ war-machine-tuning
(local) 0.9. claude-1's first ascent-gate spec was too narrow (would zero war-machine-tuning, a non-graph
producer). Fix: a `:domain` tag from the corpus gates gap to local-capability missions. First-pass candidate
shipped — `futon6/scripts/mission_domain_classify.py` → `data/mission-domain-candidates.edn` (194 missions,
`:domain ∈ {:local-capability :math :other}` + confidence + per-mission seed evidence). Key discriminations
correct: **canon-fingerprint `:math` 1.00 (gap→0), war-machine-tuning `:local` 1.00 (gap kept)**. Kept
*separate* from the auto-regenerated view (the partition is curated/ratified, not regenerated). **The
local-vs-math partition is Joe's to ratify** (27 ambiguous flagged; edge cases like M-differentiable-math /
M-categorical-code / this mission are genuinely both-ish) — it's E-efe-education's core question, not settled
autonomously. Three clean signals now: **domain (my tag) gates · gap refines within-local · ascent (graph)
within-graph credit.** Gap-reader stays GATED until Joe ratifies + claude-1 wires the domain-gate + runs (c).

**Ratification worksheet shipped** (`mission_domain_classify.py --worksheet` →
`data/mission-domain-ratification-worksheet.md`): a bounded operator decision surface — **28 calls**
(3 named edge cases + 25 ambiguous, conf<0.34), one line each (candidate + evidence + RULING),
166 pre-accepted. Flow: Joe rules → I compile `mission-domain-ratified.edn` (the ratification act) →
claude-1's gate reads *that* file (absent→gap off, gated-by-construction) → activate → run (c).
**R1 readiness chain — my side COMPLETE:** (a) de-biased gap ✓ · (b) domain candidate + worksheet ✓ ·
[domain-gate = claude-1/codex-2 · ratification = Joe · (c) control = claude-1 at activation]. Nothing of
mine touches the EFE or the store; everything gated to Joe's ruling + flip.

**Joe ratified with a sharper principle (2026-06-08).** Not topic-keywords but **compute**: `:math = needs a
superpod` (scale/GPU); `:local-capability = runs on the laptop`; "most tasks outside futon6 are local." This
*inverted* the classifier — the keyword tagger had 23 topic-math; the superpod test cut it to **16 :math
(mostly futon6), 176 :local, 2 held**. Re-classified all 194 from a superpod-signal scan (filtering incidental
"superpod" mentions from genuine scale-need) + Joe's explicit rulings, and **compiled
`futon6/data/mission-domain-ratified.edn`** (the locked-contract shape claude-1's gate reads). 4 topic-math
missions flagged compute-uncertain (prior-mathematics, canon-fingerprint-store, hyperreal-dictionary-planning,
futonzero-prelim-practice — likely laptop under the strict principle) for Joe+claude-1 sign-off. **Not
activated** — creating the file is the ratification act; flipping the gap-reader is Joe's separate step after
sign-off. This is the principle Joe will dig into with claude-1 (it's E-efe-education's core question).
