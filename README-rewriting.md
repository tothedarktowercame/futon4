# Arxana Rewriting

A companion to `README-essays.md`. Essays describes how Arxana **generates**
an annotated edition (via `eoi-new`, interview, assembly). Rewriting describes
what happens **after** an essay exists when you want to revise it through
typed, verified rewrites that accumulate rather than churn.

The short version: the essay lives as prose; a hypergraph annotation layer
sits over the prose; diagnostic surveys produce typed annotations; each
annotation gets closed by a typed transformation that exhibits its operations,
its glue-adjustments, and the change it makes to the AIF² invariant audit.
The transformation is a *section of a sheaf* over the essay's typed open
cover of entities — local edits, compatibility on overlaps, gluing to a
global section. Patterns from `~/code/futon3/library/` own the moves;
closures record the trace.

## Architecture

```
prose (UKRN_WP_draft_v11.md, etc.)
   ↓
annotation hypergraph (annotations-v12.edn)
   ↓ (typed rewrites: f:(A,B)→C with verification)
revised prose + closed annotations + invariant-delta
   ↓
3-up review surface (arxana-browser-rewrites.el)
```

Three artefacts move together:

- **The prose** is canonical. Markdown, version-controlled, the thing the
  reader eventually sees.
- **The annotation hypergraph** lives in a sibling `.edn` file. Maps,
  vectors, keywords. Each annotation has an `:id`, `:hx-type`,
  `:endpoints` (one annotated span + one source pattern), a `:note`
  with the diagnostic, and (once closed) a `:closure` block with the
  full rewrite trace.
- **The pattern library** lives at `~/code/futon3/library/{system-coherence,
  writing-coherence,...}/`. Each `.flexiarg` declares a typed
  pattern — failure modes, compositions (each with augmentation
  schema and produced entity types), and a CHECK clause.

The `.edn` is canonical for the hypergraph layer. A thin `.el` adapter
generates the manifest format the existing `arxana-browser-essays-import`
consumes; eventually a new Arxana backend reads the `.edn` directly and
the adapter retires.

## The typed-rewrite discipline (f:(A,B)→C)

Every closure is a function application:

- **A** is a typed diagnostic annotation citing a pattern in the
  library.
- **B** is the rewrite plan: a composition chosen from the pattern's
  COMPOSITIONS menu, plus any augmentation (judgment calls the
  composition required).
- **C** is the post-rewrite tuple — new prose, updated annotation graph,
  invariant-delta, verification result.

The closure block carries:

```
:closure {:locus :scaffold-augmentable
          :pattern-name "<flexiarg-path>"
          :composition-id :<composition-canonical-id>
          :composition-text "<verbatim from the pattern>"
          :augmentation {<schema-shaped fields per composition>}
          :entities-before [<typed-entity-cover-of-:before-span>]
          :operations [<typed local-section operations>]
          :glue-adjustments [<compatibility-on-overlap effects>]
          :before "<verbatim original span>"
          :after "<verbatim post-rewrite span>"
          :invariant-delta {<invariant> [<before-status> <after-status>]}
          :verification {:pattern-check-pass? <value>
                         :invariants-weakened []
                         :invariants-unchanged [...]
                         :status :verified}
          :decided-by :joe | :claude | :claude-on-plan-approval
          :executed-by :joe | :claude
          :timestamp "YYYY-MM-DD"
          :rewrite-trace "<one-line summary>"}
```

A rewrite is *verified* iff (i) no invariant in the audit weakens,
(ii) the cited pattern's CHECK clause holds on the new span, (iii)
each operation is well-typed against its target entity's type,
(iv) glue-adjustments are exhibited as derived effects, not hidden in
the `:after` text.

## Entity decomposition (the sheaf reading)

Each paragraph is a typed open cover by *entities* — sub-spans tagged
with structural types (`:meta-lede-transition`, `:question`,
`:empirical-observation`, `:bolded-comparative-claim`, `:parking-closer`,
`:closure-owner-reference`, `:counted-frame-opener`, etc.). The vocabulary
grows organically (lazy normalisation); a registration discipline
becomes worth it once the entity vocabulary stabilises across multiple
essays.

Each operation is a *local section* — a typed transformation on one
entity. Operations include `:drop`, `:keep`, `:replace`, `:add` (with
`:add-after :target`), `:move` (with `:to-position`). When a composition
prescribes structured augmentation (e.g., "name the closure mechanism"
asks for `:closure-owner`, `:closure-support`, `:closure-event`), the
augmentation fields map 1:1 to typed entities introduced by the
operation.

Glue-adjustments are the *compatibility conditions on overlaps* —
derived effects on entities adjacent to a local operation:

- Dropping a meta-lede forces the next clause to be sentence-initial
  (capitalisation glue).
- Dropping a parenthetical hedge forces the preceding clause to grow
  its own terminal period.
- Splitting a fused paragraph forces an em-dash inside the original
  to be rephrased to a verb-link.

The global section (the new prose) is recoverable by composing the
operations and applying the glue.

## Theoretical grounding: rewrite geometry and the morphogenetic lineage

The discipline above is not original *as a problem shape* — it is one
local instance of a much older question. The lineage:

> **How do stable large-scale forms emerge from local interactions and
> transformations?**

A working constellation of thinkers who have framed variants of this
question, with the slot each one occupies in our discipline:

| Thinker | Primitive operation | Emergent object | What we lift |
|---|---|---|---|
| Spencer-Brown | distinction (drawing a boundary) | logical space | The Markov-blanket cut in `stack-annotations.edn` `:system` is a Spencer-Brown distinction made operational |
| René Thom | bifurcation in continuous parameter space | topology of change | When typed rewrites compose, regions of the annotation hypergraph can undergo *abrupt topological transitions* under continuous parameter movement — Thom's framing names what "morphogenetic evolution of the system" actually looks like |
| Christopher Alexander | pattern resolution | architecture / community | The `~/code/futon3/library/*` pattern library treats patterns as local resolutions; the stack's structure is the emergent global thing |
| Stephen Wolfram | graph rewrite | spacetime / process | `stack-annotations.edn` is the graph; the geometry of the stack-as-essay is *induced by* the rewrites the annotation hypergraph admits. Nearby states = states reachable by local typed rewrites |
| Karl Friston | inferential coupling under hierarchical Markov blankets | adaptive agency | Q-SA5's decompose-then-recompose discipline is hierarchical-AIF (rsif.2017.0792 Figure 3) at the schema's typing layer |
| Maturana / Varela | recursive self-production | autopoietic system | The stack-fitness contract's F6 (operator inhabitation) names the autopoietic loop's failure mode — operator absence from own substrate = autopoiesis without the operator's authoring participation |
| sheaf / topos theory | gluing local consistencies | global structure | The "section of a sheaf over the essay's typed open cover" framing earlier in this document is the formal version; double-pushout graph rewriting + operads are the technical machinery for the *multi-endpoint* case (Q-SA3) |

What the constellation contributes to the rewriting discipline is *not*
metaphor — it is a research direction. **Rewrite geometry: geometry
induced by compositional transformations and inferential constraints.**
The stack-annotations hypergraph instantiates this directly: the
annotation graph IS the geometry; typed rewrites move points in it;
composition preserves (or fails to preserve) the I1-I6 invariants; the
hypergraph's regions (Markov-blanket cut) are coherent local trivialisations
of an otherwise-unstructured space.

### Two compositional moves the lineage names

**Multi-endpoint hyperedges as operadic composition.** A binary edge
`A → B` admits one composition rule per direction. An n-ary hyperedge
admits *operadic* composition: an operation taking n inputs to one
output, composable under associativity + identity conditions. The
Q-SA3 resolution (multi-endpoint annotations rather than pairwise) is
the operadic typing of the discipline: it admits composite arms,
multi-attribution sorrys, and cross-cutting fitness witnesses as
single first-class entities rather than as redundant pairs. Operads
are the algebraic structure that makes this rigorous; the discipline
above uses them implicitly.

**Hierarchical decomposition as nested Markov blankets.** The Q-SA5
resolution refuses to type composites with a single region. Instead:
decompose until each leaf can take an honest single-region tag, then
recompose the distribution. This is Friston's "blankets of blankets":
each composite section's region-distribution is the *signature* of
its constituents' Markov-blanket structure. The cost is more nodes; the
gain is that composites cannot lie about their typing.

### Bifurcation under sustained rewriting

Thom's specific contribution is the diagnostic for *when* a sequence of
rewrites becomes morphogenetic rather than parametric. A rewrite is
parametric when it moves the system continuously along a smooth axis
(invariants drift, the spine reorders, but the section structure holds).
A rewrite is morphogenetic when it crosses a bifurcation — a section's
region distribution flips, an I6 closure shifts qualitative status,
the audit's `:status` field changes class. The schema's
`:invariant-audit` and `:lift-anomalies` blocks function as
catastrophe-theoretic instruments: they detect bifurcations as they
happen rather than discovering them as drift after the fact.

This is what Joe's "rewriting could then switch into a morphogenetic
evolution of the system" means precisely. The system continues to be
itself across small rewrites; it *becomes a different system* when
rewrites cross a bifurcation. The discipline's job is to make those
crossings *visible and audited*, not to prevent them.

### What's not in scope (yet)

- The full double-pushout graph rewriting calculus. We have the typed-
  rewrite f:(A,B)→C and the sheaf-section reading; formalising in
  DPO terms is a future technical move worth taking if we want to
  prove compositional properties of pattern menus.
- Wolfram's substitution systems as full computational substrate.
  The futon stack is not a Wolfram-style universe; we use the
  *intuition* that geometry comes from rewrite connectivity, not the
  full machinery.
- Categorical compositionality in the strong (functorial) sense.
  Patterns admit `+ COMPOSITIONS` menus but we have not defined
  formally what composition of compositions means. The Q-SA3
  operadic framing is a first step.

### The BHK → Curry-Howard → Girard → our-direction sequence

A specific sub-lineage is worth naming because it locates the
discipline within computational logic rather than only morphogenesis.
The progression:

| Tradition | Main intuition |
|---|---|
| BHK (Brouwer-Heyting-Kolmogorov) | proofs are constructions |
| Curry-Howard | proofs are programs |
| Girard (linear logic, Geometry of Interaction) | proofs are interacting resource flows |
| Our direction | rewrites are localized morphogenetic interactions |

Girard's **Geometry of Interaction** is the specific lift worth
calling out. The idea: cut-elimination is not merely symbolic
simplification but a *flow through a graph / network*; proofs
interact dynamically; meaning emerges from paths of interaction.
Proofs become trajectories, propagations, local rewrites, signal
flows. That is strikingly close to:

- our local operations + compatibility conditions
- the multi-endpoint hyperedges (Q-SA3) as proof-net-style
  composition
- the AIF² invariant audit (I1-I6) as a coherence condition on flow
- the substrate-2 hypergraph as a discrete analog of proof-net
  geometry

The bridge to *operational* rewriting comes via **Yves Lafont's
interaction combinators**, which take Girard's interaction ideas
into explicit local graph rewriting. This is plausibly the
shortest path from BHK intuition to operational rewrite geometry —
worth a future read if the typed-rewrite calculus is ever formalised
to the level where its compositional properties can be proved
rather than asserted.

### Existing concrete instances in the stack

A multi-agent check (claude-8, 2026-05-17) surfaced where the
discipline is *already* operadic-by-construction-but-implicit-in-typing
in the npt apparatus:

- `~/npt/working-paper/annotations-v12.edn` calls itself a "canonical
  hypergraph"; closed annotations like
  `hx:wp:v12:s6-future-work-integration` link multiple paper spans ×
  patterns × external refs — multi-endpoint by construction, but the
  schema is hand-typed rather than derived from a composition theory.
- `~/npt/working-paper/aif_library_to_criteria_crossref.md` —
  **closest concrete instance.** Each row is an explicit 3-endpoint
  relation (R-criterion × library-pattern × evidence-pointer),
  self-described as "three coordinates per row." Currently markdown.
- The AIF code (`notebooks/ukrn_v3_*.clj`) uses the **shared kernel
  pattern** — one pure function called both by the live step and by
  the predictive forward model — which is architectural-discipline
  composition rather than operadic typing.

**Smallest next technical move (from claude-8):** lift
`aif_library_to_criteria_crossref.md` from markdown to a typed
signature, e.g. `cross-ref : pattern-id × criterion-id × evidence-id → composed-relation`,
with chaining rules for how cross-refs compose through implementation
graphs. *The data is already there; the typing isn't.* This is the
operadic-composition graduation point — and a candidate task that
follows the v0.5 stack-annotations schema's first ingest pass
(Phase 2.d.2 onward).

### References for further grounding

Three refs that sharpen the lineage without bloat:

- **Justin Curry, *Sheaves, Cosheaves, and Applications* (2014 PhD
  thesis, University of Pennsylvania).** Accessible introduction to
  sheaves on combinatorial structures (posets, simplicial
  complexes) — the right level for the rewrite-geometry framing
  here.
- **David Spivak, *Category Theory for the Sciences* (MIT Press,
  2014) + the wiring-diagram operad work.** Engages multi-endpoint
  composition explicitly in a software-systems vocabulary.
  Directly relevant to Q-SA3.
- **Ehrig, Heckel, Corradini et al., *Fundamentals of Algebraic Graph
  Transformation* (Springer, 2006).** Canonical reference for
  double-pushout graph rewriting — the formal calculus the f:(A,B)→C
  discipline approaches.

Note on existing npt theoretical citations: the npt AIF+ apparatus
(annotations-v12.edn, the F1-F12 / R1-R12 contract, the v3 runner
scoping) explicitly draws on **Friston–Da Costa–Parr** for AIF,
**May–Finch (2009)** for Normalisation Process Theory, and **Ashby
(requisite variety)** in the four-tier scoping. The refs above
*extend* that lineage with the explicit compositional machinery
(sheaf / operad / DPO), not replace it.

Two further figures worth tracking once the discipline is being
formalised:

- **Jean-Yves Girard** — *Linear Logic* (1987) and *Geometry of
  Interaction* (1988 onward). Proofs as flows; resource-sensitive
  composition; cut-elimination as graph dynamics. The closest
  formal home for "compositional closure under typed rewrites."
- **Yves Lafont** — *Interaction Combinators* (1997). Local graph
  rewriting that operationalises Girardian interaction; a
  computational substrate for "rewrites as localized
  morphogenetic interactions."
- **Joachim Lambek** — sits near the intersection of rewriting,
  syntax, category theory, and compositional semantics; a useful
  bridge between the operadic / monoidal-category framing and the
  rewriting calculus.

### The composite stack (synthesis recommendation)

A reading of what the discipline-as-it-stands amounts to once these
threads are taken together:

| Layer of the rewriting discipline | Best theoretical lineage |
|---|---|
| Local / global coherence | Sheaf theory |
| Rewrite mechanics | DPO graph rewriting (Ehrig et al.) |
| Semantic transformation geometry | René Thom (catastrophe / morphogenesis) |
| Adaptive / inferential interpretation | Active inference / Markov blankets (Friston et al.) |
| Compositional closure under interaction | Girard / Lafont (Geometry of Interaction; interaction combinators) |

A condensed reading of what the assembled stack amounts to:

> ***Typed sheaf-theoretic graph rewriting with invariant-preserving
> morphogenesis, operating on an active-inference-coupled annotation
> hypergraph.***

That sentence is grandiose-sounding but is in fact a direct reading
of the discipline as implemented: typed (annotation `:hx-type`),
sheaf-theoretic (sheaf-section over typed open cover with overlap
compatibility), graph rewriting (f:(A,B)→C with operations on
typed entities), invariant-preserving (the AIF² I1-I6 audit),
morphogenetic (typed rewrites that cross bifurcation surfaces in
the audit class), on an active-inference-coupled (the F1-F10
fitness contract) annotation hypergraph (`stack-annotations.edn`
under the v0.5 schema).

The discipline does not need this sentence to operate. But it
explains what is being done with enough compression that future
implementers can recognise what they're touching.

### One-line takeaway

The rewriting discipline is not a notation. It is a *geometry-
generating mechanism* whose lineage runs through Spencer-Brown,
Thom, Alexander, Wolfram, Friston, and the operad / sheaf / DPO
calculi, and whose specific contribution at stack scope is *the
operator-inhabited annotation hypergraph as a substrate the
operator and the stack can both evolve under typed rewrites with
audited invariant deltas*.

## The pattern library

Each `.flexiarg` declares:

- A `! conclusion` stating the structural principle.
- A `+ context` motivating the principle.
- A `+ FAILURE-MODES` enumerating the specific surfaces the principle
  catches.
- A `+ COMPOSITIONS` menu of typed transformations, each with its own
  augmentation schema and entity-types produced.
- A `+ CHECK` clause naming what success looks like.

Patterns coined or extended during the UKRN working-paper v12 pass:

- `system-coherence/plan-as-enumeration`
- `system-coherence/argument-as-coverage-checklist-vs-system-specification`
- `system-coherence/verified-rewrite-from-diagnostic-annotation` (the
  meta-pattern; the spec for typed rewrites themselves)
- `system-coherence/tag-evidence-by-timescale`
- `system-coherence/match-region-weight-to-load`
- `system-coherence/present-graph-topology-not-adjacency-lists`
- `writing-coherence/citation-density-load-bearing-claim`
- `writing-coherence/one-paragraph-per-typed-claim`

Plus the existing writing-coherence library
(`stale-reference-after-restructure`, `subheading-without-paragraph`,
`meta-lede`, `triad-inflation`, etc.) was used directly.

## AIF² invariants (the audit lens)

Borrowed from the AIF+ wiring-diagram formalism in
`~/code/futon5/docs/chapter0-aif-as-wiring-diagram.md` (see also AIF
Dual Usage §4.7 of `~/code/futon4/holes/missions/M-three-column-stack.md`).
Six structural properties checkable as graph properties of the typed
annotation graph:

- **I1** Boundary Integrity — the essay names what it settles vs. what
  it hands off.
- **I2** Observation-Action Asymmetry — the essay both senses and acts
  (typically via roles or the recommendation).
- **I3** Timescale Separation — fast / medium / slow / glacial evidence
  is typed, not flattened.
- **I4** Preference Exogeneity — no Action→Preference path that doesn't
  pass through Environment (the wireheading check).
- **I5** Model Adequacy — internal claims track external reality.
- **I6** Compositional Closure — removing one role / capability /
  exchange degrades but doesn't crash the design.

Each `:closure` block records its `:invariant-delta` (which invariants
moved, in which direction). The top-level `:invariant-audit` accumulates
witnesses and resolved failures per invariant. A separate
`:region-balance-audit` tracks page-weight balance across AIF² regions
(outside / sensory / internal / active-surface / preferences).

## The 3-up review surface

`dev/arxana-browser-rewrites.el` opens a dedicated frame "Arxana
Rewrites" with three side-by-side panels:

- **Version A** (left) — the `:before` span, rendered entity-by-entity
  with fate tags (`keep`, `DROP`, `REPLACE`, `keep (glue → :effect)`)
  and op pointers (`← op=:drop`).
- **Rewriting Rule** (centre) — the closure block laid out structurally:
  pattern citation, composition, augmentation, operations (with target
  entities and `:with-entities` for replaces and adds), glue-adjustments
  with their causes, invariant-delta, verification status.
- **Version B** (right) — the `:after` span, with kept entities aligned
  to A, dropped entities shown as placeholder lines, replaced entities
  followed by the newly-added entities they introduced.

Entity ids align across A and B for KEEP entities; structural changes
are localised at their specific entity positions. The sheaf is
physicalized: local sections visible per-entity, the global section
recoverable by reading across.

Entry point:

```elisp
M-x arxana-browser-rewrites-open
;; or
(arxana-browser-rewrites-open "hx:wp:v12:s1-1-local-self-eval-parked")
```

Action keys (currently stubs that print messages):

- `a` accept — mark `:reviewed-by :joe` on the closure
- `r` reject — revert prose, reopen the annotation
- `e` edit — open the closure block for structured editing
- `d` defer — leave open, look at it later
- `p` promote — capture a finding from this rewrite for promotion to
  the pattern library
- `n` / `b` next / back through closed annotations
- `g` reload from disk (always re-reads the .edn)
- `RET` open the cited pattern's flexiarg in a side buffer

## The workflow loop

1. **Survey.** Read the essay against the AIF² invariants and the
   writing-coherence library. Produce typed annotations
   (`:hx-type :annotation/comment`, `:status :open`) citing patterns.
2. **Diagnose.** For each open annotation, identify the cited pattern's
   relevant composition and the augmentation needed.
3. **Rewrite.** Apply the prose change. Decompose the affected span
   into entities. Record operations, glue-adjustments, and the
   invariant-delta in a `:closure` block. Mark `:status :closed`.
4. **Verify.** Check the pattern's CHECK clause on the new span; check
   that no invariant weakened in the audit.
5. **Review.** Walk the closed annotations in the 3-up surface;
   accept / reject / edit / promote-finding per rewrite.
6. **Promote.** Findings captured at review time amend the pattern
   library (new flexiarg, new composition, new failure-mode), or
   amend the verified-rewrite spec itself. This is the
   structure-learning loop.

The loop's invariant: same typed problem cannot recur, because the
typed precondition that surfaced it no longer holds in the post-state.

## Caveats worth keeping in mind

- **Mechanical fixes have no pattern.** Typos and punctuation fixes
  flow through the same channel with `:pattern-name nil` and
  `:composition-id :mechanical-typo-fix`. Pragmatic minimum; a
  spec refinement (meta-pattern, or separate annotation type) is
  pending.
- **Entity vocabulary grows lazily.** A registration discipline
  (`entity-types-registry.edn`) becomes worth it once the same
  conceptual type recurs under different names in 3+ rewrites.
  Until then, synonyms are tolerated.
- **The EDN reader is a minimal subset.** Maps, vectors, lists,
  strings (with `\\n`/`\\t`/`\\\\` escapes), namespaced keywords,
  numbers, nil/true/false, line comments. Tagged literals
  (`#inst "..."`) are read with the tag discarded — the form is
  returned bare. Sets (`#{...}`) are read as lists (order
  irrelevant for our consumers). Character literals (`\\u00e9`)
  are still not supported; if you need them, the reader needs
  extending.
- **Bracket-balance on edits matters.** Edits that drop closing
  brackets at the end of a replaced segment will break the parser.
  `check-parens` catches global imbalances; structural misplacement
  (extra closing material balancing missing closing material
  elsewhere) requires more care. The eventual mutate-through-typed-API
  surface will avoid this class of bug.
- **Stale `.elc` precedence.** Emacs prefers a newer `.elc` over a
  newer `.el`. During iterative work, delete the `.elc` or set
  `(setq load-prefer-newer t)`. The `arxana-browser-rewrites.el`
  surface is intentionally not byte-compiled during development.

## Related

- Essay generation (the upstream half): `README-essays.md`
- Engine spec (interview/assembly): `~/code/algorithms/eoi-engine.md`
- Schema (institution-objects): `~/code/atthangika-buckets.json`
- AIF+ wiring diagram formalism:
  `~/code/futon5/docs/chapter0-aif-as-wiring-diagram.md`
- AIF Dual Usage (Active Inference × Argument Interchange):
  `~/code/futon4/holes/missions/M-three-column-stack.md` §4.7
- Pattern library: `~/code/futon3/library/{system-coherence,writing-coherence}/`
- Meta-pattern (spec for typed rewrites):
  `~/code/futon3/library/system-coherence/verified-rewrite-from-diagnostic-annotation.flexiarg`
- Review surface: `dev/arxana-browser-rewrites.el`
- Worked corpus (UKRN working-paper v12, twelve closed rewrites):
  `~/npt/working-paper/annotations-v12.edn`
- Mission-scoped worked instance (`M-stack-essay-code-alignment`,
  VSATARCS-side, R-criteria typing axis): `docs/vsatarcs-alignment-completeness.aif.edn`.
  Refactors the npt v12 shape from AIF² invariants (I1-I6) onto R-criteria
  (R1-R12); the contract doc it overlays is `docs/vsatarcs-alignment-completeness.md`.
- Companion `.el` adapter (current Arxana import surface):
  `~/npt/working-paper/annotations.el` (v5/v6 era; v12-adapter pending)
- Essays browser: `dev/arxana-browser-essays.el`
- Annotation lifecycle (retraction visibility, diachronic graph):
  `holes/missions/M-essays-edit-cycle.md`

## Spec-refinements pending

Findings recorded during the UKRN v12 pass, awaiting absorption into
`verified-rewrite-from-diagnostic-annotation.flexiarg`:

1. `:composition-id` can be a vector for multi-composition rewrites.
2. `:op :move` needs explicit `:to-position` schema.
3. Mechanical fixes need either a meta-pattern or a separate
   annotation type.
4. Glue-vs-sub-entity-edit boundary — when a "glue-adjustment"
   removes >50% of an entity's body, it's probably a `:replace` on
   a sub-entity, and decomposition should go one level deeper.
5. `:also-closes [<ids>]` for derived-effect annotations (one
   rewrite closing multiple diagnostics).
6. `:pattern-check-pass?` should be multi-valued (passes-locally,
   partial-document, etc.), not boolean.
7. Composition canonical names alongside numeric ids.
8. Augmentation-schema declaration per composition (consistently
   across all flexiargs).
9. Edit-time bracket-balance preservation (operational; the eventual
   mutate-through-typed-API surface).
10. Status enums vs descriptive strings in the invariant-audit.
11. **Code-side entity convention for `:operations :with-entities`.**
    The npt v12 corpus rewrites prose, with entities typed as prose
    spans (e.g. `:role-bullet-with-handoff-stack`,
    `:closing-paragraph`) and bodies in `:text`. Code-side closures
    (first instance: `hx:vsatarcs-align:v0-2:r1-chrome-integration-closure`
    in `docs/vsatarcs-alignment-completeness.aif.edn`) rewrite code
    files, with entities that are elisp / clj symbols — defuns,
    defvars, require-clauses, ert-deftests. The first code-side
    closure was refactored to fit the npt field shape
    (`:id`/`:type`/`:from-augmentation`/`:text`) by stringifying the
    function-name list into `:text`; this works but loses the
    structured handle on the underlying symbols. The renderer in
    `dev/arxana-browser-rewrites.el` (line ~640, `:with-entities`
    cl-loop) reads only the four prose-oriented fields and would
    benefit from a code-side dispatch — e.g. a richer entity type
    namespace (`:elisp-defun`, `:elisp-require-clause`, `:ert-deftest-block`)
    plus structured fields (`:symbol`, `:tests`, `:insertion-point`)
    rendered with appropriate faces. Lifting this becomes worth doing
    once we accumulate a second code-side closure to compare.

Each is small. Together they sharpen the typed-rewrite discipline
without changing its shape.
