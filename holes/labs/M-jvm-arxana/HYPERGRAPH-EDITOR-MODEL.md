# Arxana as a hypergraph editor and compositional typesetting system

Date: 2026-11-07

Status: conceptual contract for later design and acceptance work. This note
extends, rather than replaces, the artifact, correspondence, action-boundary,
and Edit Robustness contracts.

## Principal claim

Arxana edits a typed semantic hypergraph. Linear text, source code, rendered
mathematics, diagrams, conversations, editor panes, and page layouts are
projections through which finite agents inspect and propose changes to that
graph. No projection is presumed to exhaust the content it presents.

Arxana is therefore a compositional typesetting system in a broad sense:
meaningful contents and typed relations are composed, selected, ordered, and
rendered into surfaces. Character strings and pixel positions are important
manifestation data, but they are not the universal identity of the contents.

## Typed, local sameness

The model has no unqualified `same-as`. It admits relations whose warrants and
consequences differ, including:

- same artifact revision;
- same logical scope across revisions;
- manifestation of the same content entity;
- generated from;
- translation or informalisation of;
- corresponding proof move;
- approximation or partial formalisation of;
- contradicts;
- accepted as corresponding by an actor under a named policy.

Claims of sameness must therefore name a relation, endpoints, revisions,
evidence, standing, and—where judgement is involved—attributable assessment.
This is already compatible with D4 Correspond and its append-only assessment
records; the present note makes the general principle explicit.

## 'Clusion: the first-class recursive relation

This model adopts the earlier definition in
`holes/missions/M-self-representing-stack.md` §Hyperedges as Frozen Dynamics.
Nelson's transclusion is the degenerate case: one source is included at one
destination under an identity-preservation claim. Joe's **'clusion** is the
general case: gather one or more contents, perform declared operations, and
compose or relate the results.

A 'clusion is not merely an RDF-like predicate or an endpoint list. It is an
identified, typed, attributable hyperedge carrying:

- inputs and their named roles;
- an operation, relation, or frozen dynamic;
- outputs and their named roles;
- topology and composition ports;
- provenance and exact input revisions;
- a semantics/method version;
- evidence and standing;
- conflict/convergence policy; and
- activation requirements when the dynamic is executable.

Transclusion, translation, projection, interpretation, generation,
approximation, correspondence, contradiction, and compilation are therefore
different 'clusion types. They must not be collapsed into an unqualified
`same-as` relation.

The hypergraph is fully recursive because a 'clusion is itself content. It can
be an endpoint of another 'clusion, projected into a surface, contradicted,
assessed, superseded, quoted, or composed. An assessment of a correspondence,
for example, is a 'clusion whose endpoint includes the correspondence
'clusion—not metadata hidden beside it.

An indicative record shape is:

```clojure
{:clusion/id "clusion:..."
 :clusion/type :conversation/intent-interpretation
 :clusion/inputs [{:role :source-act :id "turn:..."
                    :revision "arxana:revision:sha256:..."}]
 :clusion/operation {:method :xiang/reply-proforma
                     :version 1}
 :clusion/outputs [{:role :interpreted-intent
                     :id :intent/delegate}]
 :clusion/evidence ["evidence:..."]
 :clusion/standing :candidate
 :clusion/convergence :append-only}
```

This is a semantic contract, not authorization to evaluate arbitrary stored
forms. A frozen executable dynamic still passes through D6's separation of
intent, candidate action, admission, execution, and receipt. Browsability and
executability are distinct capabilities.

## Five editable strata

An edit made through a surface may propose a change at one or more strata:

1. **Manifestation:** bytes, glyphs, AST syntax, rendered DOM, diagram geometry,
   or another concrete form.
2. **Content:** a proposition, definition, proof move, intent, action candidate,
   explanatory unit, or other typed semantic entity.
3. **Relation:** correspondence, dependency, ordering, contradiction,
   provenance, assessment, or another typed hyperedge.
4. **Projection:** rules selecting and arranging graph material on a human- or
   machine-facing surface.
5. **Presentation state:** cursor, selection, fold, decoration, viewport, or
   cache. This stratum is normally disposable and non-authoritative.

The originating client must not silently decide that a manifestation edit is
a content edit, nor that presentation state is semantic evidence. A projection
adapter emits a typed edit proposal; the authority for each affected stratum
validates it; accepted graph changes are then reprojected.

## Projection editing cycle

The minimal abstract cycle is:

```text
graph snapshot
    -> project(surface profile)
    -> rendered surface + addressable provenance
    -> user/agent gesture
    -> typed edit proposal
    -> validate against the authority of each affected stratum
    -> append accepted graph records / emit typed refusals
    -> reproject
```

Addressable provenance is essential: a surface fragment must retain enough
information to say which graph contents and projection rules produced it.
Otherwise an edit can only be interpreted as an ungrounded character or pixel
change.

## Example 1: reply intent marks

In `㊯ [target] text`, the glyph is one manifestation of an intent-bearing
speech act. It is not itself the intent. A conforming interpretation may
produce records equivalent to:

```clojure
{:entity/id "turn:..."
 :entity/type :conversation/turn
 :manifestation/revision "arxana:revision:sha256:..."}

{:edge/type :conversation/has-intent
 :edge/endpoints ["turn:..." :intent/delegate]
 :evidence {:projection/profile :xiang/reply-proforma-v1
            :source-range {:start 0 :end 1}}}

{:edge/type :conversation/addresses
 :edge/endpoints ["turn:..." "task-or-actor:..."]}
```

The same graph records could be projected as a leading glyph, an Emacs text
property, a colored margin mark, a JSON field, or an edge in a conversation
map. Conversely, the glyph outside the declared grammar does not necessarily
assert the relation.

Acceptance example:

1. edit only the prose after an intent mark: turn identity and intent edge
   survive while the manifestation revision changes;
2. change `㊯` to `㊢`: the client proposes replacing `delegate` with `report`,
   retaining both the source act and attribution history;
3. remove or introduce an ambiguous mark: the parser reports missing or
   ambiguous interpretation rather than silently manufacturing an intent;
4. render the same accepted intent as glyph, outline node, and graph edge and
   show that all projections address the same records.

### 象 → REPL Circus → GCC composition

Intent-marked conversation supplies a bounded joint proof for M-象-2000 and
M-repl-circus. The source turn and its manifestation remain evidence; a 象
interpretation 'clusion produces a typed speech act; grounding 'clusions link
that act to missions, capabilities, scopes, or actors; the D6 compiler produces
a non-executable candidate-action 'clusion; and the GCC projects candidates,
warrants, risks, and unresolved alternatives into an inference-ready action
surface.

```text
turn manifestation
  -> interpretation 'clusion
  -> speech-act content
  -> grounding 'clusion(s)
  -> candidate-action 'clusion
  -> GCC projection
```

Every arrow denotes an addressable record with its own provenance and standing.
Later evidence may criticise the interpretation without rewriting the turn, or
criticise the grounding without denying that the interpreted intent existed.
This is the recursive structure that a flat list of intent labels would lose.

## Example 2: programming-language structure

A parser projects source characters into an AST; the AST exposes content such
as bindings, applications, and definitions. Evaluation assigns operational or
denotational meaning. A catamorphism is a systematic fold that replaces a
recursive datatype's constructors with an algebra and combines bottom-up.
Ordinary Lisp `apply` is function invocation with an argument sequence, not in
itself a catamorphism, although an evaluator or analysis may be expressed as a
fold.

Arxana need not replace these language authorities. It relates their typed
outputs to other contents and manifestations. A source edit may relocate a
definition, change its AST while retaining an accepted logical identity, or
invalidate a prose correspondence; each consequence has a separate warrant.

Acceptance example: rename and move a definition, then show separately that
the source scope relocated, the AST binding changed under the language
authority, and a linked explanation either remained applicable or became a
typed semantic conflict.

## Example 3: mathematical notation and layout

Linear LaTeX, a two-dimensional rendered expression, a Lean term, and a spoken
reading are distinct manifestations. Linear serializability does not make them
the same language or give them the same binding, typing, or layout semantics.
Their relations are directional and evidence-bearing.

A Tufte-style page is likewise a projection of argument structure: main-line
claims, examples, diagrams, source definitions, proof obligations, and
sidenotes are meaningful contents whose layout expresses selected relations.
Pixel coordinates remain presentation state unless a declared projection rule
turns a spatial relation into evidence.

Acceptance example: move a diagram from margin to main flow without changing
its content identity; separately change the claim it supports and require an
attributable relation edit. Re-rendering may choose new pixels while preserving
the semantic support edge.

## Consequences for concurrency

A text CRDT or OT algorithm converges manifestation edits within one shared
character-sequence projection. It does not provide a universal merge operator
for the hypergraph. Other strata require typed policies:

- append-only evidence and assessments may commute;
- competing correspondences may coexist;
- derived projections may be recomputed;
- uniqueness-governed definitions may require refusal or adjudication;
- semantic contradictions remain explicit until resolved by an authorized
  decision.

Thus collaborative Arxana needs convergence declared per record or relation
type, not a single last-writer-wins policy over the graph.

In “conflict-free replicated datatype,” conflict-free describes operational
merge: replicas can converge on the same state without an unhandled merge
failure. It does not imply that the converged contents are semantically
consistent. Replicas may lawfully converge on both `A` and `not-A`, represented
as competing 'clusions with an explicit contradiction relation.

Resolution therefore has four distinct levels:

1. **replica convergence** — participants hold the same graph records;
2. **structural normalization** — records satisfy identity, role, topology, and
   provenance constraints;
3. **semantic resolution** — a named theory or policy reconciles, selects, or
   rejects competing contents; and
4. **productive non-resolution** — ambiguity, contradiction, missing theory,
   or a required domain extension remains first-class.

The equation `x² = -1` illustrates domain-sensitive resolution. It is
unsatisfiable over the reals; extending to the complexes makes solutions
available, but does not identify the two roots `i` and `-i`. The richer type
preserves branching rather than forcing equality. Correspondingly, Arxana
admits at least four outcomes: lawful join of compatible information,
attributable choice, extension into a richer theory/type, and explicit
non-resolution.

A replicated Arxana store can consequently converge by accumulating typed
'clusion claims and assessments while semantic processors derive qualified
standings such as `verified`, `partial`, `ambiguous`, `contradictory`,
`unsupported`, or `resolved-under <theory-or-policy>`. Operational convergence
must never be reported as semantic agreement.

## Design obligations opened by this model

1. Define the common envelope for a projection adapter's typed edit proposal.
2. Require projection outputs to carry addressable provenance back to graph
   records and projection rules.
3. Assign one authority per question for each editable stratum.
4. Specify typed conflict and convergence policies per core record/relation
   type.
5. Demonstrate round trips in which graph identity survives reprojection while
   manifestation and presentation details change.
6. Keep inference honest: a projection may reveal or suggest structure, but it
   cannot silently promote that suggestion into authoritative graph content.
7. Give 'clusion types queryable schemas for roles, topology, dynamics,
   composition, standing, and convergence policy.
8. Prove that replica convergence can preserve an explicit semantic
   contradiction without resolving it by overwrite.

These obligations should be proved first with the intent-marked conversation
example. It is small, already present in real operator/agent traffic, and can
be projected both linearly and graphically without depending on the harder
Lean/LaTeX semantics.
