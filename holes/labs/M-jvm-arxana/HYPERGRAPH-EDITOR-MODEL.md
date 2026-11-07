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

These obligations should be proved first with the intent-marked conversation
example. It is small, already present in real operator/agent traffic, and can
be projected both linearly and graphically without depending on the harder
Lean/LaTeX semantics.
