# M-formal-arxana contributor path

Date: 2026-11-16

This is the completion record for the DOCUMENT phase.  The construction and
review history remains in `INSTANTIATE.md`; this file is the short route for a
new contributor and the boundary around what the demonstrations establish.

## Load once

In Emacs, evaluate:

```text
M-x load-file RET /home/joe/code/futon4a/lisp/futon4a-formal-demos.el RET
```

That file loads the common client and all six views.  Each demonstration uses
an isolated fixture service; its launcher is the named path below.  Launchers
record their own PID and their matching `stop-fN-demo.sh` refuses an unrelated
process.  Never use a process-name kill.

| Step | Start from a shell | Open or act in Emacs | What it demonstrates | What it does not prove |
|---|---|---|---|---|
| F1 | `futon4a/scripts/launch-f1-demo.sh` | `M-x futon4a-formal-f1-linear`, `-outline`, `-gcc` | One set of interpretation records keeps its identity across linear, outline and GCC projections; contradictory assessments remain present. | That a projection is the authority, or that model evidence is human acceptance. |
| F2 | `futon4a/scripts/launch-f2-demo.sh` | `M-x futon4a-formal-f2-configure-from-ready`, then `M-x futon4a-formal-f2-open`; submit with `C-c C-c` | Two clients edit through one CAS sequencer; anchors relocate and overlapping stale edits become an explicit conflict with alternatives. | A general CRDT, arbitrary offline merge, or conflict-free collaborative text. |
| F3 | `futon4a/scripts/launch-f3-demo.sh` | `M-x futon4a-formal-f3-correspondence`, `-manifestations` | Authored prose, Lean and generated prose retain distinct provenance, typed correspondence and an explicit synthetic contradiction. | That every informal step aligns, or that generated prose inherits kernel standing. |
| F4 | `futon4a/scripts/launch-f4-demo.sh` | `M-x futon4a-formal-f4-open`; `RET` inspects a record; `M-x futon4a-formal-f4-execute` exercises the admitted action | A 'clusion can contain typed recursive stages; execution creates attributable attempt/receipt history behind capability and admission. | That arbitrary effects are permitted, predicted effects are receipts, or semantic disagreement has disappeared. |
| F5 | `futon4a/scripts/launch-f5-demo.sh` | `M-x futon4a-formal-f5-open`; `M-x futon4a-formal-f5-select-map` | The square/cube examples answer map, invariant and fibre queries independently of Cartesian or polar rendering; coordinate-only and semantic changes differ. | General geometry, or that either drawing is the graph authority. |
| F6 | `futon4a/scripts/launch-f6-demo.sh` | `M-x futon4a-formal-f6-outline`; then `-move-diagram` and `-re-support` | Layout is a projection of stored argument items and support relations; moving a diagram and changing what it supports are different operations. | That pixel position is semantic identity, or that the page is an unrestricted editor. |

The corresponding non-interactive walks are
`futon4a/test/e2e-formal-f1-demo.sh` through F3, plus
`formal-f4-e2e.sh` and `formal-f6-e2e.sh`.  F5's retained read path is
`futon4a/test/futon4a-formal-f5-test.el`.  The end-to-end scripts use
temporary runtime directories and private ports; they are the reproducible
path when no interactive reading is needed.

## Conceptual glossary

- **'clusion** — an immutable, recursively addressable typed relation with
  ordered, role-labelled endpoints and declared identity-bearing content.
- **attestation** — attributable evidence about a subject 'clusion; producer,
  method, basis, standing and coverage do not mutate the subject.
- **assessment** — an attributable candidate/accepted/rejected judgment.
- **manifestation** — concrete bytes at an immutable artifact revision.
- **scope / relocation** — a locator in one revision and the evidence-bearing
  result of finding its successor in another; ambiguity and absence are data.
- **projection** — a view computed from records.  It is never a competing
  source of identity.
- **attempt / receipt** — what an operation tried, and the observed result.
  A prediction is not a receipt.
- **external reference** — a typed pointer to an authority that Formal Arxana
  indexes but does not copy or silently reinterpret.
- **introduction** — an attributable assertion that a named entity has entered
  the working vocabulary, with the source turn as its referent.
- **referent / witness** — a typed link to what an entity denotes; as an answer
  to a link query, the link record is the witness rather than a copy of its
  target.
- **port** — a proposed entity for a system's named read, write or delivery
  interface.  The worked record refers to E-gcc-schema B5; no live writer is
  claimed.
- **want** — a proposed open link query with a horizon.  Its worked record
  refers to E-gcc-schema B7; expiry means unresolved, not false.
- **construction** — evidence of the transformation that establishes a link,
  such as the existing `[:kernel-builds 1]` relation; not every witness is a
  construction.
- **institution statement** — a proposed entity/attestation describing a
  rule-in-use; M-ways-of-working and E-gcc-schema B12 remain its authorities.

## Authority boundaries

XTDB is the durable graph authority.  Artifact bytes and revisions remain the
manifestation authority.  Lean kernel evidence establishes only the checked
formal claim.  Human acceptance remains an assessment by the named human;
model readings stay model evidence.  Admission and governance belong to
M-ways-of-working.  GCC and Emacs pages are projections.  M-repl-circus owns
the evolving turn/reply/mission/pattern action surface; Formal Arxana supplies
record identity, evidence, correspondence, bounded query and projection
contracts, not a second orchestration authority.

Legacy entity and hyperedge rows remain read-compatible and write-frozen.
`futon3c.arxana.legacy-records` exposes a typed view while retaining the exact
observed row as `:legacy/original`; `formal_c1_test.clj` checks round-trip
equality against retained real entity and mission-scope hyperedge captures.

## Intentionally unresolved

The six demonstrations are accepted, but they are not a claim that the graph
vocabulary is finished.  E-gcc-schema Part B14 is the live vocabulary debt
register: ports, wants, construction requirements, feature-benefit claims,
institution structure, additional animal semantics and schema evolution stay
open until their named trigger supplies evidence.  New kinds require a
writer, a reader, and a query that needs the distinction; absence of those is
not permission to add a speculative record kind.

The closure seam is therefore: M-formal-arxana owns the tested core contracts
and completed demonstrations; E-gcc-schema owns vocabulary certification;
M-repl-circus owns subsequent operational GCC evolution.  Closing this
mission does not silently decide the items in E-gcc-schema B14.
