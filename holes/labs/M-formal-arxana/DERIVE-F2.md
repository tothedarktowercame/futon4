# DERIVE-F2 — collaborative same-artifact editing

Recorded by claude-13, 2026-11-08, from codex-21's answers (round 5, job
invoke-1794099415833-15425-816686ea). Core contract: DERIVE-core.md.

## F2 as written (mission §1)

> Emacs and VS Code/code-server edit one shared document while preserving
> shared scope identities and semantic markup. Concurrent non-conflicting
> edits, same-range conflict, copy ambiguity, offline replay, and promotion
> are tested. A CRDT/OT substrate is warranted only if two legitimate
> histories must survive without an accepted sequencer; semantic conflicts
> remain graph content even after replicas converge operationally.

## Clarifications

1. **Artifact**: the F1 exported conversation Markdown. F2 proves that
   collaborative editing keeps the semantic markup F1 established.
2. **Write path**: both clients (futon4a in Emacs; the futon6 VS Code /
   code-server client) write successor revisions with `revision/ingest` and
   an expected head (futon1b CAS). A stale writer gets a typed conflict and
   keeps its patch.
   - F0 relocates semantic scopes; it does **not** merge text.
   - Non-overlapping edits: deterministic three-way rebase, then resubmit.
   - Same-range edits: kept as explicit alternatives that need an
     attributable choice.
3. **Promotion** means two things:
   - client working bytes → immutable artifact revision and new head
     (mandatory);
   - relocated, qualified semantic observations → durable scope/'clusion
     evidence naming that revision (shows shared semantic markup, not
     text-only collaboration).
4. **Falsifier for CRDT.** The single sequencer suffices if every legitimate
   concurrent or offline edit can be kept, rebased, or presented for
   attributable choice without losing intent, and collaborators accept one
   CAS head as sequencer. **Offline replay against an old head is the
   deciding test.** A CRDT is warranted when legitimate branches must
   converge without an accepted sequencer, or when the stale/rebase
   workflow destroys work or required availability.
5. **Out of scope**: cursor and presence transport. Optional as a
   presentation demo; never evidence, never a dependency of convergence.

## Acceptance cases (from the mission text and the above)

- concurrent non-conflicting edits from both clients → one rebased
  successor; scopes relocate;
- same-range conflict → typed conflict; both alternatives kept; attributable
  choice recorded;
- copy ambiguity → resolution `ambiguous`, both candidates kept (F0 case);
- offline replay against an old head → rebase or attributable choice; the
  outcome decides the CRDT question and is recorded either way;
- promotion → new head plus scope/'clusion evidence on the new revision.

## Depends on

F1 (the exported conversation, its scopes and 'clusions); the core's
`clusion/assert`; a three-way text rebase (new); write capability in futon4a
and the futon6 client (new: both are read/refusal clients today, MAP-Q4/Q6).
