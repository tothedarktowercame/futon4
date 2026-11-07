# Edit robustness correction and resumption checkpoint

Date: 2026-11-07

Status: recorded correction; implementation deliberately paused at the
operator's request.

## Correction

Arxana interaction is not pinned to one immutable byte sequence. Immutable
artifact revisions identify evidence and make answers reproducible; they must
not turn ordinary buffer editing into an interaction refusal. An editor client
must be able to present current bytes as a new/transient revision and ask the
strategy cascade to relocate the same logical scope. A digest mismatch against
an earlier revision invalidates or supersedes old evidence; it does not by
itself mean that the scope is absent.

The 2026-11-07 code-server demo violated this distinction by requiring the
open document to digest-match its original fixture revision before it called
scope resolution. Making the demo document read-only repaired a fixture
accident but did not repair the Arxana contract. That workaround is acceptable
only as a static first demonstration and must not be generalized.

## Evidence already present

### Emacs behavior

`dev/arxana-links.el:697` implements `arxana-links-find-anchor` over the current
buffer, not over an immutable stored snapshot. It cascades:

1. hint offset plus content hash;
2. context-pattern search plus content hash;
3. fuzzy context recovery, which may return moved bounds even when the old hash
   no longer verifies.

`arxana-links-verify-scholium` then updates the scholium to `:anchored` with the
new bounds or `:orphaned`. Emacs text properties elsewhere in the module are a
useful editor-local tracking mechanism, but they are not the portable identity
or authority.

### JVM behavior and the gap

`futon3c.arxana.resolve.anchor/run` ports the same three-stage cascade.
`test/futon3c/arxana/resolve_test.clj` already exercises unrelated prefix/suffix
edits and fuzzy recovery after an inside-anchor edit. Thus relocation logic was
ported.

However, `futon3c.arxana.resolve.evaluator/evaluate` obtains bytes only through
an `ArtifactProvider` under a named immutable revision and intentionally emits
`stale-input` without running finders when supplied bytes disagree with that
revision (`evaluator.clj:310-320`). The missing bridge is therefore not the
anchor algorithm. It is the live-edit protocol that turns current editor bytes
into a new or explicitly transient artifact revision, evaluates the existing
strategy against those bytes, and relates the new result to prior scope
identity/evidence.

## Required resumption work

The next design/implementation pass must specify and test the boundary among:

- editor-local moving markers (for example Emacs text properties);
- portable anchor strategies and their cascaded evidence;
- transient versus durable artifact revisions for unsaved buffers;
- logical scope identity when one old scope is copied into two candidates;
- promotion of a relocated result into durable evidence.

Minimum acceptance matrix:

| Edit | Required behavior |
|---|---|
| Insert before a scope | same logical scope relocates; bounds move |
| Insert after a scope | bounds remain stable |
| Edit inside a scope | exact tiers may fail; fuzzy/unverified result remains usable and visibly qualified |
| Move/cut-paste a scope | context cascade relocates it |
| Copy a scope | all candidates returned as ambiguous; never first-match selection |
| Delete a scope | typed missing/orphaned result |
| Unsaved buffer edit | preview resolution runs against current bytes; no refusal solely because the prior revision hash differs |
| Save/promote | new immutable revision and attributable evidence are minted without rewriting the prior assertion |

No further UI polishing should count as progress on the interactive demo until
this matrix passes across the JVM service and at least one editor client.
