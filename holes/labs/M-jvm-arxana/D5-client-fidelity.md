# D5 — client projections and fidelity contract

Mission: `M-jvm-arxana`. Phase: DERIVE. Inputs: P2/P3, D1–D4 and O-D5a/b.
Status: design checkpoint.

## Decisions

**D5-DR1.** IF Emacs, VS Code and the Oxide pane use different native position
models; HOWEVER authority answers must be identical; THEN clients convert only
at their edge to/from D3 Unicode-codepoint half-open ranges; BECAUSE editor
coordinates are projections, not shared identity.

**D5-DR2.** IF current clients demonstrate valuable but incompatible
behaviours; HOWEVER this mission is not a universal UI/router port; THEN retain
behavioural tripwires while classifying each donor capability preserve, adapt
or drop; BECAUSE fidelity means deliberate disposition, not copying every
transport.

**D5-DR3.** IF typed refusal and journalling prevented observed data loss;
HOWEVER current enforcement is split between Emacs and the browser; THEN adapt
both to D3 authority outcomes while clients retain presentation and recovery
commands; BECAUSE correctness cannot depend on which editor initiated a write.

## Shared client boundary

All clients implement:

```text
open artifact revision
select/project local range → authority codepoint range
resolve scope → display status, candidates, evidence and revision
navigate returned authority range → local editor/view range
query correspondence neighbourhood
show freshness and typed refusal
inspect/export recovery command or evidence
```

Clients own buffers, selections, decorations, panels, keybindings, local
history and coordinate conversion. They do not own entity identity, strategy
execution, correspondence, current-head decisions or promotion of evidence.

Status labels are object-qualified in UI and accessibility text:
`resolution: ambiguous`, `correspondence: stale`, `write: stale-refused`.
The shared words are never displayed without the object whose state they name.

## Minimum surfaces

### Emacs

- Commands: open/pin revision; resolve region/point; next/previous candidate;
  navigate correspondence; inspect evidence; submit guarded edit; recover or
  export refused edit.
- Buffer markers are disposable caches tagged with response and revision IDs.
  Buffer modification invalidates them visually but cannot alter authority
  records.
- Line/column and character positions are converted against the exact buffer
  bytes whose digest equals the named revision; mismatch triggers refresh.

### VS Code

- Commands parallel the Emacs operations; a tree/webview shows candidates,
  cascade evidence and correspondence neighbours.
- Extension converts VS Code UTF-16 positions to Unicode codepoints using the
  exact document revision, explicitly rejecting a boundary inside a surrogate
  pair.
- Decorations carry only response/candidate IDs. No extension-local database
  becomes identity authority.

### Oxide document pane

- Displays the revision-pinned HTML fixture, sends a selected DOM locator plus
  page revision to `correspondence/query`, and navigates returned candidates.
- Build-local DOM IDs remain locators. The pane cannot mint manifestation IDs
  or silently choose among ambiguous mappings.
- Edit affordances appear only for capabilities returned by the authority;
  macro-expanded/partial cases show source navigation or typed refusal.

## Capability Preservation Matrix

| Donor capability | Decision | New owner/form | Tripwire |
|---|---|---|---|
| Exact/context/fuzzy anchor rediscovery | preserve + adapt | D2 authority evaluator over immutable bytes | existing ERT golden cases plus duplicate ambiguity |
| Interactive anchor capture | preserve + adapt | editor captures selection; authority creates revision-pinned declaration | both editors produce equivalent declaration for same range |
| Voiced-link promotion/status | adapt | D2 resolution assertion + attributable acceptance | ambiguous/unverified promotion without acceptance is refused |
| Two-way WYSIWYG navigation | preserve + adapt | D3/D4 queries; clients only project | source→HTML→source returns same manifestation/revisions |
| Typed WYSIWYG edit refusal | preserve | D3 error algebra and recovery data | stale, invalid, unsupported and conflict remain distinct |
| WYSIWYG edit journal | preserve + adapt | authoritative command/outcome journal; client renders/exports | replay is idempotent; crash republish does not reapply |
| Content-hash freshness | preserve + strengthen | content-addressed revisions + expected-head precondition | both P5 incidents and two-writer interleaving |
| Static codepoint span rendering | preserve | D3 canonical locations, live responses replace fixtures | exact spans render identically in scope view and clients |
| File-index source-map lookup | adapt | D4 coherent fixture manifest maps logical artifacts | unknown/mismatched index is typed failure |
| Build-local DOM navigation | adapt | revision-specific locator evidence | DOM ID change does not change manifestation ID |
| Agent chat queue/routing | drop from this client contract | remains Marimo/Agency transport | no Arxana operation depends on SQLite route/revision count |
| Element widget `postMessage`/intent polling | drop as transport; preserve intent IDs as possible inputs | Element remains separate client/producer | intent may enter D6 through shared envelope without polling semantics |
| Emacs salience fields | drop/unsupported | no live canonical reader/writer found | write reports unsupported; no preserved capability reads them |
| Plexus record | drop/defer | prose-only; future mission must define live contract | absent capability is advertised honestly |
| Client-side hyperedge validation copy | drop | server/authority typed validation | invalid edge receives same error from both editors |

No preserved donor behaviour depends on salience or Plexus. Agent routing and
Element messaging are useful exemplars but not editor-projection semantics.

## Protocol transcript equivalence

For one D4 section manifestation, Emacs converts its selected source range and
VS Code converts its UTF-16 selection to the same D3 codepoint range. Both send
the same artifact revision and strategy revision (message/principal IDs may
differ). The authority returns byte-equivalent semantic fields:

```clojure
{:result/status :resolved
 :artifact/revision-id "...draft8-revision..."
 :strategy/revision-id "..."
 :result/candidates [{:candidate/id "..." :location {:unit :unicode-codepoint
                                                       :range {:start n :end m}}}]
 :result/evidence [...]}
```

Each client may render differently, but candidate ID, range, revisions and
evidence digest must match. The Oxide pane queries that candidate's
manifestation and receives the same correspondence assertion ID. Equality is
checked on canonical response fields, never pixels.

## Refusal mapping (O-D5b)

| Current WYSIWYG event | D3 outcome | Required client action |
|---|---|---|
| page/source hash mismatch | `write: stale` | show current/submitted revisions; refresh, rebase or export |
| non-editable macro/generated region | `write: unsupported` | navigate to source or explain capability |
| malformed range/payload | `write: invalid` | retain edit, show field diagnostics |
| competing invariant despite current base | `write: conflict` | show conflicting assertion/constraint |
| accepted edit | `write: applied` with before/after revisions and receipt | update projection from event/snapshot |

Existing journal entries are importable evidence but not authority receipts
unless they name exact before/after revisions and operation identity.

## Client-boundary tests

1. Unicode containing astral characters maps Emacs and VS Code selections to
   one codepoint range; mid-surrogate input is refused.
2. Same D2 request from both clients yields identical semantic response digest.
3. Local buffer edit invalidates decoration but cannot mutate stored result.
4. Ambiguous response renders every candidate; no client auto-selects first.
5. Stale edit produces identical refusal category/recovery choices in both.
6. Disconnect/reconnect resumes events or requests resync; correctness still
   comes from guarded writes, not subscription delivery.
7. Oxide DOM ID changes on rebuild while navigation follows stable
   manifestation plus new locator.
8. Calls to dropped salience, agent-routing or Element-polling behaviour are
   absent from the shared client API.

## Obligations

- **O-D5a closed:** every surveyed donor behaviour has preserve/adapt/drop and
  tripwire disposition; none relies on dropped salience/Plexus.
- **O-D5b closed at design level:** WYSIWYG refusals/journal map to D3.
- D7 must assign extension/webview packaging and the canonical response digest.
- INSTANTIATE must run transcripts against the repaired D4 positive fixture;
  mock responses alone cannot satisfy shared-authority demonstration.

No architecture or UI styling is selected by D5.
