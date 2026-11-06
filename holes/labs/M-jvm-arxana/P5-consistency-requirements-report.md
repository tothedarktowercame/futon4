# P5 — consistency and CRDT requirement audit (report)

Mission: `M-jvm-arxana`. Phase: MAP. Discovery only — no CRDT chosen, no
libraries compared, no collaboration spike built. Every concurrency claim
below is labelled **observed** (an incident or behavior with a citation) or
**hypothetical** (a possible use case with no observed instance).

## 1. Writer inventory (Arxana-backed / WYSIWYG source artifacts)

| # | Writer | Artifact class | Evidence |
|---|--------|----------------|----------|
| W1 | Emacs (Arxana client) | XTDB entities/relations via futon1 HTTP API | `futon4/dev/arxana-store.el:574-713` (`ensure-article`, `ensure-entity`, `create-relation`, batches) |
| W2 | Emacs (single writer of mapped LaTeX sources) | WYSIWYG source `.tex` files | `futon3c/emacs/latex-wysiwyg.el` "single-writer discipline" section (~lines 1032-1057): "Emacs must be the ONLY writer of the mapped sources" |
| W3 | Agents / humans via git and direct file edits | futon4 code, essays `annotations.edn`, source `.md` (canonical, read-only by convention) | `futon4/AGENTS.md` ("Sources of truth are split…", annotations layer); `futon4/README-essays.md:146` |
| W4 | Rebuild pipeline (`rebuild-page.sh` + `latex-wysiwyg-watch`) | derived Oxide HTML + `scopes.json` (re-snapped artifacts) | `futon3c/emacs/latex-wysiwyg.el:1079-1112` (async rebuild, reload map, broadcast `build/done`) |
| W5 | Sync scripts (`arxana-vsatarcs-mission-doc-sync.el`, `arxana-vsatarcs-xtdb-clicks.el`, `arxana-vsatarcs-aif-edn-sync.el`) | XTDB records from staged EDN/files | files exist in `futon4/dev/` (writer-action executors) |
| W6 | VS Code client | — | **none today**; prospective per `futon4/holes/missions/M-jvm-arxana.md` (Gap section) |
| W7 | Browser as an independent writer | — | **none today**: browser edits are relayed through Emacs over websocket and applied by Emacs (W2); refusal cases at `latex-wysiwyg.el:466-495, 532-544` |

No writer today mutates the same XTDB entity or the same mapped source file
concurrently by design; W2's single-writer discipline is explicit prose plus
mechanism (autorevert for unmodified buffers only,
`latex-wysiwyg.el:1050-1057`; Emacs-mediated restore, ~line 1060).

## 2. Current safeguards, freshness, stale, conflict, undo, refusal

**WYSIWYG path** (`futon3c/emacs/latex-wysiwyg.el`):

- *Freshness/stale*: build-time sha256 (first 16 hex) per mapped file in
  `scopes.json`; `latex-wysiwyg-stale-sources` (lines 90-108) compares on
  every edit; stale page ⇒ every edit refused `page-stale` (lines 467-469,
  685-687, 870-872). Emacs's own successful edits re-record their hash
  (`latex-wysiwyg--refresh-sha`, lines 113-126) so only external drift counts
  as stale — this is effectively a **content-hash revision check**, single
  client.
- *Refusal taxonomy*: `edits-disarmed`, `page-stale`, `unmapped-source`,
  `empty-run`, `quote-not-found`, `quote-ambiguous`, `literal-not-found`,
  `atom-not-attributable`, `decomposition-unsound`, `atoms-reordered`,
  `deletion-not-yet-supported`, `macro-text-edited` (lines 466-495, 681-718,
  869-948). "The refusal is the feature: we never guess where an edit
  belongs" (line ~331).
- *Undo*: edits applied through undo-boundaries (lines 495-500, 730-735,
  948-953); restore via buffer so it is a normal undoable change (~1053).
- *Journal*: every attempt, applied or refused, appended as JSONL with old
  and new text (`latex-wysiwyg--journal`, ~lines 974-1020) — replayable
  recovery record.
- *Conflict*: a modified buffer whose file changed externally keeps Emacs's
  changed-on-disk prompt ("A modified stale buffer is a genuine conflict and
  keeps Emacs's prompt", line ~150-153).

**Arxana/XTDB path**:

- Writes are upserts through the futon1 HTTP API with no client-supplied
  expected revision: `arxana-store.el` POSTs entities/relations; futon1
  handlers return 409 only for **model/store invariant failures or failed
  post-write verification**, not for concurrent-modification conflicts
  (`futon1/apps/api/src/api/handlers/graph.clj:266-335`,
  `docbook.clj:50-131`). A grep for optimistic/If-Match/CAS across
  `futon1/apps/api/src` and `apps/graph-memory/src` found none.
- Durability: "XTDB durable proof failed" ⇒ 503, write rejected
  (`graph.clj:275-280`).
- Freshness/offline: `arxana-store-remote-status` health probe with TTL
  cache, statuses `:ok/:down/:disabled`, explicitly "not an authoritative
  guarantee for write availability" (`arxana-store.el:166-200`). Failure
  details recorded client-side (`--record-error`, lines 134-146).
- Subscriptions: no subscription/streaming endpoints found in futon1's API
  routes (grep for sse/subscribe/watch matched only a diag handler);
  freshness is poll-on-demand.

## 3. Observed incidents vs hypothetical use cases

**Observed:**

1. **2026-08-08 — lost updates, out-of-band restore.** A test harness
   restored a snapshot over a session's browser-made edits; "nothing could
   be recovered", which motivated the edit journal. Cited in
   `latex-wysiwyg.el` journalling commentary (~lines 975-984) and repeated in
   the single-writer section (~1034-1039): the restore also raised a modal
   prompt that "blocks the whole daemon, including the websocket server".
   Two concurrent writers (Emacs session + test harness) to one file;
   resolution was last-writer-wins at the filesystem with total loss.
2. **2026-08-08 — stale view.** A page "100 minutes out of date" was edited;
   edits failed as inscrutable `literal-not-found` until the staleness guard
   was added (`latex-wysiwyg.el:90-95` docstring). Stale-read-then-write
   against drifted source.

**Hypothetical (no observed instance found in mission records, tests or
docs searched):** simultaneous online multi-user editing, cursor
co-presence, offline editing with later merge, divergent replicas of XTDB
entities, concurrent agent-vs-Emacs writes to the same entity. The mission
file itself frames the CRDT question as conditional: "The implementation
may eventually need a CRDT, but that choice does not decide the core
design" (`M-jvm-arxana.md`, Operator anchor).

## 4. Requirement matrix

| Scenario | Status | Evidence |
|----------|--------|----------|
| Single writer (Emacs alone) | **Required, supported** | observed daily use; single-writer discipline prose + journal + undo |
| Single writer with stale/derived views | **Required, supported** | sha staleness guard + refusals (observed incident 2) |
| Out-of-band writer bypassing Emacs | **Required to be prevented** (or routed through Emacs) | observed incident 1; autorevert + Emacs-mediated restore are the current safeguards |
| Sequential multi-client (one at a time, e.g. Emacs then VS Code) | **Required (future), unobserved** | mission Gap; nothing in current code exercises it |
| Simultaneous online writers | **Optional / unobserved** | no observed incident or expectation in records; refusals currently arbitrate, not merge |
| Offline writers with later merge | **Optional / unobserved** | no offline queue beyond failure records (`--record-error`); `:down` probe status exists but no replay path was found |
| Cursor co-presence | **Unsupported, unobserved** | websocket broadcast exists (`scope/ack`, `build/done`, remote jump suppression `latex-wysiwyg--suppress`) but no presence protocol |

## 5. Which consistency property can revision-checked transactions NOT supply?

**Finding: none is currently demanded.** Both observed incidents are exactly
the class revision-checking addresses: incident 1 is a lost update caused by
an unguarded out-of-band write (a per-file content revision/lock would have
refused it), and incident 2 is a stale read (revision compare would refuse
earlier and legibly). Neither incident involved two live clients with
legitimate concurrent intent — the second "writer" was a harness bug. Within
the WYSIWYG path, content-hash revision checking is already implemented and
sufficient for every observed requirement. On the XTDB path, writes are
upserts without expected-revision; nothing observed requires more than
adding revision checks there if multi-writer exposure arrives.

## 6. Smallest falsifying observation

The smallest observation that would falsify the sufficiency of
revision-checked transactions: **two clients with legitimate authorial
intent each editing the same artifact concurrently online, where refusing
the second writer's stale attempt destroys real work the author cannot
re-attempt** (i.e. a refusal is no longer an acceptable outcome because
both edits must survive). Equivalently: any observed offline edit session
whose merge cannot be posed as "rebase onto new revision and possibly
refuse". A single such incident, evidenced in the edit journal, is the
trigger the mission's completion condition anticipates ("identify the
observed requirement that would trigger a CRDT decision").

## 7. Unknowns

- Whether any real multi-client session (e.g. VS Code prototype plus Emacs)
  has ever run against the same artifact — none found; unverified beyond
  search.
- Whether the journal file (`wysiwyg-edits.jsonl`) contains other loss
  incidents besides 2026-08-08 — not read (discovery budget; journal is
  per-session and location-dependent).
- futon1 write path below handlers (XTDB transaction internals) not audited
  beyond the API surface; no CAS found at the API layer.

## 8. Q5 ready / missing table

| Item | Status |
|------|--------|
| Writer inventory with citations | **ready** (§1) |
| Freshness/stale/conflict/undo/refusal trace, WYSIWYG path | **ready** (§2) |
| Freshness/refusal trace, Arxana/XTDB path | **ready** (§2; no subscription support found) |
| Observed incident citations, separated from hypotheticals | **ready** (§3; two observed, both 2026-08-08) |
| Requirement matrix with labels | **ready** (§4) |
| Falsifying observation for revision-check sufficiency | **ready** (§6) |
| Multi-client observed evidence | **missing** — none exists to collect |
| Journal corpus review for further incidents | **missing** — future observation, not blocking |

**Principal conclusion: no present CRDT requirement found** — every observed
consistency failure is a single-authority failure mode (out-of-band writer,
stale view) that content revision checks already address, and no legitimate
concurrent multi-writer intent has ever been observed.
