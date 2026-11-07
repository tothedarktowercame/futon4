# MAP Q4 — Projection provenance: which views map a rendered fragment back to graph records and the projection rule

## Answer

Only three of the eight surfaces carry all three recoverables — futon4a, the
fumarimo/Marimo Matrix posts, and the VS Code extension — and all three share
one design: the id/revision is put INTO the rendered artifact at build time.
The Emacs browser and WebArxana can recover record ids but not revisions (the
store client supports `version`/`as-of` and no renderer passes it); Giraffe
records its rule per row but pins revisions only at run-directory granularity;
the 象 turn feed recovers span ids but has no version at all. In every view the
"projection rule" is the rendering code itself — no view names its rule by id
except Giraffe (`reason` per row) and fumarimo (`kind` per event). Summed:
(a) record ids — 7 of 8 views yes; (b) revision — 3 of 8 (futon4a, VS Code,
fumarimo's `revision: 1` + `execution_id`); (c) projection rule — 2 of 8
recorded in the artifact (Giraffe, fumarimo), the rest attributable only by
source file:line of the renderer.

## Evidence

Legend: IaE = implemented-and-exercised, IbU = implemented-but-unexercised,
P = prose-only.

**1. Emacs Arxana browser (`futon4/dev/arxana-browser-*.el`) — IaE** (live
daemon: `emacsclient -e '(featurep ...)'` → `arxana-store` and `arxana-browser`
both `t`, checked 2026-11-07; bootstrap path `futon4/README.md:38-39`).
(a) YES: items carry record ids and open actions re-fetch by id — e.g.
`arxana-browser-chorus.el` chorus-open reads `:entity-id` from the item and
calls `arxana-store-fetch-entity entity-id` (arxana-browser-chorus.el,
`defun arxana-browser-chorus-open`; ids minted in
`arxana-browser-hypergraph.el:963,995`). (b) NO at render time: the browser
calls `fetch-entity` bare; the version/as-of parameters exist in the client
(`arxana-store.el:879` `(defun arxana-store-fetch-entity (id &optional version
as-of))`) but no arxana-browser-* renderer passes them — a fragment maps to
the CURRENT entity, not the one rendered. (c) DROPPED: rows are plain vectors
(`arxana-browser-hypergraph-row` renders label/thread/nodes/edges/status/path
— the id survives only on the item plist, not in the displayed text), and no
rule id exists; the rule is the .el function.

**2. futon4a (`futon4a/lisp/futon4a-arxana.el`, 274 lines) — IaE** (demo
client; exercised by `futon4a/test` and the committed demo transcript, per
`futon6/holes/M-jvm-arxana-codeserver-demo-report.md` "strategy revision copied
from the committed futon4a demo transcript"). (a) YES: every candidate printed
with `candidate/id` (futon4a-arxana.el:126-133). (b) YES:
`artifact/revision-id` and `strategy/revision-id` are printed (line 123-124)
and cached (`futon4a-arxana--last-revision-id`, lines 40-41, 138). (c) PARTIAL:
the wire protocol `arxana.v1` operation names the operation, and the renderer
(`futon4a-arxana--render-resolve`, line 115) is the rule — displayed, not
recorded as an id.

**3. 象 turn feed (`futon3/src-cljs/futon3/turnfeed/core.cljs`) — IaE**
(live page /var/www/zone.hyperreal.enterprises/wip/turns/feed-all.json, 120
turns, refreshed by systemd `wip-scope-ledger.service` ExecStart #2 running
`turn_margin_html.py --latest 120`). (a) YES: note ids are minted
turn-qualified — `fragments()` gives `f"{prefix}n-{sentence['id']}-{i}"`
(turn_margin_html.py:90-102), the DOM carries them (`:id id` on the note card,
core.cljs:351), and they resolve to `~/.emacs-graph/session-turn-analysis/
turn-*.analysis.json`. (b) NO: the feed is a regenerated JSON file with no
version or digest per turn (conditional GET only, core.cljs:296-314 comment).
(c) DROPPED: rule = core.cljs `note-card`/`turn-block` + the generator; no id.

**4. Giraffe (`futon3c/holes/labs/giraffe-observation-series/` + rendered
runs under `storage/giraffe/<run>/`) — IaE** (run 20261107T075606.546Z-c0a9ec59
read live for this survey). (a) YES: report.md rows carry the item id
(M-/E-/T-). (b) RUN-GRADE ONLY: the run directory name carries the input sha
prefix and manifest.edn pins sha256 of every artifact (input/pass/rejected/
work-clusters); per-item revision exists as `:stale-markers` and
dependency-reading `:record-pin` (batch sha + line) inside pass.edn entries —
recoverable, but not surfaced in report.md. (c) YES, uniquely: each row carries
its `reason` (`ready-by-unblocks`, `not-ready-by-source-order`, …), i.e. the
projection rule per fragment. The observation-series lab adds
provenance-carrying proposals: "operator evidence ID, exact quote, clocked
work, discussed work, and turn-record ID" per proposal
(giraffe-observation-series/README.md Step 1), with resolution acts requiring
`resolution-evidence-id` (Step 2).

**5. WebArxana (`futon4/dev/web/webarxana/`) — IaE** (rendered in the futon3c
JVM per MAP-Q10 review note `8941b39`). (a) YES: nodes key on `:nema/id` /
`:entity/id` (graph.cljs:874-886 `node-component`; state.cljs:364-378
normalises ids). (b) NO: `pin-entity!`/ego fetches carry no version
(api.cljs `pin-entity!`, GET `/ego/<name>?fold=1&depth=1`); a
`:version` numeric prop is read for essays only (state.cljs:319
`essay-version`). (c) DROPPED: the 9-stage pipeline is documented
(README-rendering-pipeline.md stages 1-9) but positions are layout-computed
per render tick (`:_render-tick`); the artifact records neither stage ids nor
a digest of which pipeline instance ran.

**6. Element/Matrix bridge posts (`futon3c/scripts/matrix_bridge.py`,
`fumarimo_agent.py`) — IaE** (unit tests `futon3c/test/matrix_bridge_test.py`;
live posts observed in rooms). Best of the eight: the fumarimo python-cell
content embeds an extensible-event namespace with `kind: "python-cell"`,
`cell_id`, `request_event_id`, `revision: 1` (fumarimo_agent.py:109-126), and
outputs embed `request_event_id`, `cell_event_id`, `cell_id`, `execution_id`
(image_output_content fumarimo_agent.py:131-163, value_output_content:164+;
wired in matrix_bridge.py `_emit_success_reply` 274-312). So (a) YES (Matrix
event ids), (b) YES at execution granularity — `revision` is a constant 1 and
the real version story is `execution_id`; no store revision is claimed. (c)
YES: `kind` names the projection. Ordinary (non-fumarimo) bridge replies carry
only Matrix's own `m.relates_to` reply linkage (matrix_bridge.py `_say`).

**7. Marimo notebooks (`marimo-zone/notebooks/`) — IaE but URL-parameter
provenance**: `matrix-room-posts.py` reads `mo.query_params()` `room`/`event`
and falls back to "unknown room"/"unknown event" (matrix-room-posts.py,
second cell) — provenance is a URL parameter, silently degraded when absent;
no revision, no rule id.

**8. VS Code / code-server (`futon6/tools/arxana-vscode/`) — IaE** (18/18 node
tests + live acceptance per M-jvm-arxana-codeserver-demo-report.md).
(a) YES: decorations carry candidate IDs only; (b) YES, the strongest in the
stack: `lib/digest.js` digest-matches the open document's exact bytes to
committed revision ids and refuses unmatched bytes rather than guessing a
revision; (c) PARTIAL: typed envelope operations name the interaction, no rule
id (same shape as futon4a).

**Doubt 1 — is `arxana-store.el` called by a live Emacs flow?** YES. Loaded in
the running daemon (featurep → t, above); 31 dev files require it (grep
`arxana-store` over futon4/dev: arxana-links 28, arxana-media 29,
arxana-saving 15, all arxana-browser-* viewers, bootstrap.el:109,130). It is
the sole HTTP client the browser views use.

**Doubt 2 — futon5 "exotype" wiring-diagram material.** It lives in
`futon5/holes/labs/M-aif-tokamak/`: `exotype-*.png`,
`exotype-port-census.{csv,README.md}` — generated by
`futon5/scripts/exotype_ports.clj` from the coordinate system in
`mathlib4/DarkTower/MetaCAExample.lean` (exotype-port-census.README.md header);
plus the MMCA wiring diagrams proper under `futon5/data/wiring-rules/*.edn`
(hybrid-110-*.edn …), the catalog target of
`futon5/holes/missions/M-fulab-wiring-survey.md`.

## Ready — no new code / Missing — actual work

| Surface | Ready (no new code) | Missing (actual work) |
|---|---|---|
| Emacs browser | item→id re-fetch exists; client version/as-if support exists | renderers pass no `version`; rows drop ids from display; no rule id |
| futon4a | ids+revisions rendered and cached | rule id not in artifact (operation name only) |
| 象 turn feed | turn-qualified span ids minted + carried in DOM | no per-turn version/digest; generator has no rule id |
| Giraffe | per-row `reason`; run-dir + manifest sha pins; observation proposals carry evidence/quote/turn ids | per-item revision not surfaced in report.md |
| WebArxana | node ids; 9-stage pipeline documented | no version on ego/pin fetch; no pipeline/rule id in artifact |
| Matrix bridge | fumarimo namespace: kind, cell/execution ids, request event | `revision` constant 1 (execution_id is the real version); plain replies carry no namespace |
| Marimo | query-param room/event | silent "unknown event" fallback; no version, no rule |
| VS Code | digest→revision match, typed refusal, candidate-id decorations | rule id not named (operation name only) |

## Open doubts

- Whether Giraffe's pass.edn `:record-pin` (batch sha + line) is durable
  enough to count as a per-item revision for MAP's Manifestation vocabulary,
  or whether it needs promotion to a first-class field.
- Whether the Emacs browser rendering CURRENT entity on re-open has ever
  bitten in practice (a fragment silently changing under a stale render);
  no incident record was searched for this survey.
- fumarimo's `revision: 1` reads like a placeholder for future cell edits; if
  Marimo cells become editable, the constant must become real history.
