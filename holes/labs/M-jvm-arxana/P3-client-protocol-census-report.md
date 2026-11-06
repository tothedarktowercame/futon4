# P3 — client and protocol census report

Mission: `M-jvm-arxana`. Phase: MAP. Discovery only: no client was changed, no
package installed, no server restarted, nothing published. Every claim below
comes from reading local source files, cited per row.

## 1. Comparison table

| Surface | Transport | Request/event names | Identity fields | Revision / freshness | Selection / navigation | Error / refusal shapes | Authority |
|---|---|---|---|---|---|---|---|
| Emacs ↔ LaTeXML WYSIWYG (`futon3c/emacs/latex-wysiwyg.el`, ws server on port **7079**, deliberately not the Agency 7070 socket) | WebSocket, JSON text frames; Emacs is the *server*, browser(s) are clients; broadcast to all clients | inbound: `hello`, `ping`, `scope/point`, `scope/edit`, `scope/edit-para`, `scope/edit-atom`, `scope/delete-atom`; outbound: `pong`, `state`, `scope/point`, `scope/ack`, `scope/reject`, `build/done` | `file` = integer sourcepos file index into the map loaded from `scopes.json` (`src-dir`, `files` vector); `line`/`col` 1-based; `version`/`ua` on `hello` | per-file SHA-256 prefix (16 hex) recorded in `scopes.json` as `source-sha`; `latex-wysiwyg-stale-sources` detects external drift; `latex-wysiwyg--refresh-sha` re-arms after our own edit; edit refusals `page-stale`, `map-stale` | browser click → `scope/point` moves Emacs point; Emacs idle timer broadcasts `scope/point` back (echo suppressed via `latex-wysiwyg--suppress`) | typed string `reason` in `scope/reject`: `edits-disarmed`, `page-stale`, `unmapped-source`, `empty-run`, `quote-not-found`, `quote-ambiguous`, `map-stale`, `synthesised-source`, `no-such-file`, `literal-not-found`, `atom-not-attributable`, `decomposition-unsound`; journal records applied *and* refused edits (refusals keep the attempted text) | **Emacs**: it owns the file map, the staleness check, the literal-matching regexp, and refuses rather than guesses |
| TypeScript scope view (`futon3c/web/scopes/scopes.ts`, mounted by `futon3c/web/scopes/index.html`) | none of its own — `index.html` does `fetch('./fixtures/over-time-<session>.json' | './fixtures/<turn>.json')` and hands the parsed `ScopeData` to `renderScopes` / `renderGrid` | data vocabulary, not messages: `segments` (`id`,`role`,`label`,`text`), `layers` (`id`,`name`,`ink` c/m/y), `spans` (`id`,`seg`,`start`,`end`,`layer`,`kind`,`mark`,`weight`,`note`), `edges` (`from`,`to`,`relation`,`label`), `legend` | `seg` = segment id string; span/segment ids are opaque strings | none — the JSON document *is* the snapshot; `auditScopeData` reports `invalidSpans` (offsets outside segment), edge `stubs`, legend gaps instead of fetching newer data | click/hover on pieces and anchored units; `jumpTo(spanId)` scroll+light; layer toggles via `setLayers` | validation, not refusal: `ScopeAudit {invalidSpans, stubs, legend}`; invalid spans simply don't render | **the JSON fixture document**; the view is read-only and self-auditing |
| Marimo agent chat (`marimo-zone/marimo_chat/{send.py,assignment.py,static/chat.js}`) | HTTP from `chat.js` to the marimo server (`../send`, `../assignment`, `../agents`); server-side `urllib` to Agency `POST /api/alpha/invoke-stream` and `POST /api/alpha/evidence` | `POST ../send` `{file, revision, text}`; `GET/POST ../assignment` `{file, agent, revision}`; `GET ../agents`; Agency payload `{agent-id, prompt, surface:"marimo", caller, turn-id}`; evidence `forum-post` observation with `transport:"marimo"` | `file` = notebook basename matching `chat-*.py`; `agent` string id; `turn-id` minted `t-YYYYMMDD-HHMMSS-hex`; user from `X-Marimo-Chat-User` header (Caddy-set) defaulting to `joe` | **optimistic concurrency**: SQLite `.chat-routing.sqlite3` `assignments(file, agent, revision)`; stale client `revision` → HTTP **409** "Notebook assignment changed"; accepted turns keep their original agent | no document navigation — the chat bar floats in the notebook DOM (`chat.js` reconciles on `marimo-filename` textContent) | typed HTTP statuses from `HTTPException`: 400 invalid JSON/text, 401 unauthenticated, 403 cross-site (`sec-fetch-site`), 404 unknown notebook, 409 assignment changed, 422 agent not in roster, 500 no directory workspace, 502 Agency roster unreadable; invoke failures logged to `.chat-turns.jsonl` (`ok:false`) | **Agency** for turns (durable queue via `invoke-stream`); **marimo server** for routing/auth; evidence writes never gate delivery (failure logged and dropped) |
| Element / FUTON (`element-web-futon/apps/web/src/customisations/FutonIntentMarks.ts`, `FutonAnnotations.ts`, `FutonMarimoCell.tsx`) | HTTPS `fetch` to same-origin `/chat-api/...`; `postMessage` to `/xiang-widget/` iframes; `<iframe src="/marimo/...">` | `GET /chat-api/api/xiang/turns?limit=300` (10 s poll per room); `GET /chat-api/api/xiang/turns/{turnId}`; `POST /chat-api/api/marimo/session` `{room_id}`; postMessage types `futon.select-event`, `futon.highlight-intent`, `futon.annotation-theme` | Matrix `eventId` ↔ turn summary `id` ↔ `evidence-id`; `roomId` in `X-Matrix-Room` header; `Bearer` Matrix access token; span identity = codepoint `[start,end)` + verbatim `text` into `source_text` | freshness by **status polling**: `analysis-status` from requested → final set `{analyzed, refused, failed, not-requested}`; marks only applied when `analysis.source_text === source` (or `draft.status === "drafted"` with matching source); no revision counter | hover a marked cue → `highlightFutonAnnotationIntent(eventId, intent)` into the annotation widget; marks are DOM-underlines placed only on exact text match (`exactMark` re-verifies offsets and length caps: 80 codepoints / 8 words / no newline) | silent degradation: failed polls are swallowed (`catch(() => undefined)`); invalid cues/marks are filtered out, never surfaced as errors; Marimo cell shows "Join this Matrix room to use its notebook" on auth failure | **the xiang turns API** for analysis, **Matrix** for auth; the client itself is the validator — anything that doesn't verify against `source_text` is dropped |
| VS Code extension / webview | — | — | — | — | — | — | **absent**; see §4 |

## 2. Source-cited example messages

**Emacs WYSIWYG, inbound navigation** (`latex-wysiwyg.el`, `latex-wysiwyg--on-message` / `latex-wysiwyg--goto`):

```json
{"type": "scope/point", "file": 2, "line": 41, "col": 8}
```

**Emacs WYSIWYG, edit refusal** (`latex-wysiwyg--refuse-edit`):

```json
{"type": "scope/reject", "reason": "page-stale", "file": 2}
```

**Emacs WYSIWYG, journal record of a refused edit** (attempt text preserved):

```json
{"applied": false, "reason": "quote-not-found", "index": 2,
 "kind": "run", "pairs": [{"old": "the original words", "new": "the new words"}]}
```

**Emacs WYSIWYG, hello handshake** (`latex-wysiwyg--on-message`):

```json
{"type": "hello", "version": "…", "ua": "Mozilla/5.0 …"}
```

**Scopes view, one span** (`scopes.ts`, `ScopeSpan`; offsets are Unicode
code-point indices, half-open, into `segment.text`):

```json
{"id": "s7", "seg": "turn-3", "start": 12, "end": 31,
 "layer": "intent", "kind": "request", "weight": 0.8}
```

**Marimo chat, send** (`chat.js` submit → `send.py send_chat`, reply 202):

```json
// request
{"file": "chat-arxana.py", "revision": 3, "text": "please continue the census"}
// response 202
{"turn-id": "t-20261106-101530-a1b2", "agent": "claude-4", "revision": 3}
```

**Marimo chat → Agency** (`send.py invoke_stream`):

```json
{"agent-id": "claude-4", "prompt": "Surface: marimo chat notebook …",
 "surface": "marimo", "caller": "joe", "turn-id": "t-20261106-101530-a1b2"}
```

**Element intent marks, turn summary + cue** (`FutonIntentMarks.ts`,
`TurnSummary` / `Fragment`/`Cue`):

```json
{"turns": [{"id": "turn-Q1x0eR", "evidence-id": "$abc…", "analysis-status": "analyzed"}]}
{"intent": "request", "target": "zai-4", "display_cues":
  [{"start": 4, "end": 12, "text": "please", "label": "request", "method": "lexical"}]}
```

**Element → annotation widget** (`FutonAnnotations.ts`):

```json
{"type": "futon.highlight-intent", "eventId": "$abc…", "intent": "request"}
{"type": "futon.annotation-theme", "theme": "dark", "fontFamily": "…",
 "foreground": "#…", "background": "#…"}
```

**Element → Marimo session** (`FutonMarimoCell.tsx`):

```json
// POST /chat-api/api/marimo/session, Authorization: Bearer <matrix token>
{"room_id": "!room:matrix.org"}
// then iframe /marimo/?file=matrix-room-posts.py&room=<id>&event=<id>&authors=["joe"]
```

## 3. Verified overlaps and collisions

Common fields that genuinely recur across surfaces:

- **`file`** (Emacs, Marimo), **`text`/`source_text`** (Emacs old/new, scopes
  segments, Element marks), **`line`** (Emacs), **codepoint half-open
  `[start,end)` offsets with verbatim text re-verification** (scopes spans;
  Element cues/fragments — both `Array.from(text)` based), **`agent`/`agent-id`**
  (Marimo client and server), **status strings as freshness** (Element
  `analysis-status`; Emacs `armed` state flag), **typed refusal reasons**
  (Emacs reasons; Marimo HTTP codes).

Collisions — same name or shape, incompatible meaning:

1. **`file`** is an *integer sourcepos index* in the WYSIWYG protocol but a
   *filename string* (`chat-*.py`) in Marimo chat, and a query parameter for
   the Marimo iframe (`matrix-room-posts.py`). A merged protocol needs one
   disambiguated referent.
2. **`revision`** exists only in Marimo chat, where it counts *assignment
   changes* (SQLite counter), not document content versions. Emacs freshness
   is content hashing (`source-sha`); Element freshness is status polling with
   `source_text` equality. Three unrelated freshness regimes.
3. **Turn identity**: Marimo mints `t-<timestamp>-<hex>` turn-ids locally;
   Element keys turns by Matrix `eventId` *and* `evidence-id` *and* xiang
   `turn.id`; the WYSIWYG protocol has no turn concept at all (only
   last-jump/last-edit singletons).
4. **Error philosophy is incompatible**: the WYSIWYG protocol *refuses with a
   typed reason and journals the refused text*; Marimo returns *HTTP status
   codes* and records outcomes in a JSONL ledger; Element *silently drops*
   anything that fails verification (failed polls swallowed, invalid cues
   filtered). Element's "refused" is an `analysis-status` value, not a
   transport error.
5. **Authority differs per surface**: Emacs is the refusing authority for
   edits; Agency is the serialising authority for Marimo turns; the xiang
   turns API + client-side verification is authority in Element; the JSON
   fixture is authority in the scopes view. No surface delegates.
6. **Offset semantics agree between scopes.ts and Element marks (codepoints,
   half-open) but differ from WYSIWYG positions (file/line/col, 1-based)** —
   the two position vocabularies have no bridge today.

## 4. Q3 ready / missing table

| Capability | Status | Evidence |
|---|---|---|
| Two-way document↔client navigation (WYSIWYG) | **ready** | `latex-wysiwyg.el` S1 |
| Guarded client-initiated edits with typed refusals + journal | **ready** | S2/S3 in same file (`scope/edit`, `scope/edit-para`, atom ops) |
| Page staleness detection (content hashing) | **ready** | `latex-wysiwyg-stale-sources` |
| Codepoint-accurate span annotation rendering | **ready** (two independent implementations) | `scopes.ts` `splitPieces`/`auditScopeData`; `FutonIntentMarks.exactMark` |
| Agent-chat turn submission with durable queuing | **ready** | `send.py` `invoke_stream` → Agency `/api/alpha/invoke-stream` |
| Optimistic-concurrency agent routing | **ready** | `assignment.py` SQLite `routing()` |
| Evidence recording of every turn | **ready** | `send.py record_evidence` (best-effort) |
| Cross-client annotation-intent protocol | **ready** (Element↔widget only) | `FutonAnnotations.ts` postMessage types |
| Shared document/position vocabulary between WYSIWYG (file/line/col) and span views (codepoint offsets) | **missing** | §3.6 above; no converter exists in any repo |
| Uniform refusal/error surface across clients | **missing** | §3.4 |
| Turn identity shared between Marimo, Element/xiang, and evidence store | **missing** (Element has the eventId↔turn-id↔evidence-id join; Marimo turn-ids join to nothing outside `.chat-turns.jsonl`) | §3.3 |
| Live (non-fixture) scope-data transport | **missing** — scopes view fetches static JSON fixtures only | `index.html` fetch of `fixtures/*.json` |
| VS Code extension or webview client | **missing entirely** | §5 searches |
| Scope data produced from Arxana/XTDB rather than fixtures | **missing** (out of P3 scope; P2/P6 packets cover) | — |

## 5. Search commands supporting claimed absences (VS Code work)

Run 2026-11-06 from `/home/joe/code`:

```
find /home/joe/code -maxdepth 2 \( -iname '*vscode*' -o -name '.vscode' \
  -o -iname '*vs-code*' -o -iname '*code-oss*' \) -not -path '*/node_modules/*'
# → only editor-config dirs: chipwits-forth/.vscode, codex/.vscode,
#   mathlib4/.vscode, lean4/.vscode, xtdb/.vscode, xtdb-wt-literal-slots/.vscode,
#   mathlib4-latest/.vscode, xtdb-wt-cancel-checks/.vscode

grep -rli 'webviewview\|vscode.ExtensionContext\|vscode.window.registerWebviewPanel' \
  /home/joe/code --include='*.ts' --include='*.js' --exclude-dir=node_modules -m1
# → no output

grep -rl 'vscode\.' /home/joe/code --include='*.ts' --exclude-dir=node_modules -m1 \
  | grep -vi 'lean\|mathlib\|xtdb\|chipwits'
# → marimo-zone/.venv/.../marimo/_lsp/copilot/api/types.d.ts  (vendored marimo
#   LSP typings inside a virtualenv — not local extension work)

find /home/joe/code -maxdepth 4 -name 'extension.js' -path '*extension*' \
  -not -path '*node_modules*'
# → no output
```

Honest absence: **no VS Code extension, webview, or VS Code-adjacent client
work relevant to Arxana, scopes, agent chat or document viewing exists in any
local repo.** The `.vscode` directories found are editor settings for Lean and
XTDB checkouts, not extensions.

## 6. Marimo and Element provenance (acceptance)

- Marimo agent-chat lives at **`/home/joe/code/marimo-zone`**, package
  `marimo_chat` (`send.py` 167 lines, `assignment.py` 108 lines,
  `static/chat.js`, `static/chat.css`), with tests and tools under
  `marimo-zone/tests/` and `marimo-zone/tools/` (e.g. `execute-code.sh`).
- Element/FUTON integration lives at **`/home/joe/code/element-web-futon`**
  (branch `futon`), customisations in
  `apps/web/src/customisations/`: `FutonIntentMarks.ts` (+test),
  `FutonAnnotations.ts` (+test), `FutonMarimoCell.tsx` (+test), plus
  `apps/web/futon/` and `apps/web/scripts/build-futon.sh`.

Principal finding: four working but mutually unintelligible client protocols
exist — integer-file/line/col WebSocket WYSIWYG with typed refusals, static
codepoint-span JSON in the scopes view, revision-counted HTTP agent routing in
Marimo, and status-polled eventId-keyed marks in Element — and the only field
families they genuinely share are codepoint half-open offsets with verbatim
text verification and a `file`/`text`/`agent` vocabulary whose meanings
collide at every boundary, while no VS Code client exists at all.
