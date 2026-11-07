# MAP-Q2 — 象 source-to-intent path

**Surveyed:** 2026-11-07 by kimi-1 for M-formal-arxana MAP (requisition from claude-13).
Read-only: futon1b GETs on :7073, file reads; no writes to any live store.

## Answer

The path from a source turn to an interpreted move is five records today:

1. **Source turn evidence** — one `:coordination` entry, body `{:event "chat-turn" :role "user" :turn-id "<agent>-turn-N" :text …}`, id `emacs-<md5>` (e.g. `emacs-4ead27b5ce52ee1d57f5f3cacf49878d`), `:evidence/origin {:kind :operator :actor "joe" :writer "agent-chat/turn"}`. Every stored Joe turn carries claim-type `:question` regardless of content (already noted in M-象-2000 MAP Q2; still true of both traced turns).
2. **Turn record** (canonical store: `~/.emacs-graph/session-turn-analysis/turn-<id>.json`, id `turn-aKK07D`) — source_text, codepoint sentence spans, `analysis_dispatch` job id, `evidence_id` back-link, `acts` (`<turn>-f-s<n>-<i>`), `happened_summary`, `pattern_verdicts`, `ports`, `standings`.
3. **Classical draft** — `turn-<id>.json.draft.json` from `scripts/xiaoxiang_preview.py` (小象); Emacs mirror `futon3c/emacs/xiaoxiang-preview.el`.
4. **象 model reading** — `turn-<id>.json.analysis.json`: `method: agent-interpretation`, `labeller: 象-sonnet`, `interpretation_version: 3`, `vocabulary_version: 3`, `human_approved: false`, `source_sha256` pin; sentences → fragments with `intent`, `target`, `rationale`, `relations`, `display_cues`, `pattern_refs`/`pattern_rejections` over a 25-intent vocabulary (table in `turnfeed/core.cljs` `intent-mark`, glosses in `reply-proforma.md`).
5. **Settled-turn evidence** — `e-xiang-turn-<record-id>`, type `:memory`, claim-type `:assert`, author `xiang-turn-service`, `in-reply-to` the source chat-turn entry; body = record reference with sha256/char witnesses + the full reading (`turn_service.clj` `turn-evidence-entry`, l.427). Side records: `e-xiang-relationship-<sha1>` proposals and `hx-xiang-scope-<sha1>` hyperedges (`labeller "rule"`, status `proposed`).

**Reply side (the marks).** Agent replies carry proforma marks ㊥㊢㊯… (`~/code/reply-proforma.md`; the mark↔intent map is `turnfeed/core.cljs` l.56–61). A separate **馬-sonnet alignment reading** (`futon3c/xiang/ma_reading.clj`) maps each reply paragraph to the operator act it answers, storing `{:act … :verdict … :mark …}` on the record's `:alignment`. Marks are speech-act *declarations by the replying agent*, parsed downstream by `turn_acts.clj` (`mark-kind`, l.84); a different, smaller mark channel (✘ ✓ 💡 on turns themselves) is minted as evidence tags by `futon3c/marks.clj`.

**Interpretation authority.** Nowhere is a reading itself authoritative. The 象 label is a versioned, rejectable translation (象/翻译契约, 象/释义非授, `futon3/library/象/释义非授.flexiarg`: "解读…自己从不创造、终止或转移承诺"); `human_approved: false` on every traced analysis. Institutional effects require a stamped act — `:act/stamp {:executor :signer :authority}` validated against grants (M-象-2000 DERIVE-2 item 1; 象/名分有据) — and the 403-with-no-write refusal is exercised (`http.clj` l.10442 `:not-xiang`, `xiang-provisional-grant-id` l.10504; E-elephant demo step 5 unrun live). Provisional effects from a `withdraw` reading are a separate harness write under a scoped grant with `:provisional-only true` (M-象-2000 l.454–456), not a consequence of the label.

## Evidence

- Source chat-turn, WITH mark in reply: `GET :7073/api/alpha/evidence/emacs-4ead27b5ce52ee1d57f5f3cacf49878d` → `turn-id "claude-13-turn-1"`, session `ed69fb38-e76d-4009-8bc7-880ea8a99bf8`, at 2026-11-07T17:42:17Z.
- Its record/analysis: `~/.emacs-graph/session-turn-analysis/turn-aKK07D.json` (acts `claude-13-turn-1-f-s1-0` kind `report`, alignment seat `馬-sonnet` verdict `noted` mark `㊥`) and `turn-aKK07D.json.analysis.json` (`labeller 象-sonnet`, fragment intent `report`, target `M-formal-arxana mission doc`).
- Its settled evidence: `GET …/e-xiang-turn-turn-aKK07D` → type `:memory`, in-reply-to the source entry, `:record {:source-text-sha256 "d7835e2a…" :interpretation-version 3}`, full `:reading` embedded.
- Source chat-turn, NO leading mark in reply: `emacs-1fcffdd12a3b63b2fccca1f2cf9d8b38` → `turn-id "claude-13-turn-2"`; record `turn-DmpC3a.json` — reply_text begins "Dispatched; checking…" with ㊥ only mid-reply; alignment still assigns `㊢` to act `claude-13-turn-2-f-s1-0` with `method: model` (the 馬 reading infers the mark when the glyph is absent). Settled evidence `e-xiang-turn-turn-DmpC3a` present.
- Pipeline orchestration: `futon3c/xiang/turn_service.clj` ns doc (states requested→analyzed/refused/failed; seat defaults l.63–80: primary `象-sonnet`, Kimi pool `象-1..4`); evidence wiring l.380–470.
- Record schema / pure parser port: `futon3c/xiang/turn_record.clj` ns doc (offsets `unicode-codepoints-zero-based-end-exclusive`).
- Mark→intent table: `futon3/src-cljs/futon3/turnfeed/core.cljs` l.56–61 (`intent-mark`); reply proforma spec `~/code/reply-proforma.md` ("mark honestly… 🖖 is not a mark").
- Alignment: `futon3c/xiang/ma_reading.clj` l.2–45 (result stored as `:alignment` version 1).
- Authority: `futon3/library/象/释义非授.flexiarg` (IF/HOWEVER/THEN/BECAUSE); `M-象-2000.md` l.444 ("an interpretation id is refused as authority (象/释义非授)"), l.454–456 (three records: interpretation, provisional effect, confirmation); `futon3c/src/futon3c/transport/http.clj` l.10442 (403 `:not-xiang`), l.10504 (provisional grant id).
- Operator-mark channel: `futon3c/src/futon3c/marks.clj` l.1–45 (✘✓💡 core, second-string tier; minted as tags on chat-turn/turn-round evidence).

## Ready — no new code | Missing — actual work

| Ready — no new code | Missing — actual work |
|---|---|
| Full source→reading chain exists live and is exercised on every Emacs-REPL turn: chat-turn evidence → turn record → 小象 draft → 象 model analysis → settled `e-xiang-turn-*` evidence with sha256 witnesses | No authoritative "this turn meant X" record type: readings carry `human_approved: false` forever; there is no confirmation/assessment object that upgrades or rejects a reading (operator confirmation exists only as prose + the provisional-withdrawal grant path) |
| Interpretation authority is already separated by design (释义非授) and enforced at the act boundary (stamp + grant validation, 403 no-write) | No link from a reading to the acts it later grounds: an `:act/withdrawal` cites `:interpretation-id`/version, but no reverse index or as-of query joins reading versions to effects (DERIVE-2 lists "待复核" but no implementation found — searched `grep -rn "interpretation-id\|interpretation_id" src/`, only the withdrawal payload) |
| Versioned vocabulary (`vocabulary_version: 3`) and interpretation version pinned per analysis; source text pinned by sha256 | The 25-intent vocabulary lives in three places that must agree by hand (`turnfeed/core.cljs`, `xiaoxiang-preview.el`, reply-proforma.md); no single versioned vocabulary artifact found (`vocabulary-from-json` reads a file, but the table is code) |
| Two independent interpreters (classical 小象 draft; model 象/馬 seats) write to the same record, so disagreements are co-recorded | No systematic assessment of interpreter agreement; pattern_verdicts and alignment verdicts are per-turn, with no cross-seat scoring (E-elephant "Open" notes disagreements must be counted, not treated as failures — no counting code found: `grep -rn "disagree" src/futon3c/xiang/` matches only intent strings) |
| Mark vocabulary is small, enumerable, and already dual-channel (reply proforma; ✘✓💡 turn marks) | Reply-mark parsing depends on agents volunteering glyphs; the traced no-mark turn shows 馬 inferring `㊢` by `method: model` — inferred marks are not distinguished downstream from declared ones except in the alignment map itself |

## Open doubts

- Whether `e-xiang-turn-*` (type `:memory`, claim-type `:assert`) is the right evidence shape for readings — an assert by `xiang-turn-service` flattens "a model read a turn" into a claim; no dedicated claim-type exists.
- `human_approved` appears writable only by hand-editing the analysis file; no route found that flips it (`grep -rn "human_approved" src/ emacs/` → writer only in the Python/Elisp originals).
- Scope hyperedges (`hx-xiang-scope-*`, status `proposed`, `labeller "rule"`) are written but no reader was traced this session; possibly consumed only by the Emacs scope overlay (`xiang-scope-overlay.el`).
- The `turn-<6char>` record id namespace (random, per-installation) vs the deterministic `e-xiang-turn-<id>` evidence id: collision/merge behavior across stores not checked.
