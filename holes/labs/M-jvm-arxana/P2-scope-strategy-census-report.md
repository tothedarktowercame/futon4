# P2 — scope strategy and cascade census (report)

Mission `M-jvm-arxana`, phase MAP, packet
`packets/P2-scope-strategy-census.md`. Discovery only: nothing was ported,
refactored, generalized, or executed against production stores. All line
numbers cite the working tree at the time of writing.

## Principal finding

**The strategy layer is pure stored data with no executor.** Finder
declarations (`:symbol-as-term`, `:filename-mention`, `:embedding-proximity`,
`:explicit`, `:surface-form-hybrid` — `dev/arxana-links.el:265`) are
persisted as JSON-serialized plists and loaded back, but no code in `dev/`
or `test/` ever dispatches on a finder's `:type` to compute a link. The only
genuine *cascade* in the codebase is the anchor re-finding cascade in
`arxana-links-find-anchor` (`dev/arxana-links.el:697`), which is
algorithmically pure (hash + string search) despite living on Emacs
buffers. Porting therefore splits cleanly: a data layer (strategies, links,
surface forms, scholia — portable as-is), one cascade algorithm (portable
with a string adapter), and interactive capture/promotion plumbing
(Emacs-intrinsic).

## 1. Strategy grammar as it exists

### Link strategy (Tier 1)

Constructor `arxana-links-make-strategy` (`dev/arxana-links.el:289`).
Stored fields:

| Field        | Meaning                                        | Notes |
|--------------|------------------------------------------------|-------|
| `:xt/id`     | `"strategy:" + md5(repo, finders)` (deterministic) | line 257, 302 |
| `:type`      | literal `"arxana/link-strategy"`                | |
| `:scope`     | `(:repo string :code-roots (strings) :docbook string)` | docstring line 295–298 |
| `:finders`   | list of finder plists, below                   | |
| `:created-at` / `:created-by` | ISO timestamp / `user-login-name` | |

Finder plist keys: `:type` (one of the five known types, line 265),
`:auto-link?` (boolean; constraint-checked in
`arxana-data-constraints--finder-ok-p`, `dev/arxana-data-constraints.el:57`),
plus open props per type — `:def-patterns` for `:symbol-as-term` (see
`arxana-browser-code-ensure-strategy`, `dev/arxana-browser-code.el:224–229`).
Type matching at read time is lenient: keyword, downcased symbol-name, or
string all match (`arxana-links--finder-type-match-p`, line 269), because
JSON round-trips lose keyword-ness.

**There is no cascade/fallback ordering among finders and no finder
evaluation at all.** Producers: `arxana-links-demo-create-strategy`
(line 983) and `arxana-browser-code-ensure-strategy`
(`dev/arxana-browser-code.el:203–235`) — both only *declare and persist*
finders. Consumers: none execute them; grep for `:symbol-as-term` outside
`arxana-links.el` finds only declaration sites and test fixtures.

### Voiced link (Tier 1 result)

`arxana-links-make-voiced-link` (line 358). Stored fields: `:xt/id`
(`"link:"+md5(source,target)`), `:source` (e.g. `(:type :code-symbol :file …
:symbol … :def-type …)`), `:target` (e.g. `(:type :doc-paragraph :docbook …
:doc-id … :paragraph-anchor …)`), `:found-by` (strategy id), `:status` (one
of `:confirmed :suppressed :orphaned :fuzzy-matched`, line 354),
`:promoted-at/by`, `:annotations`. Voiced links are created per-link by
humans/demos (`arxana-links-promote-voiced-link`, line 945), never by a
finder run.

### Surface form, embedding cache, scholium (adjacent tiers)

- Surface form: `arxana-links-make-surface-form` (line 452); `:concept-id`,
  `:surface`, `:term`, `:concept-label`, `:gloss`, `:context` (`:doc`
  `:snippet` `:offset`), `:source` (`:explicit`/`:inferred`). Feeds finders
  nominally; nothing consumes `:surface` for matching yet.
- Embedding cache: `arxana-links-make-embedding-cache` (line 831); `:space`,
  `:model`, `:dimensions`, `:neighbors` (alist id → [(:id :score)…]),
  `:k`, `:threshold`. Neighbor math (`arxana-links--cosine-similarity`,
  line 898; `arxana-links-compute-neighbors`, line 913) is pure numeric
  code. Consumer: `arxana-browser-patterns.el:2137` (read-only lookup).
- Scholium + resilient anchor: `arxana-links-make-anchor` (line 651) /
  `arxana-links-make-scholium` (line 665). Anchor fields: `:strategy
  :content-hash`, `:content-hash` (`sha256:` prefix), `:content-length`,
  `:context-before/after` (30 chars default, line 57), `:hint-offset`,
  `:hint-line`.

### Validation

`arxana-data-constraints.el` validates *shape only* (non-empty strings, list
types, membership in finder-type/status constants), optionally through
Reazon relations (`--define-relations`, line 142). It is **off by default**
(`arxana-data-constraints-enable nil`, line 12) and the Reazon path is a
no-op wrapper around the same predicate. It is portable data-shape
validation, not semantic constraint checking.

## 2. Three source-cited traces

**Trace A — code entity (symbol) → doc paragraph.**
`arxana-links-promote-voiced-link` (line 945): source defaults to
`(buffer-file-name)` of the current buffer (Emacs buffer), `file-relative-name`
(filesystem+string), target is a `docbook://` doc-id entered interactively.
ID minted from md5 of both endpoints. Persisted via `arxana-store--request
"POST" /entity`. The production caller `arxana-browser-code-ensure-strategy`
declares the `:symbol-as-term` finder but the link itself is human-promoted;
the symbol is never scanned out of a file by the strategy.

**Trace B — document heading/paragraph target (scholium).**
`arxana-links-demo-create-scholium` (line 1059): region read from
`(region-beginning)/(region-end)` in the current buffer (Emacs region);
`arxana-links--context-doc` (line 493) resolves a `docbook://book/doc-id`
identifier from `arxana-docbook--entry-*` buffer-local variables, falling
back to `buffer-file-name` then `buffer-name`. Anchor captures hash +
30-char context. Verification re-finding is the cascade of §3.

**Trace C — relative/fallback strategy (anchor cascade).**
`arxana-links-find-anchor` (line 697): given a stored anchor, (1) try the
hint offset and verify the sha256 of that exact span; (2) else regex-search
for `context-before + fixed-length span + context-after` and verify hash;
(3) else fuzzy `search-forward` of before/after context with a length sanity
check — this tier can return a span whose hash does *not* match (comment at
line 755: "may not verify hash"). Failure: `nil`; caller
`arxana-links-verify-scholium` (line 773) then marks the scholium
`:orphaned`.

## 3. Dependency classification

| Primitive | Class | Notes |
|---|---|---|
| plist/JSON normalize, `--parse-json`, `--jsonify` | pure data/string | lines 86–211 |
| `--generate-id` (md5), `--timestamp`, `--content-hash` (sha256) | pure string | 249–259 |
| `arxana-store--request` HTTP `/entity`, `/entities/latest` | external service (Futon1) | 223; fail-fast via `assert-ok` |
| Reazon shape validation | pure data | optional, off by default |
| anchor cascade (offset → context regex → fuzzy) | pure string algorithm over buffer text | only Emacs coupling is `buffer-substring`, `re-search-forward`, `point-min/max` — trivially string-rewritable |
| `arxana-links-make-anchor` capture | Emacs buffer/marker | region positions, `line-number-at-pos` |
| `arxana-links--context-doc` | Emacs buffer-local state + filesystem | docbook entry vars, `buffer-file-name` |
| surface-form / scholium / promote interactive commands | interactive UI | `read-string`, `use-region-p`, demo render buffers (`--render-promoted-link`, line 1040) |
| embedding neighbor math | pure numeric | 898–943 |
| `--bounce-ok-p` constraint | filesystem | `file-exists-p`, `file-executable-p` (constraints line 116) — unrelated to link strategies, noted for completeness |

## 4. Observed ambiguity and failure semantics

- **Finder level:** none observable — finders are never evaluated, so no
  ambiguity resolution exists to observe. Multiple links to the same target
  are de-duplicated only by the deterministic `:xt/id`
  (md5 of source+target), i.e. same endpoints ⇒ same entity id ⇒ natural
  upsert. Suppression of false positives is a stored status change
  (`arxana-links-suppress-link`, line 400), not removal.
- **Anchor level:** ambiguity is resolved *destructively*: strategies 1 and
  2 take the **first** match that hash-verifies (`re-search-forward` from
  `point-min`); a duplicated region earlier in the buffer silently wins over
  the original. Strategy 3 may return an unverifiable span (documented at
  line 755); status value `:fuzzy-matched` exists in
  `arxana-links-status-values` but `find-anchor` never sets it — only
  `:anchored`/`:orphaned` are used (line 780, 783).
- **Persistence level:** sync disabled ⇒ constructors return `nil` with a
  `*Cannot persist*` message (no error); HTTP failure raises via
  `arxana-store-assert-ok`. Load paths return `nil` on any failure.

## 5. Portability verdicts

| Unit | Verdict | Why |
|---|---|---|
| Entity plists (strategy/link/surface-form/scholium/embedding-cache) | **portable as-is** | plain data; JSON serialization already defined |
| Shape validators (`arxana-data-constraints`) | **portable as-is** | predicate-only; Reazon layer optional |
| Anchor cascade (`find-anchor`) | **needs adapter** | pure algorithm, but takes a buffer; adapt to (string, anchor) → bounds |
| Anchor capture (`make-anchor`) | **needs adapter** | input is region positions in a buffer |
| HTTP persistence wrappers | **portable as-is** (client-side) | thin over `arxana-store--request` |
| `--context-doc` doc identity | **needs adapter** | buffer-local docbook vars |
| Interactive capture/promote/status UI | **Emacs-intrinsic** | regions, prompts, render buffers |
| Finder *evaluation* | **unknown / missing** | no Elisp exists to port |
| Embedding neighbor computation | **portable as-is** | pure numeric |

**Stored data vs executable closures:** critically for the JVM question —
no strategy component is an Elisp closure. Finders are keyword-tagged data
plists; the Reazon relations are the only code evaluated *from* stored
shape, and they are generated fresh at load (`--define-relations`), not
persisted. Everything persisted through `--wrap-entity-payload` is
`json-encode`d data. The Emacs-intrinsic part is the *capture and
re-finding surface* (buffers, regions, buffer-local doc identity), not the
stored semantics.

## 6. Stability test (work item 5)

Existing ERT tests already demonstrate the stability claim for unrelated
text *before* the anchor: `arxana-links-test-find-anchor-after-insert`
(prepend insertion, re-found via context) and
`arxana-links-test-find-anchor-multiline`,
`…-find-anchor-buffer-start`, `…-find-anchor-orphaned`
(`test/arxana-links-test.el`). Analysis (not a new test): for insertion
*after* the anchor, cascade stage 1 (hint offset) still lands on the
original span and hash-verifies, so stability holds there too — provided no
earlier duplicate of the 30-char context+span pattern was introduced, in
which case stage 2's first-match-wins resolves to the earlier copy
(§4). The claim "enclosing anchor + relative finder stable against
unrelated text before/after" is **confirmed by existing tests for 'before',
and by cascade reasoning for 'after'**, with the duplicate-context caveat.
No test was added, per packet constraints.

## 7. Q2 ready / missing

| Q2 prerequisite | Status |
|---|---|
| Strategy/finder data model | **ready** (portable plists, lenient type matching) |
| Link/surface-form/scholium data model | **ready** |
| Anchor cascade semantics (3-stage, first-match) | **ready** (needs string adapter) |
| Anchor stability evidence | **ready** (existing ERT tests) |
| Finder evaluation engine (scan code-roots, match symbol-as-term / filename-mention / surface-form-hybrid) | **missing** — declared, never implemented |
| Cascade/fallback *ordering between finders* | **missing** — no ordering exists to port |
| Ambiguity semantics at finder level (e.g. `:fuzzy-matched` producer) | **missing** — status value defined, never set |
| Embedding-proximity finder path | **partial** — cache read/compute exist; no finder ties them to link generation |
| Deterministic-id/upsert semantics under JVM ids | **ready** (md5 scheme is implementation-independent) |
