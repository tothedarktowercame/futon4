# V1 — code-server consumer contract verification report

Mission: `M-jvm-arxana`. Phase: VERIFY. Verifier: Zai-1, on dispatch from
codex-21 (`invoke-1793996818017-14319-265232ea`). Packet:
`packets/V1-codeserver-contract-verification.md`.

Inputs verified against (live reads, futon4 `f89d192` and futon6 `92da6b7`):
D1–D7 in this lab, `A2-final-design-fitness-argument.md`, futon6
`holes/T-codeserver-arxana-demo.md` (R1–R7 as dispositioned 2026-11-07), and
T-arxana-ts `holes/T-arxana-ts.md` at futon6 `f5e5461`.

## 1. R1–R7 trace matrix

| Req | Traced to | Disposition | Notes |
|---|---|---|---|
| **R1** transport = `arxana.v1`, AnnotationSet names revision IDs, promotion needs authority digest + `resolution-acceptance` | D3 Envelope (`:protocol/version "arxana.v1"`, unknown fields rejected in v1); D3 ops table `revision/ingest` (artifact ID, canonical bytes/digest, expected head) and `assertion/promote` (result digest, evidence, optional acceptance); D1 artifact-revision entity (`arxana:revision:sha256:<digest>` of canonical bytes); D2 promotion section (`resolution-acceptance` naming actor/policy/reason) | **verified-by-spec** | One open seam, not a contradiction: D3 v1 rejects unknown envelope fields, and no operation-level schema for an AnnotationSet payload inside `:body` is registered yet — the projection-registry owner is D7/O-D7d. The demo never sends one pre-service, so nothing is violated; the schema slot is **requires-instantiate**. |
| **R2** status mapping: `exact`→`resolved`; `located`→`resolved-unverified` only for exactly one permitted candidate; span-hash mismatch inside correct bytes ≠ `stale-input` | D2 result algebra table; D2 anchor finder stages 1–3; D2 inputs ("bytes whose digest differs from the named revision are `stale-input`"); D2 `allow-unverified-fuzzy?` default false; multiple fuzzy candidates → `ambiguous`; D5 refusal-mapping non-alias rule | **verified-by-spec**, with a scoping note | The scoping note: T-arxana-ts `Resolution.kind "located"` has three `via` values. `via:"anchor"` is genuinely D2 evaluator territory and the ticket's mapping holds. `via:"sourcepos"` and `via:"align"` are *substrate placement* — where a span lands in a rendering — which in the authority model is D4 locator evidence / D5 client projection, not a D2 evaluator result at all. Mapping them to D2 statuses would be a category error; the demo must not present DOM-placement outcomes as evaluator outcomes. The ticket's wording is compatible; flagging so the demo implements it that way. |
| **R3** `Segment` carries `artifact/revision-id`; preview IDs `client-preview:*`, never `arxana:manifestation:*` | D1 record-class table (revision is the named identity; hash is a check, not identity); D4 manifestation minting (`arxana:manifestation:sha256:<digest>` from authority/family/role tuple; registry refuses duplicates); D7-DR4 (`arxana/*` reserved at futon1b; generic writes refuse) | **verified-by-spec** | Small **requires-spike**: canonical bytes for a segment. The m7 fixture embeds `text` as a JSON string and checks the `.tex` byte-identical; D1 revision identity is over *canonical bytes* with declared algorithm/media-type. The demo must define which byte form (file bytes vs serialized segment text) the digest names before any revision ID is written down, or IDs would be minted over an unstated canonicalization. Pre-service the demo can stay on `Segment.hash` + `client-preview:*` and defer this; INSTANTIATE must settle it. |
| **R4** UTF-16→codepoint conversion on digest-matched revision bytes; reject mid-surrogate; decorations carry IDs only | D5 VS Code section (converts UTF-16 using the exact document revision, explicitly rejecting a boundary inside a surrogate pair; decorations carry only response/candidate IDs; no extension-local identity authority); D3 location vocabulary (`:unit :unicode-codepoint`, zero-based half-open scalar values); D5 client-boundary test 1 | **verified-by-spec** (transcript execution requires-instantiate, increment 6) | |
| **R5** client recovery is preview; promotion requires authority replay; acceptance applies to ambiguous/unverified only | D7-DR2 (only authority-side deterministic evaluation may create promotable evidence in v1); D3 client-hosted evaluation (read-only, client-attested, non-conforming for writes); D2 promotion (`resolved` needs no acceptance record; `resolved-unverified`/`ambiguous` need attributable `resolution-acceptance`) | **verified-by-spec** | |
| **R6** semantic digest field set; frozen JSON digest is a labelled fixture stand-in | D5 "Canonical semantic response digest" (includes protocol/semantics version, operation, domain status, revision IDs, ordered candidate IDs + canonical locations, verification states, semantic evidence digests; excludes message/request ID, principal, timestamps, duration, transport/trace metadata, presentation strings, operational diagnostics); D5 client-boundary test 2; D7 invariant 1 | **verified-by-spec** | The ticket's field list matches the D5 projection word-for-word. The pre-service JSON digest is over fixture bytes, not a versioned canonical projection, and the ticket already says so — no misreporting found. |
| **R7** pre-service demo does zero futon1/futon1b writes; `arxana/*` reserved | D7-DR4 + D7 wiring (authenticated conditional route only); D1 invariant (capability absent → `unsupported`, never simulated success); D3 mutation contract | **verified-by-spec** | Consequence worth stating: the demo also never calls `revision/ingest` (it carries `expected/head` and creates durable state), so even the "freeze my buffer as a revision" convenience is out of scope pre-service. Local buffer edits invalidate decorations (D5 marker-cache rule) and produce nothing durable. |

Every row has a cited disposition. No `contradiction` findings.

## 2. Representative payloads on paper

### 2.1 Frozen AnnotationSet ingest (demo step 1)

Input: a00J02 m7-scopes JSON — 1 segment (`mark7-0708.2185`), 6 layers, 520
spans, 9 edges, `offset_unit` codepoints. Client-side path: sha256(segment
text) == `Segment.hash` → all spans render `exact`; decorations carry
`client-preview:<span-id>`; no envelope, no `message/id`, no authority
interaction. If the same bytes were sent to the authority (post-service):
`revision/ingest {artifact-id, canonical-bytes, digest, expected-head}` →
returns `arxana:revision:sha256:…`; the AnnotationSet then names that
revision ID and every span resolution cites it (D3 `scope/resolve` requires
artifact revision + strategy revision). Pre-service: the first half only;
the JSON digest is reported as fixture parity, nothing else.

### 2.2 Resolution mapping

| Client outcome | Authority result | Source |
|---|---|---|
| `exact` (hash matched at hinted range) | `:resolved`, one candidate `:verification :verified-hash` | D2 stage 1 |
| `located/anchor`, one hash-verifying context match after hint miss | `:resolved` (stage 2 verified) | D2 stage 2 |
| `located/anchor`, one fuzzy context match, fuzzy permitted | `:resolved-unverified`, `unverified-fuzzy` + similarity features, never rendered "verified" | D2 stage 3 |
| same, `allow-unverified-fuzzy? false` (default) | `:missing` with diagnostic evidence | D2 options |
| two context matches retained | `:ambiguous`, both candidates, ranking is evidence not selection; demo must render both, no first-select | D2 DR2, D5 test 4 |
| `unanchored` | `:missing` + typed reason ("unanchored is a result, not an absence" survives as a *display* invariant, not a distinct status) | D2 table; T-arxana-ts §1.1 |
| revision bytes SHA-256 ≠ named revision | `:stale-input` (evaluator result, distinct from protocol `stale`) | D2 inputs; D3 DR2 |
| span content-hash mismatch, revision bytes correct | stage 1 fails silently → stages 2/3 decide: `resolved` / `ambiguous` / `missing` / `resolved-unverified`. **Not** `stale-input`. | D2 stages; matches the R2 correction exactly |

### 2.3 UTF-16 conversion

Document contains `𝔘` (U+1D518) at codepoint 120; it occupies UTF-16 units
120–121. VS Code selection ending at unit 121 → mid-surrogate boundary →
**reject** with typed invalid input (D5 VS Code section, test 1). Selection
ending at unit 122 → codepoint range `[120, 121)` — the same authority range
the Emacs client computes from its own buffer. Digest of buffer bytes must
equal the named revision first; mismatch triggers refresh, never a
guessed conversion (D5 Emacs section, same rule by D5-DR1 symmetry).

### 2.4 Preview versus promotion

Edit above a span in code-server → client anchor cascade re-finds text →
decoration labelled `resolution: preview (client-attested)`. Promotion path
pre-service: none. Post-service: `scope/resolve` on immutable bytes →
authority result digest → `assertion/promote`; if the authority returns
`ambiguous`/`resolved-unverified`, promotion additionally requires an
`arxana/resolution-acceptance` naming actor, policy, reason and selected
candidate(s) — "accept evidence ≠ admit action" (D6). The client may request
promotion; it can never supply its preview as the evidence (D7-DR2).

### 2.5 Semantic digest comparison

Post-service target: same `scope/resolve` request from the Emacs client and
the code-server extension (message/principal IDs differ) → byte-identical
`semantic/digest` over the canonical projection (D5 transcript equivalence).
Pre-service stand-in: digest of the frozen m7-scopes JSON — a fixture parity
number. The demo UI must label it "fixture digest", never "semantic digest"
(acceptance criterion 2 of the packet: not misreported as a D5 semantic
digest — satisfied by the ticket's R6 wording; the demo's legend/histogram
copy must keep the label).

### 2.6 Zero-write mode

Demo operation inventory: local file reads (.tex, JSON), in-memory model,
decorations/hover/webview, scripted buffer edits. No D3 mutation-shaped
operation, no `Store.put`, no futon1/futon1b HTTP of any kind. `Store.put`
exists in T-arxana-ts and a futon1 store adapter is listed there for
compatibility experiments — in this demo it stays unexercised; per D7-DR4
such an adapter could not lawfully write `arxana/*` anyway.

## 3. Where the demo could accidentally claim authority it lacks

1. **Minting identity**: formatting any preview ID as `arxana:manifestation:*`
   or `arxana:revision:*` (R3). Preview namespace only.
2. **Presenting preview recovery as resolution**: rendering client fuzzy
   anchor results with "verified" styling or status `resolved` (R5); D2 keeps
   them `unverified-fuzzy`/`resolved-unverified`.
3. **Digest mislabeling**: showing the frozen JSON digest as authority
   semantic equality (R6).
4. **First-candidate selection**: auto-picking one span among duplicate
   context matches (D2 DR2, D5 test 4).
5. **Confidence as verification**: T-arxana-ts `located` carries a
   `confidence` number; presenting it as a verification class conflates
   score evidence with standing (D2 candidate ordering "ranking is evidence,
   not selection").
6. **Buffer edits becoming revisions**: silently treating an edited buffer as
   a new artifact revision without `revision/ingest` + expected head (D3
   write contract); pre-service the only lawful outcome of an edit is
   decoration invalidation.
7. **Substrate placement as evaluator result**: reporting `located/sourcepos`
   or `located/align` outcomes under D2 status vocabulary (R2 scoping note).
8. **Store writes**: exercising `Store.put` against futon1/futon1b in the
   demo, including non-`arxana/*` writes that would imply a storage role the
   demo does not have (R7; D7 authority table assigns no durable write to
   clients).

## 4. Code-server vs desktop VS Code

D5's VS Code surface is specified in extension-host terms (commands, UTF-16
conversion, decorations carrying IDs, no extension-local identity database).
Code-server is the same extension host in a browser, so every D5 rule
transfers unchanged; a browser-delivered decoration is still a disposable
projection. **No new authority question arises.** Two co-location concerns
checked:

- **Shared URL / wider audience**: anyone with the URL can view and edit the
  buffer; buffer edits are client-local projections with no durable effect
  (D1/D5), so visibility widens, authority does not.
- **Terminal/repository access in code-server**: could write repository files
  directly, but repository files are not Arxana records — artifact revisions
  are content-addressed entities created only via `revision/ingest` with
  expected head, and out-of-band head replacement is refused or becomes an
  explicit administrative revision (D3 trace 3). Co-location changes no
  ownership: the D7 authority table assigns nothing to clients regardless of
  host capability.

## 5. Failed / conditional checks

- **Conditional (R1):** no registered v1 schema for an AnnotationSet payload
  inside the D3 `:body` — requires-instantiate (registry owner per O-D7d).
- **Conditional (R3):** canonical-bytes definition for segment text (file
  bytes vs serialized string) must be fixed before any revision ID is derived
  — requires-spike, blocking only for INSTANTIATE.
- **Conditional (R2 note):** demo implementation must scope `located`'s
  `via:"sourcepos"/"align"` to substrate projection, not D2 statuses —
  verified at spec level, a demo-implementation obligation.
- **Requires-instantiate:** the cross-client semantic-digest equality
  transcript (D5 test 2) and the arxana.v1 adapter swap (demo step 5) both
  wait on the headless service and the repaired draft8 fixture (D7 increment
  1 gate).

## 6. Verdict

**Verified-by-spec: the finalized D1–D7 design, as closed at `f42b39f` /
`0fd5ad1` / `f19eafe`, fully supports the code-server consumer contract the
futon6 ticket records; all seven R-items trace to specific D-item fields,
operations, authorities and tests with no contradictions.** No stop-the-line
finding. Three items wait on INSTANTIATE (payload schema registration,
canonical-bytes definition, service-backed digest transcript); none blocks
the pre-service demo steps 1–4 as specified, provided the demo keeps the
zero-write, preview-labelling and `client-preview:*` disciplines above.
