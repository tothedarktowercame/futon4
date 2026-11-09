# Code ↔ docs demo — INSTANTIATE record

Plan: CODEDOCS-survey.md §5 (futon4 40330b3), approved by Joe 2026-11-09
with two notes: few items, not coverage; the client must be terminal-friendly
(no X/Wayland; faces, text, echo-area feedback, no mouse-only actions).

## Packet 2 — pinned example and code-definition scopes (accepted after a test fix)

codex-21: futon3c ce5ed7591 (`codedocs.clj`: one metadata-aware Clojure
definition finder, method `codedocs/clojure-definition` v1;
`[:code-definition 1]` scopes whose identity is revision, kind, symbol and
form; codepoint ranges as parser attestations; manifestation/correspondence
registration so `scope/at-position` resolves; fixture
`test/resources/arxana/codedocs/fixture.edn` pinning futon2 historical
16be6314 and current 07cea11a, the two docbook stubs at futon4 40330b3 and
SelfHealRoute.lean by sha256), warrant e4f22b219 (shas match). Five
functions; historical lines 462/746/922/954/1075 vs current
2182/1223/2027/2061/2227. The two docbook stubs have no mojibake. Process
safety: no kills, no :7073 writes, shared pids unchanged.

Review (claude-13): the test pinned only each range's first codepoint and
only that no `:codepoint-range` key sat in the scope. Fixed in futon3c
638b31aa9: the scope's content
keys are exactly the four, the range reads as one whole form naming the
symbol, resolves at its middle and last codepoint and not one past. Plants
caught: line-number range (21 failures), truncated end, range under another
key, finder without `defn-`.

## Packet 3 — doc-entry scopes and code↔doc correspondences (accepted, one test added)

codex-21: futon3c 9245bad7e (`[:doc-entry 1]` scopes, identity revision +
ENTRY_ID + symbol; whole-bullet ranges as parser evidence, method
`codedocs/def-doc-bullet` v1, ending at the next bullet or heading;
`[:correspondence 1]` source = current code scope, target = doc scope,
`:code-documents`, matched by SOURCE_PATH + symbol with a
`codedocs/source-path+symbol` attestation; both directions read by indexed
graph/query), warrant e93e78ef6 (shas match). Five links. Process safety
clean.

Review (claude-13): whole-span and exact-key checks were already there.
Added in futon3c 2fac834da: a SOURCE_PATH with the same base file name in another
directory must not link (the existing wrong-path case could not tell
base-name matching from path matching). Plants caught: one-line range for a
wrapped bullet (6 failures), base-name matching (1), prose mention taken as
an entry (error). The "current revision" choice is made by the caller; the
test pins it.
