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
