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

## Packet 4 — Lean declarations and their citations of the code (accepted after a fix)

codex-21: futon3c 606ef79dd (`[:lean-declaration 1]` scopes, identity Lean
revision + FQN, namespace-qualified; nested-comment/string-aware lexer;
declaration and doc-comment ranges as parser attestations;
`[:correspondence 1]` `:cites-code` to the current code scope with an
`:unverified` parser attestation naming the token and its position),
warrant 9bb99d08a (shas match). Process safety clean; no mathlib4 build.

Review (claude-13). The port found 11 citation pairs; the legacy browser
finds 16 on the same pinned file. My packet said "attached doc comment" only,
but the legacy rule (`arxana-browser-code--lean-declarations`) also reads the
`--` line comments in the declaration body, so `prepare-diagnosis` and
`prepare-acceptance` cite `RouteState` through
`-- diagnosis stage (`prepare-diagnosis`)`. Fixed in futon3c 05f43f6f2:
body `--` comments cite; a body ends at the next declaration or its attached
doc comment, not at a constructor's doc comment inside it. The test's oracle
is the legacy function's own output on the pinned file: run-opportunity! 0,
prepare-diagnosis 1, production-readiness 4, route! 10, prepare-acceptance 1.
Added (next commit): a module docstring directly above a declaration is not
its doc. Plants caught: bare-word citation (35), module docstring as doc (1,
after the added case), FQN without namespace (13), kernel standing on a
parser citation (48), body comments dropped (4).

## Packet 5 — `:codedocs/view` (accepted)

codex-21: futon3c 558bb867e (`codedocs_view.clj`; `[:scope-in-revision 1]`
membership so a revision's scopes are found by indexed query; the view reads
only revision/get, entity/get and bounded graph/query; definitions in source
order with doc spans and citing Lean declarations, first sentence flattened
and backtick-free, parser/unverified standing, record ids, typed truncation
and missing), warrant 290df69a3 (shas match). route! lists the legacy ten
Lean declarations; prepare-diagnosis cites RouteState. The historical
revision reports `:links/status :not-materialized-for-revision`.

Review (claude-13): plants caught — backticks kept in the first sentence (4),
doc text read off by one / from the wrong text (8), Lean list unsorted (2);
two-store reversed write order gives identical views (in the test).
`:links/at-revision` is nil rather than naming the current revision; the
status field says what matters. Process safety clean.

## Packet 6 — survives edits (accepted, one test added)

codex-21: futon3c 431aa8554 (`codedocs_relocation.clj`: F1's exact anchor
per historical definition, relocated onto the current revision, appended as
evidence; `formal_f1_relocation/relocation-attestation` now omits
application fields that are absent instead of storing nils), warrant
ebb9b0350, report `test/resources/arxana/codedocs/survives-edits.edn`.

| function | exact relocation | current scope by name | doc links | Lean links |
|---|---|---|---:|---:|
| run-opportunity! | missing (body 4511→1675 chars) | found | 1 | 0 |
| prepare-acceptance | missing (900→3016) | found | 1 | 1 |
| prepare-diagnosis | missing (1438→1537) | found | 1 | 1 |
| production-readiness | unique (body unchanged) | found | 1 | 4 |
| route! | missing (15170→21070) | found | 1 | 10 |

Exact: 5 historical / 1 current / 1 both. By name: 5/5. Read together: every
link still reaches its function; for four of the five the body changed after
the link was made, so their docs and Lean citations deserve a re-read. My
independent body comparison (futon2 git show, balanced-form extraction)
gives the same four changed and one unchanged. Duplicate text: ambiguous,
two candidates, never first-picked.

Review (claude-13): F1/F2 relocation, projection, markup and demo tests still
pass after the relocation-attestation change (11 tests, 383 assertions).
Added in futon3c 72d55ec5b: the duplicate case appends its attestation and reads
it back (outcome :ambiguous, two candidates); before, a reporting layer that
called ambiguity unique passed (plant survived; now 1 failure).

## Packet 7 — terminal two-pane client (accepted after review fixes)

codex-21: futon3c e58b95d5d (`:since` per definition in `:codedocs/view`,
from the stored packet-6 relocation attestations; private demo
`codedocs_demo.clj`; the arxana JSON codec now carries strings that begin
with ":" as `{"$string": s}` instead of refusing to encode them; a per-request
read cache in the view), warrant b9c92cf9d; futon4a 73d09a6
(`futon4a-codedocs.el`: pinned read-only code buffer beside
`*Arxana Code Docs*`, current-definition highlight, echo summary, RET records,
definitions table; pid-owned launch/stop; real-stack ERT). Process safety
clean.

Review (claude-13), walked in a clean `emacs -Q` daemon against a private
stack, then fixed in futon4a bfe426a:
- the definitions table was always empty (it read the view inside the table
  buffer); ERT now checks its rows (the old code fails it);
- `route!`'s Lean summary ":refused reasons of …" begins with ":" and now
  travels as `$string`; `futon4a-arxana--post` unwraps it for every view
  (the VS Code client in futon6 does not yet — open item);
- the client takes the demo's port from the ready file; the since line
  dropped a hash suffix that read like a futon2 sha; the docs pane redraws
  only when point enters another definition.

## Packet 8 — demo for Joe

Started 2026-11-09 on 127.0.0.1:7091 (`systemd-run --user --scope
--unit=futon4a-codedocs-demo`, futon3c master, futon4a bfe426a), ready file
`/run/user/1000/futon4a-codedocs-1000/ready.edn`. Smoke with default settings
in a fresh daemon: the client found 7091 from the ready file; definitions
table prepare-acceptance/prepare-diagnosis/route! changed,
production-readiness unchanged.
