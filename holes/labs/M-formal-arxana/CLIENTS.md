# Clients of M-formal-arxana — readiness for INSTANTIATE

claude-13, 2026-11-08 (Joe: hold INSTANTIATE for Codex; meanwhile get the
clients ready to consume it). This note does not run any client mission. For
each client it records what that client will read and write through Arxana,
where it writes relations today, and what "ready" means: when C1/C2/C3 land,
the client can cut over without redesign.

**Ready** means four things are written down, by the client's owner or
advocate: (1) its record kinds mapped to the round-9 shapes (DERIVE-core
§17); (2) a cut-over inventory (file:line of every graph write and read,
with its target port); (3) a decision for each legacy record type on a
non-Arxana route (read as a referenced source, or re-assert through C2);
(4) a named reader for every record kind it stores (ARGUE P15). Real records
it supplies become C1 golden-record candidates.

| Client | Role | Relations today | Readiness | Next |
|---|---|---|---|---|
| **M-repl-circus** (owner claude-6; advocate claude-19) | producer and reader: proposals (3 producers), 馬 occurrences, 驴 aggregates, Giraffe `needs`, `proposal/shown` events | nothing on :7073 yet (claude-8's batch-route plan has written nothing); on :7093 `xiang/scope` 14,140, `xiang/act` 11,021, `xiang/open` 7,445, `lu/occurrence` 5,400 (4,996 of them void) | **ready on paper**: all four items in `CLIENT-repl-circus.md` (claude-19, e682421), reviewed by claude-13 (3 sites and 3 golden turn ids spot-checked). Legacy decision: re-assert only the 404 non-void verdicts and 62 Giraffe edges, from turn records and JSON, not from the edges; the rest stays a referenced authority. Four golden-record candidates for C1 | for M-repl-circus (not us): `score-patterns` weighs every void −0.5 (`lu_memory.clj:338`), including 374 "no reader" records, so a reader that did not run lowers a pattern's score; and `situations-of` and the :7093 `lu/occurrence` edges have no reader (P15) |
| **M-象-2000** (owner claude-16, seat dormant; builder codex-4) | **source, not a cut-over client.** Typed acts, rules, clearances, grants, agreements, `escalation/blocker`, `:xiang/act`/`open`/`answers`/`scope`; F1's interpretation 'clusions read 象 readings | minted-id hyperedges on futon1b (`rule/record`, `agreement/record`, acts with valid time); on :7093 `xiang/scope` 14,140, `xiang/act` 11,021, `xiang/open` 7,445 (claude-19's count, 2026-11-08) | Substantially complete, awaiting acceptance by live demo (futon3c `E-elephant-2000-feature-completeness-demo`, Joe 2026-11-07; not yet run). DERIVE-core §12 already rules the 象 store **preserve**: Arxana reads it through typed external references and does not ask 象 to change routes. So no readiness pass now: it would reopen a mission that is waiting to be accepted | none before its acceptance. Afterwards, only if 象 is to write through Arxana: the same four-item pass |
| **War Machine / Giraffe / M-the-perfect-crime** | readers of the GCC (priority, hotspots, misconstruals, cascade construction) | read Giraffe passes and tickets; M-the-perfect-crime is at HEAD | reader side only: they consume what the Circus and 象 write. Covered by the Circus inventory (Giraffe) | none until the Circus inventory lands |
| **M-ways-of-working** | admission actor for F4 (external governance; outside Arxana semantics) | its own probes and records | F4 needs one admitted bounded operation and one refusal; the admission-decision record shape comes from D6 (adapt) | at F4 DERIVE detail, not now |
| **M-latex-wysiwyg** (designed, nothing built) | guarded edit and rendered manifestation; an F2/F6-style client | none | not a consumer until it builds; F2's rebase contract (VERIFY S2) is what it would use | none now |
| **futon4a (Emacs)** | the fixture client: F1 projections, F2 writes, F5/F6 pages | display-only today (MAP-Q4, Q6) | built *by* F1/F2 increments, not a separate readiness task | in INSTANTIATE |
| **futon6 VS Code** | F2's second writer | read client with typed refusal (MAP-Q4) | built by F2 | in INSTANTIATE |
| **Euclid tutor** (future) | mathematics client of F3/F5 material | none | no action; noted so F3/F5 records stay usable by it | none |

## What each client should *not* do meanwhile

Keep writing where it writes now; nothing is gained by moving before C2
exists. But new relation types should be shaped as round-9 records
(endpoints with roles, asserted content, evidence as separate records), so
the cut-over is a change of route, not of shape.
