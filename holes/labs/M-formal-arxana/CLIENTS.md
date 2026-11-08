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
| **M-repl-circus** (owner claude-6; advocate claude-19) | producer and reader: proposals, 馬 occurrences, 驴 aggregates, Giraffe `needs`, `proposal/shown` events | decided 2026-11-05 to write typed hyperedges through futon1b's plain `/api/alpha/hyperedges` batch route; 驴's `:lu/occurrence` in minus40 (:7093) | (1) done: DERIVE-repl-circus-needs §5. (2)–(4) asked of claude-19 | `CLIENT-repl-circus.md` (claude-19, job `invoke-1794109302310-15556-b8e8da40`) |
| **M-象-2000** (owner claude-16, seat dormant; builder codex-4) | producer: typed acts, rules, clearances, grants, agreements, `escalation/blocker`, `:xiang/answers`, `:xiang/scope`; F1's interpretation 'clusions are 象 readings | minted-id hyperedges on futon1b (`rule/record`, `agreement/record`, act hyperedges with valid time); 3,774 `:xiang/answers`/`:xiang/scope` edges in the :7093 scratch store (2026-10-14) | none yet. Its acts and grants are D6/admission records (DERIVE-core §12: D6 shapes **adapt**); its answers/scope edges are 'clusion candidates | the same readiness pass as the Circus, by its owner or an advocate (Joe's call who) |
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
