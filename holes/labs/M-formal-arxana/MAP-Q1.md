# MAP Q1 — Existing 'clusion shapes (roles, topology, dynamics, evidence, standing, composition, convergence)

## Answer

Three live substrates already carry parts of the 'clusion shape, none carries
all of it. **futon1b (XTDB :7073)** stores entities, binary relations
(`relations` docs: `:type :src :dst :provenance`, futon1b_graph.clj:861–928)
and flat hyperedges (`:hx/type`, `:hx/endpoints`, `:hx/props`;
futon1b_arxana.clj:145–165; live record via
`GET /api/alpha/hyperedges?type=memory/assert&limit=1` shows `:hx/ends` as
bare `{:entity-id …}` — no per-endpoint role). The sidecar index (futon1b_hxindex.clj)
adds position (`hx_edge(hx_id,type,pos,endpoint)`) but only as derived seek
structure, not exposed semantics. **futon3c JVM arxana** has the richest
'clusion-analog: correspondence assertions with direction, methods,
verification standing (`:stale 0 … :verified 4` rank, correspond.clj:18–22),
manifestation roles (`#{:authored :generated}`, line 43), attributable
assessments (`:accepted/:rejected/:absent` with actor, policy, evidence ids,
content-addressed id, lines 45–298), and versioned operation schemas
(schema.clj). **futon4/dev/arxana-store.el** is a write client for the futon1b
shapes only. **Prose-only:** 'clusion-as-frozen-dynamics — gather/transform/
compose operations, recursive participation, port-disciplined topology,
wiring-diagram reference, geometry — exists only in
M-self-representing-stack.md §Hyperedges as Frozen Dynamics (lines 183–242)
and the M-formal-arxana HEAD; no store field represents an operation,
composition step, or convergence policy for edges.

## Evidence

- futon1b hyperedge doc shape — `futon1b/futon1b_arxana.clj:145–165`
  (`arxana-hyperedge-doc`: `hx:<type>:<sorted-endpoints>` stable id,
  `:hx/endpoints` strings, `:hx/ends` `{:entity-id}` only; `hx/props` free map).
- Live probe (read-only GET, 2026-11-24):
  `curl -G :7073/api/alpha/hyperedges --data-urlencode type=memory/assert --data-urlencode limit=1`
  → one record `{:hx/endpoints ["bench-memory-assert-…-e" "benchmark-subject"],
  :hx/ends [{:entity-id …} …], :hx/type :memory/assert}` — implemented and
  exercised; no role/topology fields present.
- Binary relations with provenance — `futon1b/futon1b_graph.clj:861–928`
  (`:type :src :dst` required, `:provenance` map, resolve-rel-endpoint).
- Hyperedge positional index (derived, not served) —
  `futon1b/futon1b_hxindex.clj:2–29` (P1 "fills the index only; no read is
  served from it yet").
- futon3c standing ladder and roles —
  `futon3c/src/futon3c/arxana/correspond.clj:18–45` (`verification-rank`
  stale/unverified/ambiguous/partial/verified; `manifestation-roles`;
  `assessment-verdicts`).
- Attributable, evidence-bearing assessments (content-addressed id over
  subject/verdict/actor/policy/evidence-ids) — correspond.clj:287–298.
- Ambiguity preserved, convergence delegated to caller — correspond.clj:378–381
  ("Returns ALL matching assertions; never selects among ambiguous candidates")
  and `next-absent`/`next-ambiguous` `require-order!` (lines 415–470): "Arxana
  does not invent domain sequencing".
- Operations registry with pinned projections (dynamics of the protocol, not
  of edges) — `futon3c/src/futon3c/arxana/schema.clj:7–80`.
- Frozen-dynamics / 'clusion account — prose only —
  `futon4/holes/missions/M-self-representing-stack.md:183–242` (hyperedge =
  membership + roles + topology; props carry dynamics; hyperedges should
  *reference* futon5 wiring diagrams). No corresponding field in any schema
  above (searched `hx/` keys in futon1b_arxana.clj and schema.clj; searched
  `role|topolog|dynamic|clusion` across futon3c/src/futon3c/arxana/ and
  arxana-store.el — no hits beyond correspondence role/verification).
- Emacs client surface — `futon4/dev/arxana-store.el:708–832`
  (create-relation src/dst/label; create-hyperedge type/endpoints/props/id/
  content/labels/confidence; fetch/query). No roles, composition, or dynamics.
- Exercise status: futon3c correspond/schema exercised by tests
  (`futon3c/test/futon3c/arxana/correspond_test.clj`, `spine_test.clj`,
  13 test namespaces present); futon1b hyperedge writes exercised live
  (benchmark record above + `test_hx_read_after_write.clj`); Emacs wrapper
  implemented; exercise not verified this survey (no in-namespace grep run for
  call sites — noted as doubt).

## Ready / Missing

| Property | Ready — no new code | Missing — actual work |
|---|---|---|
| Roles | futon3c manifestation roles (`:authored/:generated`, correspond.clj:43); relation src/dst asymmetry (futon1b_graph.clj:861) | per-endpoint roles on hyperedges (futon1b `:hx/ends` is bare entity-id); n-ary directional roles |
| Topology | flat endpoint membership (futon1b); assertion direction (correspond.clj) | how endpoints connect *through* the edge; port discipline; wiring-diagram reference (prose-only, M-self-rep 183–242) |
| Operations/dynamics | protocol operations versioned (schema.clj registry-v1) | 'clusion operations (gather/transform/compose) as data; frozen-dynamics in props (prose-only) |
| Evidence | assessments carry `:evidence/ids` + actor/policy (correspond.clj:287–298); `assertion/promote` requires `:result/evidence` (schema.clj) | evidence attached to futon1b hyperedges/relations beyond free-text `:provenance` map |
| Standing | 5-level verification ladder + assessments (correspond.clj:18–45); current-only filtering | standing on futon1b edges (none); temporal supersede policy for assertions unexamined |
| Composition | none found (searches above) | recursive 'clusion-as-endpoint; composition chains (F4 is greenfield) |
| Convergence policy | ambiguity-preserving queries + caller-supplied ordering (correspond.clj:378–470); content-addressed idempotent ingest (artifact.clj `ingest!`) | any edge-level CRDT/OT/merge policy; Q7 owns the fuller inventory |

## Open doubts

- Whether `arxana-store.el` functions are exercised by any live Emacs flow was
  not verified (call-site search not run); classified implemented-but-
  unexercised pending that check.
- futon5 "exotypes / social-exotype" wiring diagrams: the M-self-rep section
  cites them as the topology exemplar, but grep found only analysis scripts in
  `futon5/scripts/` — the exotype substrate itself may be prose-only or live
  elsewhere; a dedicated pass (Q9/Q10 adjacent) should census it.
- The `memory/assert` live record is a benchmark fixture, not organic use;
  organic hyperedge usage volume on :7073 was not measured.
