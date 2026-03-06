# Mission: Futon Enrichment (Rational Reconstruction)

**Date:** 2026-03-04
**Status:** INSTANTIATE (Phase 2 API live-tested, 2026-03-06)
**Blocked by:** None (M-three-column-stack complete, reflection API operational,
evidence store operational, hyperedge API operational)
**Owner:** futon4 (Arxana), with dependencies on futon3c (reflection API,
evidence store, Mission Control), futon1a (hyperedge persistence, XTDB
bitemporality), futon5 (devmap topology)

## Motivation

M-three-column-stack gave us the schema: code, project, and math columns with
typed hyperedges and cross-column invariants. M-self-representing-stack proved
the plumbing works: tensions trace through 6 gates, reflection envelopes ground
claims to source code, XTDB persists it all.

But the columns are mostly empty. The code column has reflection data (live in
the JVM, queryable via API), the project column has mission/devmap/evidence
data (live in MC), and the math column has 153 hyperedges from futon6. What's
missing is the *cross-column connective tissue* — the edges that say "this
function was created by this mission, uses this pattern, has this evidence
trail, participates in these tensions."

The code browser (`arxana-browser-code.el`) currently shows docbook matches
and source docstrings. With enrichment data, it could show mission context,
pattern provenance, evidence trails, tensions, and cross-futon dependencies
for every function — turning the side panel from documentation into a
navigable self-representation.

### Why Rational Reconstruction, Not Bulk Import

A one-shot static analysis would produce a large dump with no provenance.
We'd have edges but no story about why they exist or when they were discovered.

Instead: **rational reconstruction**. We replay the development history as if
we had been maintaining enrichment discipline all along. Each pass through the
codebase produces a timestamped layer of hyperedges with evidence entries
recording what was found, what was surprising, and what doesn't match.

This has three advantages:

1. **Replayability.** XTDB's bitemporality means each layer is queryable as of
   its ingestion time. You can "watch" the enrichment grow: "what did we know
   about `futon3c.peripheral` as of layer 3?"

2. **Incremental correction.** Early layers will have errors. Later layers fix
   them. The correction history is itself valuable — it shows where the
   codebase's self-image was wrong.

3. **Methodology transfer.** If the rational reconstruction works for the futon
   stack, it works for any codebase. The discipline of "enrich incrementally,
   record what you find, correct as you go" is the same discipline we'd want
   clients to adopt. Building it on our own code first proves the process.

### Prior Art

**Code as a Crime Scene (Tornhill, 2015).** Adam Tornhill's insight: git
history is a behavioral signal, not just an audit trail. Files that change
frequently AND are complex are "hotspots" — likely sources of bugs. Files that
always change together have "temporal coupling" — hidden dependencies. His tool
`code-maat` extracts these signals from any VCS. His second book ("Software
Design X-Rays") extends the analysis to function-level granularity.

**EmacsConf 2020 (Giugliano).** Andrea Giugliano integrated code-maat into
Emacs, answering questions like "show me technical debt," "who knows this
file?", "if I change this, what else changes?" — all from M-x. Demonstrated
on multi-million-line codebases. Used indentation-as-complexity (crude but
universal, works even for Lisp). Key UX: answers are one command away, shown
in side windows.

The futon enrichment approach subsumes and extends this:
- Hotspots → tension surface (richer than change-frequency: invariant
  violations, evidence counts, mission coverage gaps)
- Temporal coupling → cross-futon dependency edges (structural, not just
  behavioral)
- "Who knows this file?" → mission provenance (var→mission→author, not just
  git blame)
- Function-level analysis → reflection envelopes (arglists, protocols,
  dependency graphs per-var)

The rational reconstruction framing adds what code-maat lacks: each analysis
layer is a timestamped, replayable snapshot with its own evidence trail. XTDB
bitemporality means you can query "what did we know about this namespace as of
layer 3?" — something a one-shot analysis can't do.

References:
- Tornhill, "Your Code as a Crime Scene" (2015)
- Tornhill, "Software Design X-Rays" (2018)
- Giugliano, EmacsConf 2020: "Emacs and code-maat"
  (https://ag91.github.io/blog/, code-maat: https://github.com/adamtornhill/code-maat)

## Scope

### What This Mission Produces

1. **Enrichment layers** — timestamped batches of hyperedges in XTDB:
   - Layer 0: Namespace topology (all repos, ns→ns deps, entry points)
   - Layer 1: Mission provenance (var→mission, "who created this?")
   - Layer 2: Pattern provenance (var→pattern, "what pattern was applied?")
   - Layer 3: Evidence binding (var→evidence, "what PSRs/PURs touch this?")
   - Layer 4: Tension surface (var→tension, "what invariants does this violate?")
   - Layer 5: Cross-futon dependencies (var→var across repos)

2. **Enrichment API endpoint** — `GET /api/alpha/enrich/file?path=...`
   Returns a composite JSON blob: missions, patterns, evidence counts, tensions,
   and deps for every symbol in the file. One call, no N+1.

3. **Enriched code browser** — `arxana-browser-code.el` extended with:
   - Side panel: Mission Context, Patterns Applied, Tensions, Dependencies
   - Code overlays: margin annotations for evidence counts, tension warnings

4. **Replay viewer** — ability to query enrichment as-of any layer timestamp,
   showing how understanding of the codebase accumulated.

### What This Mission Does Not Produce

- Mathematical enrichment (math↔code edges) — future mission
- Automated enrichment maintenance (auto-update on commit) — future mission
- Client-facing enrichment tooling — future mission, after process validated

## Architecture

### Enrichment Layers

Each layer is a batch ingestion into futon1a's hyperedge store. The ingestion
script records:
- Layer number and timestamp
- Hyperedge count (before and after)
- Anomalies found (unexpected deps, missing vars, coverage gaps)
- An evidence entry summarizing the layer

Layers are idempotent — re-running a layer updates existing edges rather than
duplicating them. XTDB's bitemporality preserves the history regardless.

### Data Flow

```
                  ┌─────────────┐
                  │  Reflection  │ ← live JVM metadata
                  │  API (3c)    │
                  └──────┬───────┘
                         │
  ┌──────────┐    ┌──────▼───────┐    ┌──────────────┐
  │ Mission  │───►│  Enrichment  │───►│  Hyperedge    │
  │ Control  │    │  Script      │    │  Store (1a)   │
  └──────────┘    └──────┬───────┘    └──────┬────────┘
                         │                    │
  ┌──────────┐    ┌──────▼───────┐    ┌──────▼────────┐
  │ Evidence │───►│  Composite   │◄───│  XTDB Query   │
  │ Store    │    │  API (3c)    │    │  (bitemporal)  │
  └──────────┘    └──────┬───────┘    └───────────────┘
                         │
                  ┌──────▼───────┐
                  │  Code Browser │
                  │  (Emacs)     │
                  └──────────────┘
```

### Enrichment Endpoint

```
GET /api/alpha/enrich/file?path=src/futon3c/peripheral/mission_control_backend.clj

Response:
{
  :file "src/futon3c/peripheral/mission_control_backend.clj"
  :namespace "futon3c.peripheral.mission-control-backend"
  :enrichment-layer 3   ; highest layer ingested
  :missions [{:mission/id "M-mission-control" :mission/status "COMPLETE"}]
  :symbols {
    "build-portfolio-review" {
      :evidence-count 3
      :patterns ["learn-as-you-go"]
      :tensions []
      :deps {:requires ["futon3c.evidence.store"] :required-by ["futon3c.transport.http"]}
    }
  }
}
```

### ArtifactRef Extension

Add `:code-symbol` to ArtifactRefType in shapes.clj:
```clojure
{:ref/type :code-symbol :ref/id "futon3c.peripheral.mission-control-backend/build-portfolio-review"}
```

This bridges the evidence landscape to individual vars, enabling queries like
"all evidence touching this function."

## Argument (Plain English)

The stack stores structured data about itself — hyperedges in XTDB, reflection
metadata from running code, mission/tension data from MC. But these pieces
don't connect. The code browser shows docstrings; it doesn't show which mission
created a function, what patterns were applied, or what breaks when you change
it. The connective tissue between code, project, and math is missing.

A one-shot static analysis would fill the gap but give no provenance — edges
with no story about when they were discovered or whether they're correct.
Instead: **rational reconstruction**. Build enrichment in layers, one at a
time, recording findings as we go. XTDB bitemporality makes each layer
queryable as-of its ingestion time; corrections in later layers supersede but
don't erase earlier ones. The growth of understanding is itself navigable.

For Layer 0, 80% already exists in `ingest-three-columns.py` (namespaces,
vars, dependency edges). Two additions — git churn and indentation depth —
give us Tornhill-style hotspots as a free bonus.

Five carried patterns govern five concerns:

| Pattern | Sigil | Concern |
|---------|-------|---------|
| p4ng/reflect-in-layers | 节 | Layer architecture |
| iching/hexagram-53-jian | 引 | Gradual progress |
| agent/evidence-over-assertion | 示 | Each layer is verifiable evidence |
| stack-coherence/evidence-ledger | 本 | Auditable ledger entries |
| realtime/learn-as-you-go | 日 | Capture surprises, not just findings |

Five decisions not covered by existing patterns are argued in
`futon3/library/enrichment/`: extend-not-rewrite (D1), churn-as-signal (D2),
indentation-as-complexity (D3), layer-as-evidence (D4),
rational-reconstruction (D5). Full flexiarg argument at
`futon3/library/enrichment/ARGUMENT.flexiarg`.

## Phases

### Phase 0: Layer 0 — Namespace Topology (bootstrap)

Ingest namespace dependency graph for all repos into hyperedge store.
One hyperedge per namespace, one per dependency edge. This is purely
mechanical — reflection API already provides the data.

Also ingest indentation-complexity scores per file (à la code-maat). This is
language-agnostic, works for `.el`/`.clj`/`.py`/`.js`, and gives a cheap
initial complexity signal even where reflection isn't available. Combined with
git change-frequency, this produces Tornhill-style hotspots as a layer 0 bonus.

**Deliverable:** `ingest-layer-0.py` (or `.clj`), hyperedge count, evidence entry,
hotspot ranking.

### Phase 1: Layer 1 — Mission Provenance

For each mission, identify the vars it created or modified. Source: mission
docs (`:out` files, completion criteria), git blame, devmap component→file
mappings.

**Deliverable:** var→mission hyperedges, reverse index queryable via API.

### Phase 2: Enrichment Endpoint + Browser

Wire the composite API endpoint. Extend `arxana-browser-code.el` to consume
it. Initially shows whatever layers have been ingested (possibly just L0+L1).

**Deliverable:** Working enriched side panel, even if sparse.

### Phase 3: Layers 2-4 — Patterns, Evidence, Tensions

Ingest pattern provenance (from PSR/PUR evidence), evidence bindings, and
tension surface. Each as a separate timestamped layer.

**Deliverable:** Full six-item enrichment visible in browser.

### Phase 4: Layer 5 — Cross-Futon Dependencies

Static analysis of cross-repo requires/imports. Which futon3c functions call
futon1a? Which futon4 elisp functions invoke futon3c HTTP endpoints?

**Deliverable:** Forward and backward cross-futon links in browser.

## Open Questions

1. **Layer granularity.** Should each repo get its own sub-layer, or is one
   layer per enrichment type sufficient?

2. **Git blame integration.** For mission provenance, git blame gives
   commit→file mapping. Do we trace commits to missions automatically (via
   issue references in commit messages) or manually?

3. **Elisp enrichment.** The reflection API serves Clojure. For `.el` files,
   we'd need a separate static analysis (maybe `treesit` or `eglot`). Is
   this in scope or a future mission?

4. **Replay UX.** The replay viewer could be a slider ("show enrichment as of
   date X") or a diff view ("what changed between layer 2 and layer 3"). Which
   is more useful first?

5. **EmacsConf prior art.** ~~RESOLVED~~. Giugliano's EmacsConf 2020 talk on
   code-maat integration. Referenced in §Prior Art above.

## Layer 0 Evidence (2026-03-04)

**Run**: `python3 scripts/ingest-three-columns.py --layer0` (0 errors)

**Before**: 1,525 hyperedges. **After**: 3,296 hyperedges. **Delta**: +1,770.

| Type | Count | Source |
|------|-------|--------|
| code/namespace | 102 | Reflection API (280 total, 102 futon-prefixed) |
| code/var | 396 | Reflection API (54 key namespaces) |
| code/ns-contains-var | 396 | Containment edges |
| code/requires | 268 | Reflection deps (futon-internal only) |
| code/file-churn | 1,000 | Git log across 5 repos (1,141 files total) |
| code/indentation-complexity | 770 | File scan across 5 repos |
| meta/enrichment-layer | 1 | Layer 0 metadata record |
| invariant/* | 13 | INV-1: 1, INV-2: 9, INV-4: 3 |

**Cross-futon hotspots** (churn × max-depth, top 5):

| Score | File | Churn | Depth |
|-------|------|-------|-------|
| 3225 | futon3/musn/service.clj | 43 | 75 |
| 2772 | futon3/f2/transport.clj | 42 | 66 |
| 1848 | futon3/musn/http.clj | 28 | 66 |
| 1770 | futon3c/transport/http.clj | 30 | 59 |
| 1728 | futon3/fulab/hud.clj | 27 | 64 |

**Findings**: The top 3 hotspots are in futon3 (source material, expected high
churn). futon3c/transport/http.clj is the highest active-repo hotspot — this is
the main routing file, plausibly the riskiest file in the running system.
futon1a/api/routes.clj (score 1640) is also notable.

**Anomalies**: 0 orphan namespaces (down from previous runs — topology is clean).
Only 1 undocumented namespace. 3 ungrounded math definitions.

**Patterns carried**: [节引示本日]. Layer metadata recorded as evidence [示].
Gradual progress [引] — Layer 0 verified before attempting Layer 1.

## Layer 1 Evidence (2026-03-04)

**Run**: `python3 scripts/ingest-three-columns.py --layer1` (0 errors)

**Before**: 3,296 hyperedges. **After**: 3,718 hyperedges. **Delta**: +421.

| Type | Count | Source |
|------|-------|--------|
| project/mission | 59 | M-*.md scan across 3 repos |
| project/mission-file | 197 | Declaration + git blame (deduped) |
| project/mission-namespace | 165 | Inferred from file paths |
| meta/enrichment-layer | 2 | Layer 0 + Layer 1 records |

**Two-stream provenance**: Stream A (declarative, from `:out` blocks and
backtick file refs) produced 250 edges. Stream B (git blame, `--grep="M-"`)
added 112 new edges after dedup. Together: 59 missions, 362 provenance edges.

**Top missions by file coverage**: M-futon3c-codex (29), M-futon1a-rebuild (12),
M-mission-control (11), M-agency-rebuild (10), M-coordination-rewrite (7).

**Learnings** [日]: Status header format varies across missions — some parse as
`?`. Two-stream dedup works well; declarative provides intent, git provides
actuality. Layer 1 corrections could normalize status values.

## Layer 2 Evidence (2026-03-04)

**Run**: `python3 scripts/ingest-three-columns.py --layer2` (0 errors)

**Before**: 3,718 hyperedges. **After**: 5,092 hyperedges. **Delta**: +1,373.

| Type | Count | Source |
|------|-------|--------|
| project/pattern | 702 | Flexiarg library scan (38 namespaces) |
| project/pattern-reference | 474 | @references cross-links in flexiargs |
| project/pattern-mission | 197 | Pattern name mentions in M-*.md docs |
| meta/enrichment-layer | 3 | L0 + L1 + L2 records |

**Coverage**: 702 patterns catalogued, 122 unique patterns referenced across
missions. Top: social/shapes (9 missions), social/dispatch (8), musn/service
(6), social/presence (5).

**Learnings** [日]: The pattern library is much larger than expected — 702
flexiargs across 38 namespaces. The cross-reference graph (474 edges) is
dense. Some "pattern mentions" in missions are actually code paths
(social/shapes = namespace, not pattern) — the regex filter catches `.clj`
suffixes but bare namespace names still produce false positives. A future
correction layer could distinguish pattern references from code references
using the pattern entity list from this layer.

## Layer 3 Evidence (2026-03-04)

**Run**: `python3 scripts/ingest-three-columns.py --layer3` (0 errors)

**Before**: 5,092 hyperedges. **After**: 5,145 hyperedges. **Delta**: +52.

| Type | Count | Source |
|------|-------|--------|
| evidence/namespace-binding | 12 | Text-scan of evidence body for ns paths |
| evidence/file-binding | 4 | Text-scan for file paths |
| evidence/mission-binding | 36 | Text-scan for M-* references |
| meta/enrichment-layer | 4 | L0 + L1 + L2 + L3 records |

**Coverage**: 24/100 evidence entries (24%) contain code references. 5 unique
namespaces, 2 unique files, 9 unique missions referenced. Most evidence is
unbound chat — the evidence store captures coordination conversations, not
formal PSR/PUR records (zero pattern-selection or pattern-usage type entries).

**Learnings** [日]: This is the thinnest layer yet (+52), and that's itself a
finding. The evidence store is mostly chat turns without explicit code
references. This signals two things: (1) evidence recording should include
structured code references, not just free text; (2) the rational reconstruction
approach [引] correctly surfaces this gap — a bulk import would have hidden it.
The 24% reference rate is a baseline for measuring whether future evidence
practices improve code-awareness.

**Running totals**:
| Layer | Delta | Cumulative | Content |
|-------|-------|------------|---------|
| L0 | +1,770 | 3,296 | Topology + churn + complexity |
| L1 | +421 | 3,718 | Mission provenance |
| L2 | +1,373 | 5,092 | Pattern provenance |
| L3 | +52 | 5,145 | Evidence binding |
| L4 | +175 | 5,321 | Tension surface |

## Layer 4 Evidence (2026-03-05)

**Run**: `python3 scripts/ingest-three-columns.py --layer4` (0 errors)

**Before**: 5,145 hyperedges. **After**: 5,321 hyperedges. **Delta**: +175.

| Type | Count | Source |
|------|-------|--------|
| tension/orphan-namespace | 66 | Code namespaces not claimed by any mission |
| tension/ghost-claim | 82 | Mission→ns edges where ns not in code column |
| tension/mc-code-binding | 3 | MC component tensions matched to namespaces |
| tension/pattern-coverage-gap | 24 | Missions with zero pattern provenance in L2 |
| meta/enrichment-layer | 5 | L0–L4 records |

**Four tension streams**: (A) 66 orphan namespaces — code exists but no mission
owns it, (B) 82 ghost claims — missions reference namespaces not in the
reflection API (mostly futon3 legacy + test namespaces), (C) 3 MC→code bindings
from component name matching (tickle, musn, modeline), (D) 24 missions with no
pattern provenance at all.

**Learnings** [日]: The tension surface is the first layer that is *derived*
rather than *ingested* — it cross-references L0 and L1 to find gaps. The ghost
claims (82) are larger than orphans (66), suggesting missions are aspirational:
they claim namespaces that don't exist yet or that live in other repos. This is
a tension signal, not a bug. The MC→namespace matching only found 3 bindings
because component names (C-arena, C-umwelt) are abstract, not code-path-shaped.
A future enrichment could use devmap manifests to make this binding explicit.

## Layer 5 Evidence (2026-03-05)

**Run**: `python3 scripts/ingest-three-columns.py --layer5` (0 errors)

**Before**: 5,321 hyperedges. **After**: 5,367 hyperedges. **Delta**: +45.

| Type | Count | Source |
|------|-------|--------|
| dep/classpath | 7 | deps.edn `:local/root` scan across 8 repos |
| dep/cross-repo-require | 11 | Existing code/requires edges where repos differ |
| dep/http-api | 27 | Port-number scan (7070/7071/7072/8080) in source+scripts |
| meta/enrichment-layer | 6 | L0–L5 records |

**Three streams**: (A) 7 classpath deps from deps.edn (futon3c→{futon3b,futon1a,futon5},
futon3→{futon2,futon3a,futon1}, futon3b→futon3a), (B) 11 cross-repo namespace
requires (futon3c→futon3: 3, futon3→futon3b: 4, futon3c→futon1a: 1, etc.),
(C) 27 HTTP API deps across 9 repos in the dependency graph.

**I-5 violations detected**: 3 futon3c→futon3 namespace requires:
- `futon3c.bridge` → `futon3.gate.pipeline`
- `futon3c.bridge` → `futon3.gate.util`
- `futon3c.peripheral.real-backend` → `futon3.gate.shapes`

These are real violations of architectural invariant I-5 ("No futon3
Dependencies"). The enrichment pipeline discovered them automatically by
cross-referencing the code column with repo prefix analysis.

**Learnings** [日]: The HTTP API deps (27) are surprisingly rich — more files
reference sibling service ports than use classpath imports. This reflects the
futon stack's microservice topology: repos coordinate via HTTP APIs more than
via direct code imports. The I-5 violations are a concrete, actionable finding
from the enrichment — exactly the kind of signal the rational reconstruction
[引] approach is designed to surface.

**Final running totals**:
| Layer | Delta | Cumulative | Content |
|-------|-------|------------|---------|
| L0 | +1,770 | 3,296 | Topology + churn + complexity |
| L1 | +421 | 3,718 | Mission provenance |
| L2 | +1,373 | 5,092 | Pattern provenance |
| L3 | +52 | 5,145 | Evidence binding |
| L4 | +175 | 5,321 | Tension surface |
| L5 | +45 | 5,367 | Cross-futon dependencies |
| **Total enrichment** | **+3,836** | **5,367** | **All six layers** |

## Enrichment Complete — Phase 1 Summary

All six enrichment layers are now ingested. The store grew from 1,526 (base
three-column ingest) to 5,367 hyperedges — a 3.5x expansion. The enrichment
pipeline is replayable via `--layer0` through `--layer5` flags, and each layer
is recorded as a `meta/enrichment-layer` hyperedge with timestamp, delta, and
anomalies. Next phase: enrichment API endpoint + Arxana browser wiring.

**Checkpoint note**: The layered derivation xenotype (IDENTIFY → MAP → DERIVE →
ARGUE → VERIFY → INSTANTIATE) running in lockstep with the layered enrichment
(L0 → L1 → ...) is a real strength of this run. The mission methodology and
the mission content mirror each other — each enrichment layer goes through its
own mini-derivation cycle, and the layers themselves are the evidence trail.
This is reflect-in-layers [节] operating at two scales simultaneously.

## Phase 2 Checkpoint (2026-03-06)

### Enrichment API — Live

`GET /api/alpha/enrich/file?path=...` is operational against the running
futon3c + futon1a stack. Tested endpoints:

```
GET /api/alpha/enrich/file?path=src/futon3c/transport/http.clj
→ 5 missions (M-cyder, M-futon3c-codex, M-improve-irc, M-mission-control,
  M-transport-adapters), 2 public vars, enrichment-layer 5

GET /api/alpha/enrich/file?path=src/futon3c/agency/registry.clj
→ 1 tension (orphan-namespace: "in code column but claimed by no mission"),
  0 missions, enrichment-layer 5
```

**Bugs fixed during live testing:**
- EDN parsing: futon1a returns `application/edn`, not JSON. Switched
  `cheshire/parse-stream` → `clojure.edn/read-string` (`ae5dcb3`).
- Tensions missing: L4 tension hyperedges were classified but not included
  in the response. Added `:tensions` array to output (`3dcbcbd`).

### Emacs Browser Panel — Built, Not Yet QA'd

`arxana-browser-enrich.el` (futon4) implements `M-x arxana-enrich-file` and
`M-x arxana-enrich-symbol`. Dracula-themed faces for missions (green),
patterns (cyan), tensions (red), symbols (yellow). Renders in side window
slot 2. Committed at `d144e11` but not yet tested interactively against the
live endpoint.

### Infrastructure Fix: Async Invoke

During testing, discovered that `handle-invoke` ran synchronously on
http-kit's I/O threads. A 30+ minute `claude -p` invocation blocked the
entire server — health checks, agent queries, everything timed out.

Fix: 4-thread `invoke-executor` pool. `handle-invoke` and `handle-whistle`
now use `hk/with-channel` + thread pool dispatch. http-kit threads stay
free for all other endpoints (`50ed259`).

### Remaining for Phase 2

- **Interactive QA**: load `arxana-browser-enrich.el`, run `M-x arxana-enrich-file`
  on a live buffer, verify rendering in the side panel
- **Churn data**: currently `null` — churn hyperedges are keyed by file path
  (not namespace endpoint), so the endpoint-based query misses them. Needs
  a secondary query by file path or a churn-specific lookup.
