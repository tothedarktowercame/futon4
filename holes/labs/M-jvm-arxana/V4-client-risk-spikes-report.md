# V4 — client-risk spikes

Mission: `M-jvm-arxana`. Date: 2026-11-06.

## S3 — proof-profile query shape at corpus scale

`verify-query-spike.clj` loaded an isolated XTDB 2.1.0 node with 2,454 proof
moves across 347 problems and 636 manifestations, matching the largest
observed prelim-practice counts.  It exercised two stable query shapes:

1. every revision-pinned manifestation for a supplied move ID; and
2. the first absent or ambiguous move using a client-supplied ordinal.

Observed after one warm-up of each query shape:

```edn
{:fixture {:moves 2454, :problems 347, :manifestations 636}
 :all-manifestations
 {:milliseconds 13.679512
  :value [{:xt/id "manifestation:0017"
           :manifestation/role :lean-source
           :proof/move-id "move:0017"}]}
 :next-unresolved
 {:milliseconds 20.414167
  :value [{:correspondence/state :ambiguous
           :proof/ordinal 17
           :proof/move-id "move:0017"}]}
 :checks {:all-manifestations-correct? true
          :domain-order-preserved? true}}
```

The bounded query waist is a materialized evidence-derived move-status record.
Arxana derives only `present`, `absent`, or `ambiguous`; the domain client owns
and supplies the move ordinal.  Thus the index does not turn Arxana into the
owner of pedagogical ordering or scoring.  Production parameterization,
pagination and mutation-driven projection maintenance remain INSTANTIATE
tests, not open query semantics.

Verdict: **pass at query-shape/feasibility level**.  This discharges V2's
`requires-spike`; it does not claim a performance service-level objective from
one in-process run.

Reproduce:

```sh
cd /home/joe/code/futon1b
clojure -M:node -e '(load-file "/home/joe/code/futon4/holes/labs/M-jvm-arxana/verify-query-spike.clj")'
```

## S4 — client segment canonical bytes

The code-server review found that a JSON string serialization and the file
bytes it represents have different hashes.  A UTF-8 example produced:

```edn
{:artifact "814389c3354002581eff74753c6d8a43474e737b2b7f5f683c7f81e8acb1aa28"
 :serialized "b6bc1748e20e941e2649c9ff47ab59312f95f80e372b1cf2a9773bc78ee37646"
 :equal false}
```

D7 now fixes the identity rule: `Segment.artifact/revision-id` names the whole
artifact revision over authority-declared canonical bytes.  A segment hash may
check extracted bytes but cannot mint that revision.  Until authority ingest
returns the revision, the client is preview-only.  This removes the ambiguity
without inventing an editor-specific canonicalization.

Verdict: **pass by counterexample plus specification decision**.  Executable
ingest/round-trip coverage remains INSTANTIATE.
