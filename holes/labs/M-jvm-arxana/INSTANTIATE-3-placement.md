# INSTANTIATE increment 3 — placement record (recorded before coding)

Mission: `M-jvm-arxana`. Date: 2026-11-06. Owner: kimi-3.

## Decision

The headless JVM Arxana service spine lives in **futon3c** as additive
namespaces:

- `src/futon3c/arxana/digest.clj` — canonical serialization + SHA-256 projections
- `src/futon3c/arxana/envelope.clj` — arxana.v1 envelope validation,
  unknown-field refusal, typed error algebra, D5 semantic/error projections
- `src/futon3c/arxana/schema.clj` — versioned operation schema registry (v1)
- `src/futon3c/arxana/store.clj` — D1 `ArxanaGraphStore` protocol + honest mock;
  conditional persistence stubbed (kimi-2 owns the futon1b route, increment 2)
- `src/futon3c/arxana/artifact.clj` — D2 `ArtifactProvider` + in-memory
  provider; whole-artifact content-addressed revision identity; client-preview
  gating
- `src/futon3c/arxana/service.clj` — capabilities handshake + envelope dispatch
- `test/futon3c/arxana/spine_test.clj` — focused tests

## Options considered

| Home | Verdict |
|---|---|
| futon3c | selected: the live JVM/Clojure service repo; malli/cheshire/http-kit and cognitect test-runner already on classpath; futon1b already a `local/root` dep so the D1 adapter can reach the real store in later increments; additive namespaces disturb nothing |
| futon1b | rejected: kimi-2 owns the futon1b conditional-transaction route there; the spine must stub persistence behind the adapter, not co-locate with in-flight route work |
| futon4 | rejected: mission design docs live here but the repo is Emacs-Lisp-first with no Clojure build; adding a JVM build would be more disruptive |
| new repo | rejected: requisition asked for the least disruptive *existing* home |

## Scope held

Increment 3 only: envelope validation/unknown-field refusal, capabilities
handshake, D1 adapter interface (mock first), ArtifactProvider with
whole-artifact canonical revision identity, versioned operation schema
registry, canonical semantic/error digest projection. No D2 resolution, no D4
correspondence, no clients, no deployment. Conditional persistence is a typed
`:unsupported` stub behind `conditional-write!` pending kimi-2's futon1b route.
