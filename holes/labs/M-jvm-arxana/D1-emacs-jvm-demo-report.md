# DOCUMENT D1 — bounded Emacs/JVM Arxana demonstration

Date: 2026-11-06. Mission: `M-jvm-arxana`.

## Outcome

PASS locally. A separate `futon4a-demo` Emacs daemon consumed the loopback
`arxana.v1` JVM surface. The legacy futon4 code and live stores were not
modified.

The live client displayed:

- the advertised operation/capability truth, including `action/compile`;
- fixture scope resolution `:resolved`, one verified-hash candidate, semantic
  digest `0397248a041571f99764c26675c498f57971bbfbc9ca6391211e199eaa5cff8c`;
- one partial, directional LaTeX→HTML correspondence, semantic digest
  `9c91f35ee33414db14fe93b7a2bbbaff3eda8fc496bca06624b6538f80070d00`;
- a write-qualified `:stale-refused` result with refresh,
  rebase-and-resubmit and export-patch recovery choices.

## Executed evidence

From `futon3c`, the combined acceptance command selected store HTTP, journal,
subscription, HTTP, demo transcript, spine, Resolve, Correspond, fixture,
Resolve↔Correspond, Compile and Compile-service namespaces. Result: **67 tests,
352 assertions, zero failures/errors**. The store test started an embedded
real futon1b server and proved conditional commit, message-id idempotency and
typed stale refusal across the HTTP adapter.

From `futon4a`, the ERT command
`emacs -Q --batch -L lisp -l test/futon4a-arxana-test.el
-f ert-run-tests-batch-and-exit` produced **7/7 expected**. The seventh test is
the live-discovered CRLF HTTP-header regression.

The committed transcript was regenerated after Compile became public and is
checked by both JVM and Emacs tests.

## Store-boundary qualification

The public fixture demo currently uses an in-memory graph projection for
strategy, correspondence, journal and subscription reads. It reports
`:entity-compare-and-set false`; it does not simulate atomicity. The separate
`HttpStore` acceptance uses the authenticated futon1b conditional route and
reports only the one capability it actually implements. Thus every boundary
is exercised, but the demo is not yet a single deployed process graph in which
all projection reads and writes terminate at a remote futon1b. Claiming that
would be false; broadening the adapter is follow-on hardening, not required to
demonstrate the semantic JVM authority.

## Reproduction and rollback

Use `futon4a/scripts/check-demo-ports.sh`, then
`futon4a/scripts/launch-demo.sh`. Connect with
`emacsclient -s futon4a-demo -c`; the commands are prefixed
`futon4a-arxana-`. Stop with `futon4a/scripts/stop-demo.sh`. The launcher binds
only loopback and refuses occupied ports. No futon4 file is loaded or edited.

## Isolated lucy replication

PASS on 2026-11-06. A fresh census found `7084`, `7085` and `6778` free.
Commit-addressed bundles were cloned under
`/home/joe/demo/m-jvm-arxana-20261106`; Lucy's existing checkouts were not
updated. The staged heads were futon1b `130b13c`, futon3c `09d5ca6cc`,
futon4a `29537c2` (plus the copied, subsequently committed Compile example),
and futon5 `bb0b69b`, with the fixture-gate object `17dc53a` transferred as a
separate bundle ref.

Two startup refusals improved the deployment recipe without touching live
services: the isolated classpath initially lacked futon3's `inbox-zero-lib`,
and standalone futon1b attempted Drawbridge without an admin token. The release
was completed with its own copied local dependency and
`FUTON1B_DRAWBRIDGE_PORT=0`.

The remote run then demonstrated:

- authenticated futon1b conditional ingest on `127.0.0.1:7085`, producing
  revision `c4f335b0…`, followed by HTTP 409 `:arxana/stale-head` with all
  three recovery choices;
- JVM Arxana on `127.0.0.1:7084`, advertising `action/compile`;
- Emacs capability, Resolve, Correspond and typed-refusal views with the same
  semantic digests as the local/committed transcript;
- the public Compile request returning a deterministic, non-executable
  candidate and semantic digest `59bbc5db…`;
- 7/7 ERT tests on Lucy.

The Compile wire request returned `:conditional`, despite carrying a named
human interpreter and apparent trust-map entry. This is safe—no untrusted
interpretation became `:compiled`—but the dynamic interpreter-key encoding
across tagged JSON should be audited before using the HTTP example as evidence
for the trusted-Compile branch.

All temporary processes were stopped after capture. Ports `7084`, `7085` and
`6778` had no listeners, and the `futon4a-demo` Emacs socket was absent. The
isolated release and evidence under its `run/` directory were retained for
inspection; no service was installed or left running.
