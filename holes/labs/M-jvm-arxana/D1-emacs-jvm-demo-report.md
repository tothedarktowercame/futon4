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
