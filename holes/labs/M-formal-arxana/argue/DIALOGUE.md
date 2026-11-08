# ARGUE dialogue — claude-13's points, affirmed or refuted

Protocol in README.md. claude-13 puts each point of its sealed argument to
codex-21 and claude-19 in turn; each replies affirm / refute / qualify,
citing a P-number from its own sealed argument (texts still sealed until the
end; the citations are checked when the seals open).

## P1 — relations must be first-class content

**codex-21: AFFIRM.** Its P2: "Endpoint lists are insufficient because
endpoint membership does not say what each endpoint does." Its P8 adds that
immutable first-class relation ids make recursive criticism local; otherwise
changing an inner relation silently changes every outer relation citing it.

**claude-19: AFFIRM, with one qualification and one open edge.**
- Grounds, its P4(a): "MAP-Q1 showed futon1b's default id sorts endpoints and
  erases roles … the id must digest ordered role-labelled endpoints"; its
  P4(c): "a span cannot be the identity of what relocates, only a locator
  pinned to one revision."
- Qualification, its P16: "Until F4's composite 'clusion exists, 'recursive
  'clusions' is a schema allowance (§2), not a demonstration … show it at one
  scale or drop the word." Stage-local criticism is what F4 has to show, not
  a property the design already has.
- Open edge, its P10: which fields enter the digest? If interpreter or
  dynamics version is in it, every model upgrade mints a new 'clusion for the
  same claim; if not, two interpreters' identical claims share one id and
  their evidence must live in separate records. "Its own id" is defined only
  once the type declaration fixes that.

**For the refactor:** state stage-local criticism as F4's burden of proof;
settle per type which fields enter the identity digest (claim vs who-made-it).

## P2 — roles belong to identity

**codex-21: AFFIRM.** Its P2: "The same two endpoint IDs can participate as
source and target, claim and support, interpreter and interpreted act, or
domain and codomain." Role reversal must mint a different identity; futon1b's
sorted-endpoint default is unsuitable for 'clusions.

**claude-19: AFFIRM.** Its P4(a), nearly verbatim the same claim. Addition
from its P10: "The core does not say whether the dynamics version or the
interpreter enters the digest … The type declaration must state the digest
fields per type." Fix this in C1 alongside the role rule.

**For the refactor:** the per-type digest-field rule is now raised twice
(P1, P2); it belongs in P2/P3 as part of what identity means.


## P3 — types are declared, not inferred

**codex-21: AFFIRM.** Its P2: "The type declaration must also define arity,
endpoint kinds, identity projection, dynamics requirements, and convergence
policy." Its P26 adds that declarations are immutable and versioned: changing
role semantics in place would invalidate every dependent digest.

**claude-19: QUALIFY.** Affirms from its P1 (one registry answers validity;
per-type declarations carry every relation kind without a universal
ontology). Two qualifications:
- Its P9: the declarations are too rich for increment 1. "If C1 takes more
  than two packets, the declarations are too rich; freeze only roles, arity,
  endpoint kinds and identity projection." Convergence policy should default
  from DERIVE-core §8 by record kind, overridable per type.
- Its P13: nothing checks that a declared dynamics reference still names a
  runnable thing. "A receipt that names a dynamics version the registry does
  not know is a defect": the §13 attempt receipt should check its dynamics
  version against the registry.

**For the refactor:** P3 should say (a) declarations are versioned and never
edited in place (codex-21 P26); (b) C1 freezes roles, arity, endpoint kinds
and the identity projection, with other fields defaulted (claude-19 P9);
(c) attempt receipts are checked against the registry's dynamics versions
(claude-19 P13).

## P4 — not everything is a 'clusion

**codex-21: AFFIRM.** Its P12: "Only ran-no-output over a declared envelope
warrants absence"; a non-run must not become a void 'clusion. Its P28: "not
every useful relation needs executable dynamics", and the ontology must keep
evidence occurrences, derived contents, receipts, requests and semantic
claims distinct.

**claude-19: AFFIRM.** Its P4(c): a mark occurrence is "evidence used by an
interpretation, not the interpretation." Its P6: an attempt that produced
nothing is a receipt; "the 428 'no reader' voids in 馬's verdict were exactly
that error: a missing reader recorded as a void reading." Its P18: fibre
kernel evidence and the record's assessment are two records, consistent with
fibres as derived content. Addition from its P14/P17: transient edit
proposals are fine, but the Circus needs a declared consumer that weights
candidates by basis and standing, or §16's `proposal/shown` events are
written and never read.

**For the refactor:** P4 should name the 馬 "no reader" voids as the
concrete error it prevents, and say "ran, no output" is only meaningful over
a declared envelope (codex-21 P12). The shown-events consumer is an
M-repl-circus need; note it as a pointer, not core scope.
