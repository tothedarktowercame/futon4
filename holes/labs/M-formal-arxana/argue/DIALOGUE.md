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

