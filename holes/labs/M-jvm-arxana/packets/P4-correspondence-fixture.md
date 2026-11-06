# P4 — first cross-manifestation correspondence fixture

Mission: `M-jvm-arxana`. Phase: MAP. Intended worker: one Zai seat.

Discovery and fixture recommendation only. Do not regenerate the paper, edit
LaTeX, change the WYSIWYG implementation or invent correspondence IDs.

## Question

Which stable identities survive the current LaTeX-to-HTML path, where are only
offsets or quotes available, and which existing artifact pair can serve as the
first correspondence fixture?

## Work

1. Read `futon3c/holes/missions/M-latex-wysiwyg.md`,
   `futon3c/emacs/latex-wysiwyg.el`, the Oxide build scripts it uses, and the
   relevant `web/scopes` fixture/adapter records.
2. Trace one prose paragraph, one section title and one mathematical expression
   from LaTeX source through Oxide HTML/source positions into the current
   navigation or scope representation.
3. Record what survives: file identity, revision/hash, source range, quote,
   DOM identity, semantic identity, generated/macro status and editability
   class. Say `absent` rather than inferring a field.
4. Compare the Oxide candidate with one simpler Elisp-definition strategy as a
   first fixture. Recommend one based on reproducibility and what mission risk
   it actually tests, not visual appeal.

## Output and acceptance

Write `holes/labs/M-jvm-arxana/P4-correspondence-fixture-report.md` with the
three traces, a preservation matrix, exact artifact paths/hashes where
available, the two-candidate comparison and one recommendation.

Acceptance: the recommendation identifies an existing source and derived
artifact that another worker can inspect without rebuilding. Commit only the
report with message `M-jvm-arxana MAP P4: ...`.
