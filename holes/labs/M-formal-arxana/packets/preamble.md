# M-formal-arxana MAP — survey packet (from claude-13)

You are answering ONE survey question from the MAP phase of
`/home/joe/code/futon4/holes/missions/M-formal-arxana.md`. Read its HEAD
(including "Why this mission, now") and §1 IDENTIFY first, then §2 MAP.

MAP produces **facts, not designs**. Do not propose the final schema, do not
write implementation code, do not edit the mission file.

Rules:
- Every claim cites a file:line, a commit sha, or a command you ran with its
  output (trimmed). Say "not found" with the search you used when absent —
  and note that `^def ns.name` greps miss in-namespace `def name`, so search
  more than one way before concluding absence.
- Read-only against live services. You may GET from :7070 (futon3c Agency)
  and :7073 (futon1b API). Do NOT POST/PUT to live stores, do not reload code
  into the shared JVM, do not restart anything. If a question needs a write
  probe, run it against a hermetic store (an existing test fixture or a
  `clojure -M:test` run in the repo's own process) and say which.
- Do not write commit trailers (Agent-*, Agency-Job, Dispatched-By,
  Co-authored-by): a hook adds the correct ones.
- No work in /tmp beyond throwaway scratch.
- Distinguish: implemented-and-exercised (test or live record shows it),
  implemented-but-unexercised, and prose-only (described in docs, no code).

Output: write exactly ONE file,
`/home/joe/code/futon4/holes/labs/M-formal-arxana/MAP-<Qn>.md` (the Qn named
below), with sections: Answer (≤15 lines), Evidence (cited), Ready — no new
code / Missing — actual work (two-column table rows for the mission's MAP
table), Open doubts. Then commit that one path only in futon4
(`git add <that path>`; never `commit -a`, never `--amend`, never `stash`:
the checkout is shared).

When done, bell claude-13 back with a 5-line summary and the commit sha.
