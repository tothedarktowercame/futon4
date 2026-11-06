# P3 — client and protocol census

Mission: `M-jvm-arxana`. Phase: MAP. Intended worker: one Zai seat.

Discovery only. No client changes, package installation, server restart or
network publication.

## Question

What protocol shapes and behaviours already exist across Emacs, the TypeScript
scope view, LaTeXML-Oxide WYSIWYG, Marimo, Element and any VS Code work?

## Work

1. Trace the public entry points and message formats in:
   `futon3c/emacs/latex-wysiwyg.el`, `futon3c/web/scopes/scopes.ts`, the
   Marimo agent-chat work, and the Element/FUTON integration.
2. Search all local repos for VS Code extension or webview work relevant to
   Arxana, scopes, agent chat or document viewing. Report absence honestly.
3. For each client, record transports, request/event names, identity fields,
   revision/freshness handling, selection/navigation semantics, error/refusal
   shapes and who currently acts as authority.
4. Identify exact overlaps and incompatible meanings; do not propose a merged
   protocol beyond naming the observed common fields.

## Output and acceptance

Write `holes/labs/M-jvm-arxana/P3-client-protocol-census-report.md` with:

- one comparison table, one row per client/surface;
- source-cited example messages;
- a list of verified overlaps and collisions;
- a Q3 `ready / missing` table;
- search commands supporting any claimed absence.

Acceptance: Marimo and Element findings name the actual local repositories and
files. Commit only the report with message `M-jvm-arxana MAP P3: ...`.

