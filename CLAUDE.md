# futon4

Arxana hypertext system — Emacs browser, web surfaces (WebArxana), and data access layer.

## Critical rules

- **ONE JVM IS PLENTY.** The futon3c JVM hosts everything that serves —
  WebArxana app on port 3100, the WebArxana server code from
  `dev/web/webarxana/src/webarxana/server/` (started from futon3c's
  bootstrap via `requiring-resolve`), substrate-2 on 7071, futon3c API
  on 7070, Drawbridge on 6768. Do NOT start a second long-running JVM
  for WebArxana. The only acceptable second JVM here is a *temporary*
  `npx shadow-cljs watch app` for active CLJS editing; it serves no
  request path (futon3c serves the compiled `resources/public/js/main.js`
  as a static file) and should be killed when you stop editing CLJS.
  See `futon3c/CLAUDE.md` I-0 for the full invariant.
- **NEVER kill or restart the futon3c serving JVM.** Use Drawbridge
  nREPL (`futon3c/scripts/proof-eval.sh` or `futon3c/README-drawbridge.md`)
  for code reload.
- WebArxana CLJS frontend lives in `dev/web/webarxana/`. Build with
  `npx shadow-cljs compile app` (or `release app` for optimised
  output) from that directory. The watch mode (`shadow-cljs watch app`)
  is a *dev tool*; stop it when you're not editing CLJS.
- The Emacs Arxana Browser (`dev/arxana-browser-*.el`) and data layer
  (`dev/arxana-store.el`) are the reference implementation for data
  access patterns. The Mission Portfolio view talks to futon3c on port
  7070, not futon1a — see `arxana-mission-control-server` defcustom
  in `dev/arxana-browser-missions.el`.
- Mission docs live in `holes/missions/M-*.md` and follow the lifecycle
  in `holes/mission-lifecycle.md`.
- **Ingesting an essay into Arxana Essays? Read `README-essays.md` first.**
  It documents the non-obvious file-role convention: the source `.md` is
  canonical and read-only (annotations are a *render*, never baked into the
  body); `annotations.edn` is the authoritative annotation layer (verbatim
  `:passage` anchors); `annotations.el` is a thin sections-only manifest that
  the loader augments from the `.edn`. Section headings must be flat `##`
  (nesting lives in the manifest `:index`). Verify any ingest with `M-x
  arxana-browser-essays-audit-passages` before trusting it.
