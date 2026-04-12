# futon4

Arxana hypertext system — Emacs browser, web surfaces (WebArxana), and data access layer.

## Critical rules

- **NEVER kill or restart running JVM server processes** (futon1a, futon3c, webarxana server, etc.). Use Drawbridge nREPL for code reload where possible.
- WebArxana lives in `dev/web/webarxana/`. Build with `npx shadow-cljs compile app` from that directory.
- The Emacs Arxana Browser (`dev/arxana-browser-*.el`) and data layer (`dev/arxana-store.el`) are the reference implementation for data access patterns.
- Mission docs live in `holes/missions/M-*.md` and follow the lifecycle in `holes/mission-lifecycle.md`.
