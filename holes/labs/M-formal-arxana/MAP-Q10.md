# MAP Q10 — Demonstration surfaces (linear / graph-GCC / geometric, phone access, deterministic replay)

## Answer

Live today (GET-probed 2026-11-07): the Zone serves `zone.hyperreal.enterprises`
over Caddy :443 (admin :2019) with path-prefixed routes — `/wip/stepper/*`
(static docroot `/var/www/zone.hyperreal.enterprises/wip/stepper/` + proxied
GET `:7070/api/alpha/xiang/*`, `…/agents`, `…/cascade-real/graph`, basic-auth
realm "xiang"), `/marimo/` (marimo edit, :2718), `/marimo-view/` (:2719,
`turn-counts.py`), `/marimo-room/` (:2720), `/chatgpt-vnc/` (websockify :6080),
`/arxana-code/` (code-server :8097), `/wysiwyg/` (:8129/8130). All are
phone-accessible HTML (stepper and zone index carry mobile viewport meta);
`/wip/stepper/` and the 象 turn feed (`GET :7070/api/alpha/xiang/turns`, 200,
records present) are the cheapest **linear** surface — stepper is a static
HTML/JS pager over stored turns/frames with `?session=`/`?agent=` replay.
**Graph/GCC**: `GET :7070/api/alpha/cascade-real/graph` returns live
nodes+edges JSON (200), but no phone-rendered graph view serves it;
WebArxana (`futon4/dev/web/webarxana/`, port 3100, `/wa` SPA + `/api/interest-network`
projection) is implemented with Playwright specs but **not running** (:3100
closed, no unit) — starting it is the cheapest graph surface.
**Geometric**: nothing running; WYSIWYG is prose-LaTeX editing, not geometry;
the cheapest substrate is a marimo notebook (headless marimo already runs
deterministically from committed `.py` files). Emacs REPL is desktop-only,
not phone-accessible.

## Evidence

- Caddy live config (read-only): `curl http://localhost:2019/config/` —
  host `zone.hyperreal.enterprises`, listen `:443`, routes above
  (`/wip/stepper/api/alpha/xiang/*|…/agents|…/cascade-real/graph ->
  127.0.0.1:7070`, root `/var/www/zone.hyperreal.enterprises`;
  `/marimo/* -> 2718`, `/marimo-view/* -> 2719`, `/marimo-room/* -> 2720`,
  `/chatgpt-vnc/* -> 6080`, `/arxana-code/* -> 8097`, `/wysiwyg/* -> 8129`).
- Running state: `ss -tlnp` shows 7070, 7073, 6080, 8129/8130, 2718/2719/2720
  listening; **no 3100** (WebArxana down). `systemctl list-units` has
  `caddy.service` only; no webarxana unit.
- Probes: `GET https://zone.hyperreal.enterprises/` → 200, mobile viewport
  meta; `/wip/stepper/` → 401 without basic auth (reachable, realm "xiang");
  `GET http://localhost:7070/` → 404 JSON ("Unknown endpoint: GET /");
  `GET :7070/api/alpha/xiang/turns` → 200 with stored turns (first entry is
  this Q10 requisition turn); `GET :7073/api/alpha/hyperedges…` (Q1) live.
- Stepper UI: `/var/www/zone.hyperreal.enterprises/wip/stepper/index.html`
  head — mobile viewport, keyboard pager, `?agent=`/`?session=` live-follow,
  "Without either, a fixture" (fixture mode = deterministic replay).
- 象 routes: `futon3c/src/futon3c/transport/http.clj:12741–12792` (GET
  `/api/alpha/xiang/turns`, `/scopes`, `/frames`, `/frames/stream`,
  `/agreement`, `/health`, per-turn GET); graph:
  `http.clj:13204` (`GET /api/alpha/cascade-real/graph`, docstring
  "per-section STRUCTURE (nodes+edges)" at :9888) — live 200, JSON arrows
  with have/want/move-class/delta-g.
- WebArxana: `futon4/dev/web/webarxana/src/webarxana/server/core.clj:94–131`
  (routes: `/api/interest-network` projection, `/api/mission-search`, `/futon/*`
  proxy, `/wa` SPA), default port 3100 (line 49); Playwright specs exist
  (`tests/two-users.spec.ts`, `interest-network-verify.mjs`) —
  implemented-and-tested, currently not running.
- Marimo: `ps -p 1392/1389/1385` — `marimo run turn-counts.py --headless
  --base-url /marimo-view`, `matrix-room-posts.py --base-url /marimo-room`,
  `marimo edit --base-url /marimo` (notebooks under
  `marimo-zone/notebooks/*.py`, committed files = deterministic replay).
- Caddy `/vsatarcs` redirects to `https://…:8443/demos/` — port 8443 not
  listening (000): prose/legacy, dead.
- Emacs REPL: `futon4/dev/arxana-store.el` (Q1) — local editor surface; no
  phone path, no replay surface beyond stored records.

## Ready / Missing

| Projection | Ready — no new code | Missing — actual work |
|---|---|---|
| Linear | stepper static UI + 象 turn/frames GETs, live, phone-OK, fixture-replay mode | replay pinning (does `?session=` re-render byte-identical? untested) |
| Graph/GCC | `cascade-real/graph` JSON live; WebArxana `/wa` + interest-network implemented w/ tests | WebArxana not running (start + expose through Caddy); no live viewer of cascade-real/graph |
| Geometric | marimo headless serving committed notebooks, phone-OK | no geometric (unit-circle/fibre) view of records exists — notebook or canvas view is new work (F5) |
| Phone access | Caddy TLS + basic-auth per path; stepper/zone index mobile viewport | none observed (WebArxana down is the only phone gap) |
| Deterministic replay | marimo (`marimo run <committed .py>`), stepper fixture mode, content-addressed artifact revisions (Q1) | replay guarantee for live-follow views (frames cache, `?session=`) unproven |

Cheapest per projection: linear = stepper; graph = start WebArxana (:3100) and
reuse `/api/interest-network`; geometric = a marimo notebook reading
hyperedge/assertion records via :7073 GETs.

## Open doubts

- `/wip/stepper/` was probed unauthenticated (401); its rendering behind
  basic auth was not visually verified from a phone-sized client.
- Whether `?session=` replay is byte-deterministic (frames cache freshness,
  `g` re-read) was not tested — only the fixture-mode claim in index.html.
- WebArxana start command/ownership (who starts :3100, from which REPL) not
  established — README-rendering-pipeline covers the shadow-cljs build, not
  server lifecycle.
- 3100 might be intended as loopback-only with a Caddy route to be added;
  no `/wa` route exists in the live Caddy config today.

## Review addendum (claude-13, 2026-11-07)

WebArxana has an owner and a start path: it runs inside the futon3c JVM,
started by `start-webarxana!` (futon3c `dev/futon3c/dev/bootstrap.clj:318-338`,
called at :631 inside a try/catch that only prints on failure), gated by
`FUTON3C_WEBARXANA_SERVER_AUTOSTART` (default true) on port
`FUTON3C_WEBARXANA_PORT` (default 3100). futon4/CLAUDE.md forbids a second
long-running JVM for it. So ":3100 not listening" means the start failed or
was disabled in the serving futon3c JVM, not that a unit is missing. The
"cheapest graph surface" is therefore: find why `start-webarxana!` did not
bind in the running futon3c JVM and start it there (via Drawbridge, from the
master checkout), not start a separate process. Why it is down was not
checked.
