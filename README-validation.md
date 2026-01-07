# Validation (Reazon-based)

This repo includes optional, Reazon-backed validators for window layout and data
invariants. They are disabled by default, require a restart, and only run when
Reazon is available on `load-path`.

## What exists

Two modules provide validators:

- `dev/arxana-window-constraints.el`
  - Scholium layout: source left of display; docbook adds code pane.
  - Code/docs split: docs window is a side window with configured width.
  - Org hover preview: side window on the right with configured width.
  - Docbook two-up: doc left of source, both dedicated.
  - Docbook browser-left: browser left of docbook, docbook dedicated.
  - Pattern HUD: window has `futon-hud-owner` and configured width.
  - UI focal: single focal managed window at leftmost edge.

- `dev/arxana-data-constraints.el`
  - Link strategy shape.
  - Voiced link shape.
  - Surface form shape.
  - Hyperedge inputs (hx/type + endpoints).
  - Inclusion/transclusion inputs.
  - Flexiarg edit spans stay within one file.
  - Media bounce inputs (tracks, script, path matches).

Both modules are loaded from `dev/bootstrap.el` if present.

## Enabling in a new Emacs session

Add the following to your init (or evaluate after loading `dev/bootstrap.el`):

```elisp
(setq arxana-window-constraints-enable t)
(setq arxana-data-constraints-enable t)
```

Optional reporting behavior:

```elisp
(setq arxana-window-constraints-failure-action 'message) ; or 'error or nil
(setq arxana-data-constraints-failure-action 'message)   ; or 'error or nil
```

## Reazon availability

The validators load Reazon dynamically. If the submodule lives at
`dev/vendor/reazon`, `dev/bootstrap.el` will add it to `load-path`.

You can verify availability with:

```elisp
(require 'reazon)
```

## Testing (manual)

1) Start a fresh Emacs session.
2) Load the repo bootstrap:
   ```elisp
   (load-file "/home/joe/code/futon4/dev/bootstrap.el")
   ```
3) Enable validators (see above).
4) Exercise the relevant workflows:
   - Scholium display/hover, code docs split, org hover, docbook views.
   - Links creation, surface capture, hyperedge posts, inclusion/transclusion.
   - Flexiarg edits and media bounce.
5) Watch `*Messages*` (or expect errors if you set failure action to `error`).

## Notes

- Validators are opt-in and intended to help surface drift or regressions.
- They are intended to be cheap and non-invasive; adjust timeouts if needed:
  ```elisp
  (setq arxana-window-constraints-timeout 0.2)
  (setq arxana-data-constraints-timeout 0.2)
  ```
