# Evidence Landscape Viewer

The Arxana Labs evidence viewer renders evidence entries from the futon3c
evidence landscape (stored in futon1a via XTDB, served over HTTP).

## Accessing the Viewer

In Emacs: `M-x arxana-browser` -> Lab -> Evidence Timeline

Or: Lab -> Evidence by Session (grouped view)

## Timeline Columns

| Column | What |
|--------|------|
| Time | Timestamp of evidence entry |
| Type | Short label (PSR, PUR, PAR, GATE, COORD, etc.) with face color |
| Author | Who produced the evidence |
| ↳ | Reply indicator — shows "↳" if entry is a reply in a chain |
| Preview | One-line body summary (e.g., "query → selected" for PSR) |
| Subject | Artifact reference (type:id) |

## Type-Specific Body Rendering

When you open an evidence entry (RET on a timeline row), the body is
rendered with type-aware formatting instead of raw plist dumps:

- **PSR (pattern-selection)**: Query, Selected (clickable pattern link),
  Sigil, Confidence, Rationale section, Candidates (clickable pattern links)

- **PUR (pattern-outcome)**: Outcome, Actions (as list if multiple),
  Expected, Actual, Prediction Error, Notes section

- **PAR (reflection)**: Patterns Used (clickable links), What Went Well,
  What Could Improve, Prediction Errors, Suggestions, Commits,
  Files Touched (clickable code links)

- **Peripheral events**: Dispatches on event type (start/step/stop).
  Steps show Tool, Arguments, Result. Stops show Fruit and Reason.

- **Generic maps**: Any plist body renders as structured key-value pairs
  with nested structures shown as sub-items.

## Filtering

In the Evidence Timeline view:

| Key | Action |
|-----|--------|
| `F` | Filter by evidence type (completing-read with all types) |
| `g` | Refresh (re-fetch with current filter) |

Filter is shown in the header line. Selecting "(all)" clears the filter.

The filter commands are also available as:
- `M-x arxana-browser-evidence-filter-by-type`
- `M-x arxana-browser-evidence-filter-by-author`
- `M-x arxana-browser-evidence-clear-filter`

## Evidence Link Navigation

`[[evidence:ID]]` links in detail views are now clickable. Clicking
fetches the entry from `/evidence/{id}` and opens it in the detail viewer.

In an evidence detail buffer (`*Evidence:...*`):

| Key | Action |
|-----|--------|
| `C` | View reply chain for this entry |

## Configuration

The viewer reads from the futon1a HTTP API. Configure the endpoint:

```elisp
(setq arxana-lab-futon1-server "http://localhost:8080/api/alpha")
```

This defaults to `$FUTON1_API_BASE` or `http://localhost:8080/api/alpha`.

## Architecture

```
arxana-lab.el          — Body renderers, preview helper, evidence links, detail views
arxana-browser-lab.el  — Timeline/session views, filter commands, HTTP fetch, link navigation
arxana-browser-core.el — Declarations, keymap wiring, header lines
```

The body rendering dispatches first by evidence type string (from
`:evidence/type`), then by body shape as fallback (e.g., a body with
`:query` and `:selected` keys renders as PSR even without the type field).
