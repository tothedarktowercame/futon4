# Futon Docbook Workflow

This README captures the current docbook workflow used by Futon3/Futon4 so
we can later promote it into the docbook itself.

## Summary

- Docbook entries live as Org stubs under `futon4/docs/docbook/<book>/*.org`.
- Headings and entries are stored in Futon1 XTDB via the docbook API.
- Invariants enforce heading/entry linkage and non-empty bodies.
- The Arxana browser shows a docbook contents view and entry views.

## Required stub structure

Each docbook stub is an Org file with a property drawer. Minimum keys:

```
#+TITLE: <Title>
:PROPERTIES:
:DOC_ID: <book-hash>
:ENTRY_ID: <doc-id>::org
:VERSION: org
:OUTLINE_PATH: <Outline / Path>
:PATH_STRING: <Outline / Path>
:END:
```

Notes:
- `ENTRY_ID` must not equal `DOC_ID` to avoid overwriting the heading entity.
  Use `DOC_ID::org` (or another unique entry id).
- Body content lives below the property drawer; it becomes `doc/body`.

## Emacs workflow (Arxana)

- Browse docbooks: `M-x arxana-docbook-browse` (or via Arxana browser).
- Sync TOC order: `C-c C-s` in the docbook contents view.
- Ingest a book from stubs: `M-x arxana-docbook-ingest-book`.

Docbook sync requires:
- `futon4-enable-sync` set to non-nil.
- `futon4-base-url` set (for example `http://localhost:8080`).

## CLI ingest (alternative)

Use the Futon1 ingestion script (reads `spine2.org` + includes):

```
cd /home/joe/code/futon1/apps/graph-memory
DOCBOOK_ROOT=/home/joe/code/futon4 clojure -M:scripts/docbook-ingest --book futon3
```

## Verifying invariants

Docbook invariants live in Futon1 and can be checked via:

```
cd /home/joe/code/futon1/apps/graph-memory
clojure -M -m scripts.model-docbook-verify
```

## Deleting a broken doc-id

If an entry overwrote its heading (entry-id == doc-id), delete the doc-id:

```
BASE="http://localhost:8080/api/alpha"
BOOK="futon3"
DOC_ID="futon3-xxxxxxxxxxxx"

curl -s -X DELETE "$BASE/docs/$BOOK/doc/$DOC_ID"
```

Re-ingest afterward.

## Notes for troubleshooting

- `[dirty]` in the docbook contents header means either unsynced TOC order
  or entries whose local mtime is newer than the remote timestamps.
- Amber source markers indicate filesystem-only data; enable sync and ingest.
- If ingest fails with `missing-heading`, check for orphan entries created
  before `ENTRY_ID` was disambiguated from `DOC_ID`.

