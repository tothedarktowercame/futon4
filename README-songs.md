# Songs and Choruses in XTDB

This note explains the storage model used by the Songs and Chorus demo in
Arxana. The short version is:

- we did use XTDB for storage;
- we did not need new `futon1a` endpoints or schema code;
- we reused the existing entity and hyperedge API surface already exposed to
  `futon4`.

## Why `futon1a` Did Not Change

The new browser work in `futon4` sits on top of storage primitives that already
existed:

- entity upsert via `arxana-store-ensure-entity`
- entity fetch via `arxana-store-fetch-entity`
- latest-entities query via `arxana-store-fetch-entities-latest`
- hyperedge create via `arxana-store-create-hyperedge`
- hyperedge fetch/query via `arxana-store-fetch-hyperedge` and
  `arxana-store-fetch-hyperedges`

See:

- [dev/arxana-store.el](/home/joe/code/futon4/dev/arxana-store.el#L748)
- [dev/arxana-store.el](/home/joe/code/futon4/dev/arxana-store.el#L782)
- [dev/arxana-store.el](/home/joe/code/futon4/dev/arxana-store.el#L792)
- [dev/arxana-store.el](/home/joe/code/futon4/dev/arxana-store.el#L809)
- [dev/arxana-store.el](/home/joe/code/futon4/dev/arxana-store.el#L854)

That is why the demo required only `futon4` changes. The data is persisted in
XTDB, but it fits inside the server-side shapes that were already supported.

## Entity Types Used

The Songs browser currently works with these XTDB entity types:

- `arxana/media-lyrics`
  Used for existing chapbook lyric entities already present in XTDB.
- `arxana/song`
  Used for imported suite songs.
- `arxana/song-lyrics`
  Also accepted by the Songs catalog, although the current suite importer writes
  `arxana/song`.
- `arxana/chorus`
  Used for Arxana-native chorus compositions.

See the Songs catalog configuration in
[dev/arxana-browser-songs.el](/home/joe/code/futon4/dev/arxana-browser-songs.el#L40)
and the Chorus demo import in
[dev/arxana-browser-chorus.el](/home/joe/code/futon4/dev/arxana-browser-chorus.el#L817).

## Entity Shape

The entity shape is the standard Futon entity shape already used elsewhere:

```clojure
{:id "arxana/song/nightmarish-suite/the-sea/abi"
 :name "Abi"
 :type "arxana/song"
 :source "Blue carnations, in the soil\nKiss me in the moonlight\n..."
 :props {...}}
```

The important fields for this demo are:

- `:id`
  Stable entity id.
- `:name`
  Display title.
- `:type`
  XTDB entity type used by browser catalogs.
- `:source`
  Plain text body shown in the left-hand reading buffer.
- `:props`
  Optional metadata used by higher-level UI logic.

The Chorus demo uses exactly the same entity mechanism:

```clojure
{:id "arxana/chorus/demo/abi"
 :name "Abi Chorus (demo)"
 :type "arxana/chorus"
 :source "So go to the orange of purple...\n..."
 :props {...}}
```

## Song Storage

Suite songs are stored as ordinary entities whose `:source` is the song text.
The current importer reads `nightmarish-suite.tex` and writes one XTDB entity per
song. No special song-only backend table or endpoint was added.

The Songs browser simply queries latest entities by type and opens their
`:source` in a synchronized reading view.

## Annotation Storage

Annotations are stored as ordinary Futon hyperedges. No new annotation-specific
server storage path was required.

The `futon4` helper now posts rich hyperedges in this shape:

```clojure
{:id "hx:demo:abi-blue"
 :type "annotation"
 :hx/type "annotation/supports"
 :hx/endpoints [{:role "annotated"
                 :entity-id "arxana/song/nightmarish-suite/the-sea/abi"
                 :passage "lines 1-4: Blue carnations, in the soil"}
                {:role "source"
                 :entity-id "doc:no-longer-alone"
                 :passage "So go to the orange of purple or the blue of green, the blue of red"}]
 :props {:note "Blue is treated as somewhere one can travel inside."}
 :hx/content ...
 :hx/labels ...
 :hx/confidence ...}
```

In practice, the demo mainly depends on:

- `:hx/type`
- `:hx/endpoints`
- `:props`

See the payload builder in
[dev/arxana-store.el](/home/joe/code/futon4/dev/arxana-store.el#L748).

### Endpoint Roles

The Songs and Chorus code assumes the following endpoint roles:

- `annotated`
  The song passage being contextualized.
- `source`
  The chapbook passage providing the warrant or allusive source.

For `annotation/open-question`, only the annotated side is required for the
current UI.

### Hyperedge Types Used

- `annotation/supports`
  Positive warrant or gloss linking song text to chapbook material.
- `annotation/open-question`
  Negative-space prompt marking material not yet developed in the chapbook.

These are conventions in the data, not new server-enforced types.

## Chorus Storage

Choruses are also plain entities. The current demo Chorus stores a unified text
body in `:source` and a few metadata keys in `:props`:

```clojure
{:id "arxana/chorus/demo/abi"
 :type "arxana/chorus"
 :source "So go to the orange of purple...\n..."
 :props {'chorus/source-song "arxana/song/nightmarish-suite/the-sea/abi"
         'chorus/source-songs ["arxana/song/nightmarish-suite/the-sea/abi"]
         'chorus/annotation-ids ["hx:demo:abi-blue" ...]
         'chorus/questions [...]}}
```

See:

- [dev/arxana-browser-chorus.el](/home/joe/code/futon4/dev/arxana-browser-chorus.el#L817)

These `chorus/*` keys are interpreted in `futon4`, not validated in `futon1a`.

### What `chorus/*` Means Right Now

- `chorus/source-song`
  Primary song associated with the Chorus.
- `chorus/source-songs`
  Allowlist of songs whose annotations may be used to contextualize this Chorus.
- `chorus/annotation-ids`
  Specific support hyperedges the Chorus should pull from.
- `chorus/questions`
  Carried along for future authoring use; not yet a major part of the Chorus UI.

The current Chorus notes pane resolves each listed hyperedge id, checks whether
the hyperedge's `annotated` endpoint belongs to an allowed song, and if so
retrieves the corresponding song passage text.

## Passage Addressing

In this demo, passage addressing is still string-based. Examples:

- `line 7: Heavy, the sleep of butterflies`
- `lines 1-4: Blue carnations, in the soil`
- `So go to the orange of purple or the blue of green, the blue of red`

This is enough for the demo UI, but it is not yet a first-class anchor model.

Current behavior:

- Songs use the passage strings directly to highlight text and label note blocks.
- Chorus retrieval parses line-range strings like `lines 1-4: ...` to recover the
  corresponding lines from the song entity text.

This is one of the main places where we are still relying on client-side logic.

## What Was Reused vs. What Is New

Reused:

- XTDB entity storage
- XTDB hyperedge storage
- existing Futon API endpoints
- existing chapbook lyric entities
- existing Arxana store request helpers

New in `futon4`:

- Songs browser UI
- Chorus browser UI
- suite-song importer
- demo annotation importer
- Chorus note retrieval that resolves song-side annotation context
- client conventions for `annotation/supports`, `annotation/open-question`, and
  `chorus/*` props

## What Is Still Client-Side Convention

These parts are currently conventions understood by `futon4`, not server-backed
schema guarantees:

- `arxana/song` and `arxana/chorus` as application-level content types
- `annotation/supports` and `annotation/open-question` as semantic hyperedge
  types
- endpoint roles `annotated` and `source`
- string passage syntax such as `line 7: ...` and `lines 1-4: ...`
- `chorus/source-song`, `chorus/source-songs`, `chorus/annotation-ids`

## Likely `futon1a` Work Later

If this stops being a demo and becomes a first-class subsystem, the most likely
server-side additions would be:

- validation for song, chorus, and annotation shapes
- indexed queries such as “all annotations for entity X”
- first-class passage anchors instead of string passages
- fuzzy anchor resolution for lightly revised song or chapbook text
- explicit backlink/read-model endpoints for Choruses and Songs

For now, though, the important point is that the current work is not “fake local
state”. It is real XTDB-backed storage, just using existing generic entity and
hyperedge capabilities rather than requiring new `futon1a` code.
