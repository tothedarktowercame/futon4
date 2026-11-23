# PaperPrinter glossary ink

`glossary.py` joins the formal targets retained in rendered `\leanref` links
to curated citation readings and explicit reading claims.  It preserves every
observed reader-visible form, but does not call multiple forms a conflict:
`Norm.norm` applied to `x` and to `z` is ordinary contextual variation.

Claims in `glossary-claims.tsv` have status `accepted`, `contested`, or
`rejected`.  A formal target is contested when it has an explicit contested
claim or more than one distinct live (`accepted` or `contested`) reading.
Rejected readings remain in the export as historical evidence but do not ink
the current render.

Contested ink is the number of reader-visible characters inside links to
contested targets divided by all reader-visible proof-source characters.  It
is a vocabulary audit surface, not a correctness percentage.  This makes a
disagreement countable without erasing it or implying that every notational
variant is defective.

Example:

```sh
python3 glossary.py '/path/to/head/*.jsonl' \
  --readings /path/to/citation-readings.json \
  --tsv /tmp/glossary.tsv --summary /tmp/glossary-summary.json \
  --html /tmp/glossary.html
```
