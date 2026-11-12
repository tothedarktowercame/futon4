// A letter of intent as a working draft in Mojira (Joe, 2026-11-12: "hold a
// 'working draft' of that, so that I could quickly notate potential benefits
// and beneficiaries"). Pure: parses the draft's fields and Joe's notes file,
// and formats one note line. core.js does the reading and appending.
//
// Draft fields are marked as the Mozilla draft marks them:
//   <!-- field: impact limit=150 -->
//   ...body...
//   <!-- end -->
// with the limit in words (as the draft's check_limits.py counts them).
// Notes are a markdown list, one line each, appended and never rewritten:
//   - 2026-11-12 · benefit · impact · the text Joe typed
'use strict';

const FIELD = /<!-- field: (\w+) limit=(\d+) -->\n([\s\S]*?)\n<!-- end -->/g;
const KINDS = { benefit: 'benefits', beneficiary: 'beneficiaries' };

function words(s) { return String(s).split(/\s+/).filter(Boolean).length; }

function parseDraft(text) {
  const t = String(text);
  const title = (t.match(/^# (.*)$/m) || [, ''])[1];
  const fields = [];
  for (const m of t.matchAll(FIELD)) {
    const before = t.slice(0, m.index);
    const sec = [...before.matchAll(/^## (.*)$/gm)].pop();
    fields.push({ name: m[1], limit: Number(m[2]), body: m[3].trim(), words: words(m[3]), section: sec ? sec[1] : '' });
  }
  return { title, fields };
}

const NOTE = /^- (\d{4}-\d{2}-\d{2}) · (benefit|beneficiary) · ([\w-]+) · (.*)$/;
function parseNotes(text) {
  return String(text || '').split('\n').map((l) => l.match(NOTE)).filter(Boolean)
    .map(([, date, kind, field, body]) => ({ date, kind, field, text: body }));
}

function formatNote({ date, kind, field, text }) {
  if (!KINDS[kind]) throw new Error('not a note kind: ' + kind);
  if (!/^[\w-]+$/.test(field)) throw new Error('not a field name: ' + field);
  const body = String(text || '').replace(/\s+/g, ' ').trim();
  if (!body) throw new Error('empty note');
  return `- ${date} · ${kind} · ${field} · ${body}\n`;
}

const NOTES_HEAD = '# Working notes on the letter of intent: benefits and beneficiaries\n\n'
  + 'Written from Mojira (b benefit, p beneficiary), one line each, newest last.\n'
  + 'A note on the whole letter has the field "letter".\n\n';

module.exports = { KINDS, words, parseDraft, parseNotes, formatNote, NOTES_HEAD };
