'use strict';
// The letter-of-intent working draft (loi.js) on an invented draft: the real
// one is in futon7, which is private, so none of its text is copied here.
const test = require('node:test');
const assert = require('node:assert');
const L = require('../loi');

const DRAFT = `# FIXTURE letter, working draft

## Section 3: Project

<!-- field: title limit=3 -->
An invented title here
<!-- end -->

<!-- field: impact limit=150 -->
First paragraph.

Second paragraph.
<!-- end -->
`;

test('fields are parsed with their section, limit and word count', () => {
  const d = L.parseDraft(DRAFT);
  assert.strictEqual(d.title, 'FIXTURE letter, working draft');
  assert.deepStrictEqual(d.fields.map((f) => [f.name, f.limit, f.words, f.section]),
    [['title', 3, 4, 'Section 3: Project'], ['impact', 150, 4, 'Section 3: Project']]);
  assert.ok(d.fields[0].words > d.fields[0].limit, 'an over-limit field is visible as such');
});

test('a note round-trips through its line; bad kinds, fields and empty notes are refused', () => {
  const line = L.formatNote({ date: '2026-11-12', kind: 'benefit', field: 'impact', text: '  sunk cost,\n or innovator\'s dilemma? ' });
  assert.strictEqual(line, "- 2026-11-12 · benefit · impact · sunk cost, or innovator's dilemma?\n");
  const ns = L.parseNotes(L.NOTES_HEAD + line + L.formatNote({ date: '2026-11-12', kind: 'beneficiary', field: 'letter', text: 'small co-ops' }));
  assert.deepStrictEqual(ns, [
    { date: '2026-11-12', kind: 'benefit', field: 'impact', text: "sunk cost, or innovator's dilemma?" },
    { date: '2026-11-12', kind: 'beneficiary', field: 'letter', text: 'small co-ops' }]);
  assert.throws(() => L.formatNote({ date: '2026-11-12', kind: 'verdict', field: 'impact', text: 'x' }), /not a note kind/);
  assert.throws(() => L.formatNote({ date: '2026-11-12', kind: 'benefit', field: '../x', text: 'x' }), /not a field name/);
  assert.throws(() => L.formatNote({ date: '2026-11-12', kind: 'benefit', field: 'impact', text: '   ' }), /empty note/);
});
