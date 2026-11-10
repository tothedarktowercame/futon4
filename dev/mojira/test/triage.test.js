// node --test dev/mojira/test/   (synthetic fixture with the shape of
// `bb daily-loop brief --json`; futon7's scan data stays in futon7)
'use strict';
const { test } = require('node:test');
const assert = require('node:assert/strict');
const t = require('../triage');
const brief = require('./brief-fixture.json');

test('rows carry summary, p-worth, criteria and the latest ruling', () => {
  const rows = t.rowsFromBrief(brief);
  assert.deepEqual(rows.map((r) => r.id), ['example/alpha', 'example/beta', 'example/gamma']);
  assert.equal(rows[0].pWorth, 0.4);
  assert.equal(rows[0].criteria.length, 2);
  assert.equal(rows[0].verdict, null);
  assert.equal(rows[1].verdict, 'not-now');          // later line wins over "invalid"
  assert.equal(rows[1].note, 'changed my mind');
  assert.equal(t.unruledCount(brief), 2);
});

test('verdict names accept the keyword form', () => {
  assert.equal(t.verdictName(':worth'), 'worth');
  assert.equal(t.verdictName('worth'), 'worth');
});

test('verdict commands pass a keyword, the item or "all", and an optional note', () => {
  assert.deepEqual(t.verdictArgs('2026-01-01-main', 'example/alpha', 'worth'),
    ['daily-loop', 'verdict', '2026-01-01-main', 'example/alpha', ':worth']);
  assert.deepEqual(t.verdictArgs('2026-01-01-main', 'all', 'not-now', '  later  '),
    ['daily-loop', 'verdict', '2026-01-01-main', 'all', ':not-now', 'later']);
  assert.deepEqual(t.verdictArgs('b', 'i', 'invalid', '   '), ['daily-loop', 'verdict', 'b', 'i', ':invalid']);
  assert.throws(() => t.verdictArgs('b', 'i', 'maybe'), /verdict must be one of/);
  assert.throws(() => t.verdictArgs('', 'i', 'worth'), /required/);
});

test('brief commands', () => {
  assert.deepEqual(t.briefArgs(), ['daily-loop', 'brief', '--json']);
  assert.deepEqual(t.briefArgs('x'), ['daily-loop', 'brief', 'x', '--json']);
});

test('CLI refusals are reported from the ERROR line', () => {
  assert.equal(t.cliError('ERROR: :unknown-item item nope in b\n', '', 1), 'ERROR: :unknown-item item nope in b');
  assert.equal(t.cliError('verdict worth recorded for 1 item(s) of b\n', '', 0), null);
  assert.equal(t.cliError('', 'Exception in thread "main"\n  at x', 1), 'Exception in thread "main"');
});
