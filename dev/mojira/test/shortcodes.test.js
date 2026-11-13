'use strict';
// Shortcodes shared by Emacs (mojira-dispatch.el) and code-server Mojira
// (shortcodes.js), from the one table futon4/dev/mojira-codes.json.
const test = require('node:test');
const assert = require('node:assert');
const path = require('path');
const { execFileSync } = require('child_process');
const S = require('../shortcodes');

const table = S.readTable();
const repos = S.readRepos();
const codes = S.allCodes(table, repos);
const haveAllowlist = repos.length > 0;

test('fixed codes resolve to their Mojira page, or say they are Emacs-only', () => {
  assert.deepStrictEqual(S.resolve('p', table, repos), { page: 'pg' });
  assert.deepStrictEqual(S.resolve('i', table, repos), { page: 'gcc' });
  assert.deepStrictEqual(S.resolve('y', table, repos), { page: 'gparty' });
  assert.deepStrictEqual(S.resolve('?', table, repos), { help: true });
  assert.match(S.resolve('c l', table, repos).error, /claude REPLs are in Emacs/);
  assert.match(S.resolve('x 6', table, repos).error, /codex REPLs are in Emacs/);
  assert.match(S.resolve('qq', table, repos).error, /unknown shortcode/);
});

test('repo codes resolve to a directory; a kind narrows it', { skip: !haveAllowlist && 'no allowlist' }, () => {
  assert.strictEqual(S.resolve('4', table, repos).dir, '/home/joe/code/futon4');
  assert.strictEqual(S.resolve('4 m', table, repos).dir, '/home/joe/code/futon4/holes/missions');
  assert.match(S.resolve('4 q', table, repos).error, /unknown kind q/);
});

test('typing behaves like the Emacs prompt', { skip: !haveAllowlist && 'no allowlist' }, () => {
  assert.deepStrictEqual(S.step('p', codes).exit, true, 'a complete code with nothing longer acts at once');
  assert.strictEqual(S.step('c', codes).value, 'c ', 'a space is added when every continuation needs one');
  assert.strictEqual(S.step('1', codes).value, '1', '"1" stays: "1b" exists');
  assert.strictEqual(S.step('1bd', codes).value, '1b d', 'a key that only fits after a space gets one');
  assert.strictEqual(S.step('1bd', codes).exit, true);
  assert.strictEqual(S.step('c l', codes).exit, true);
  const r = S.step('6', codes);
  assert.strictEqual(r.exit, false);
  assert.ok(r.ret, '"6" alone is a code: RET acts');
  assert.ok(['d', 'm'].every((k) => r.next.includes(k)));
});

test('l is a modal: "last" in the context before it, typed without a space and acting at once', { skip: !haveAllowlist && 'no allowlist' }, () => {
  assert.deepStrictEqual(S.step('xl', codes), { value: 'x l', exit: true });
  assert.deepStrictEqual(S.step('cl', codes), { value: 'c l', exit: true });
  assert.deepStrictEqual(S.step('6l', codes), { value: '6 l', exit: true });
  assert.deepStrictEqual(S.step('l', codes), { value: 'l', exit: true });
  assert.strictEqual(S.step('6d', codes).exit, true, 'adding l leaves the instant kind codes instant');
  assert.deepStrictEqual(S.resolve('l', table, repos), { back: true });
  assert.deepStrictEqual(S.resolve('6 l', table, repos), { emacs: '6 l', label: 'the last file in futon6' });
});

test('Emacs and Mojira read the same codes from the one table', { skip: !haveAllowlist && 'no allowlist' }, () => {
  let out;
  try {
    out = execFileSync('emacs', ['-Q', '--batch', '-l', path.join(__dirname, '..', '..', 'mojira-dispatch.el'), '--eval',
      '(princ (mapconcat (function identity) (mojira--all-codes) "\\n"))'], { encoding: 'utf8', timeout: 30000 });
  } catch (e) { return; }   // no emacs here: nothing to compare
  const emacs = new Set(out.split('\n').filter(Boolean));
  // Agent codes depend on open buffers in Emacs, so compare everything else.
  const notAgent = (c) => !/^[cxzk] (l|[0-9]+)$/.test(c);
  assert.deepStrictEqual([...emacs].filter(notAgent).sort(), codes.filter(notAgent).sort());
});
