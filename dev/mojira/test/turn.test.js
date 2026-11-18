'use strict';
// Joe 2026-11-18: "I think Mojira either already can display this info or
// should be able to" (the herd's turn links only offered the Emacs stepper).
const test = require('node:test');
const assert = require('node:assert');
const T = require('../turn-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a class="ln" data-href="${esc(href)}">${esc(text)}</a>`;
const record = {
  agent_id: 'agent-invented', created_at: '2026-11-18T03:02:00Z', mission_id: 'M-invented', session_id: 'session-invented',
  prev_operator_turn_id: 'turn-prev01', next_operator_turn_id: null,
  source_text: 'cache what has not changed. which file holds it?',
  reply_paragraphs: [{ mark: '㊣', text: 'Agreed, ' + 'x'.repeat(700) }, { mark: '🈯', text: 'store.clj' }],
  ports: { closed_this_turn: [{ act: 'a1', kind: 'propose', text: 'cache what has not changed.' }],
    still_open: [{ act: 'a2', kind: 'clarify' }, { act: 'a3', kind: 'clarify' }, { act: 'a4', kind: 'propose' }] },
  pattern_verdicts: { patterns: [{ pattern: 'p/invented', status: 'occurrence' }, { pattern: 'p/void', status: 'void' }] },
  lu_ranked: { ranked: [{ id: 'p/proposed-one' }] },
};
const analysis = { sentences: [{ fragments: [
  { text: 'cache what has not changed.', intent: 'propose', target: 'slow reads' },
  { text: 'which file holds it?', intent: 'clarify' }] }] };
const text = (html) => html.replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ');

test('a turn page: what was said as 象 read it, the reply, what it addressed, patterns, neighbours', () => {
  const html = T({ esc, link }).render('turn-inv001', record, analysis).html;
  const t = text(html);
  assert.match(t, /Turn inv001 agent-invented · 2026-11-18 03:02 · M-invented/);
  assert.match(t, /propose cache what has not changed\. — about slow reads/);
  assert.match(t, /clarify which file holds it\?/);
  assert.match(t, /㊣ Agreed, x+…/, 'long reply paragraphs are cut, with an ellipsis');
  assert.match(t, /🈯 store\.clj/);
  assert.match(t, /ADDRESSED IN THIS TURN propose cache what has not changed\./);
  assert.match(t, /3 still open after it: clarify 2 · propose 1/);
  assert.match(html, /data-href="pat:p\/invented"/);
  assert.doesNotMatch(html, /p\/void/, 'a void 馬 verdict is not a pattern in the turn');
  assert.match(html, /data-href="pat:p\/proposed-one"/);
  assert.match(html, /data-href="turn:turn-prev01">← previous turn/);
  assert.match(t, /latest turn/);
  assert.match(html, /data-href="mamap:session-invented"/);
  assert.match(html, /data-href="step:turn-inv001">open in the 象 stepper \(Emacs\)/);
});

test('an unread turn shows its text and says 象 has not read it; a missing record says so', () => {
  const t = text(T({ esc, link }).render('turn-inv001', record, null).html);
  assert.match(t, /cache what has not changed\. which file holds it\?/);
  assert.match(t, /象 has not read this turn yet/);
  assert.match(text(T({ esc, link }).render('turn-gone01', null, null).html), /No turn record turn-gone01/);
});
