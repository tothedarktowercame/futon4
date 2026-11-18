// One operator turn, read from its record (the files the 象 stepper reads:
// <turnRecordsDir>/turn-XXXXXX.json and .analysis.json beside it).
// Joe 2026-11-18, on the herd's turn links, which only offered "Enter opens
// this turn in the 象 stepper in Emacs": "I think Mojira either already can
// display this info or should be able to". The page shows what was said and
// how 象 read it, the reply, what the turn addressed and left open, 馬's and
// 驴's patterns, and the turns before and after; Emacs stays one link away.
// Pure: (record, analysis) -> {title, html}.
'use strict';

const REPLY_CHARS = 600;

function hhmm(iso) { return String(iso || '').slice(11, 16); }
function day(iso) { return String(iso || '').slice(0, 10); }
function clip(s, n) { const t = String(s || ''); return t.length > n ? t.slice(0, n - 1) + '…' : t; }

module.exports = function turnView({ esc, link }) {
  function render(id, record, analysis) {
    if (!record) return { title: id, html: `<h1>${esc(id)}</h1><p class="err">No turn record ${esc(id)} in the turn records directory.</p>` };
    const r = record;
    const nav = [
      r.prev_operator_turn_id ? link('turn:' + r.prev_operator_turn_id, '← previous turn') : '<span class="dim">first turn</span>',
      r.next_operator_turn_id ? link('turn:' + r.next_operator_turn_id, 'next turn →') : '<span class="dim">latest turn</span>',
      r.session_id ? link('mamap:' + r.session_id, 'intent graph') : '',
      r.session_id ? link('strand:' + r.session_id, 'strand') : '',
      link('step:' + id, 'open in the 象 stepper (Emacs)'),
    ].filter(Boolean).join(' · ');

    // what was said, fragment by fragment as 象 read it
    const fragments = ((analysis && analysis.sentences) || []).flatMap((s) => s.fragments || []);
    const said = fragments.length
      ? `<ul>${fragments.map((f) => `<li><span class="gs-tag">${esc(f.intent || '?')}</span> ${esc(f.text)}${f.target ? ` <span class="dim">— about ${esc(f.target)}</span>` : ''}</li>`).join('')}</ul>`
      : `<p>${esc(r.source_text || r.original_text || '')}</p>${analysis ? '' : '<p class="dim">象 has not read this turn yet.</p>'}`;

    const paras = r.reply_paragraphs || [];
    const reply = paras.length
      ? `<ul>${paras.map((p) => `<li>${p.mark ? `<span class="gs-tag">${esc(p.mark)}</span> ` : ''}${esc(clip(p.text, REPLY_CHARS))}</li>`).join('')}</ul>`
      : (r.reply_text ? `<p>${esc(clip(r.reply_text, REPLY_CHARS * 2))}</p>` : '<p class="dim">no reply recorded</p>');

    const ports = r.ports || {};
    const closed = ports.closed_this_turn || [];
    const open = ports.still_open || [];
    const addressed = closed.length
      ? `<ul>${closed.map((c) => `<li><span class="gs-tag">${esc(c.kind)}</span> ${esc(clip(c.text, 200))}</li>`).join('')}</ul>`
      : '<p class="dim">nothing was closed in this turn</p>';
    const kinds = {};
    for (const o of open) kinds[o.kind] = (kinds[o.kind] || 0) + 1;
    const stillOpen = open.length
      ? `<p>${esc(open.length)} still open after it: ${Object.entries(kinds).map(([k, n]) => `${esc(k)} ${n}`).join(' · ')}</p>`
      : '<p class="dim">nothing left open</p>';

    const verdicts = ((r.pattern_verdicts || {}).patterns || []).filter((p) => p.status === 'occurrence' || p.status === 'partial');
    const ma = verdicts.length
      ? `<ul>${verdicts.map((p) => `<li>${link('pat:' + p.pattern, p.pattern)} <span class="dim">${esc(p.status)}</span></li>`).join('')}</ul>`
      : '<p class="dim">none</p>';
    const ranked = ((r.lu_ranked || {}).ranked || []).slice(0, 5);
    const lu = ranked.length
      ? `<ul>${ranked.map((p) => `<li>${link('pat:' + p.id, p.id)}</li>`).join('')}</ul>`
      : '<p class="dim">none</p>';

    const head = [r.agent_id, `${day(r.created_at)} ${hhmm(r.created_at)}`, r.mission_id].filter(Boolean).map(esc).join(' · ');
    return { title: 'turn ' + id.replace(/^turn-/, ''), html: `<div class="gcc">
<h1>Turn ${esc(id.replace(/^turn-/, ''))} <span class="dim">${head}</span></h1>
<p>${nav}</p>
<div class="gs-h">YOU SAID${analysis ? ' · as 象 read it' : ''}</div>${said}
<div class="gs-h">THE REPLY${r.agent_id ? ' · ' + esc(r.agent_id) : ''}</div>${reply}
<div class="gs-h">ADDRESSED IN THIS TURN</div>${addressed}${stillOpen}
<div class="gs-h">馬 PATTERNS IN THE TURN</div>${ma}
<div class="gs-h">驴 PROPOSED</div>${lu}
</div>` };
  }
  return { render };
};
