'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const V = require('../variants');
const fixture = {draft: {file:'toy', note:'Invented <note>'}, paragraphs: [
  {n:1, title:'Toy one', linked:1, segments:[{},{}], sources:[
    {id:'turn-a', at:'2026-01-01T12:00:00Z', text:'🈸 猴 alpha beta gamma', used_start:4, used_end:20},
    {id:'turn-b', at:'2026-01-02T12:00:00Z', text:'delta', used_start:0, used_end:5}],
   ops:[{op:'kept',text:'alpha',source:'turn-a',source_start:4,source_end:9},
        {op:'changed',old:'beta',new:'better',source:'turn-a',source_start:10,source_end:14},
        {op:'dropped',old:'gamma',source:'turn-a',source_start:15,source_end:20},
        {op:'added',new:'addition',source:'turn-a'}, {op:'new',text:'fresh'},
        {op:'kept',text:'delta',source:'turn-b',source_start:0,source_end:5}]},
  {n:2,title:'Toy two',linked:0,segments:[{}],sources:[],ops:[{op:'new',text:'<script>never</script>'}]}]};
const doc = () => V.parse(JSON.stringify(fixture));
test('list and all five operations in full/additions/clean', () => {
  assert.match(V.render(doc()).html, /vd:2/);
  assert.match(V.render(doc()).html, /1 of 2 linked/);
  for (const mode of ['full','additions','clean']) {
    const html = V.render(doc(), '1:' + mode).html;
    for (const word of ['alpha','better','addition','fresh','delta']) assert.match(html, new RegExp('>' + word + '<'));
    assert.equal(html.includes('>beta<'), mode === 'full');
    assert.equal(html.includes('>gamma<'), mode === 'full');
    assert.equal(html.includes('vd-added'), mode !== 'clean');
    assert.equal(html.includes('vd-source '), mode === 'full');
    assert.equal(html.includes('vd-dropped'), mode === 'full');
    assert.match(html, /vds:1:1:new:0/); // replacement retains its source in clean too
    assert.match(html, /data-vd-next="2"/);
  }
  assert.match(V.render(doc(), '2').html, /&lt;script&gt;/);
});
test('codepoint anchors after astral and CJK; used span and exact word', () => {
  const html = V.renderSource(doc(), '1:0:text:0').html;
  assert.match(html, /🈸 猴 <mark data-vd-here>alpha<\/mark>/);
  assert.match(html, /class="vd-used"> beta gamma/);
  assert.match(html, /\[1\] spoken 2026-01-01 12:00:00 UTC/);
  assert.match(V.renderSource(doc(), '1:5:text:0').html, /\[2\] spoken/);
  assert.match(V.renderSource(doc(), '1:1:new:0').html, /<mark data-vd-here>beta<\/mark>/);
  assert.match(V.renderSource(doc(), '1:2:old:0').html, /<mark data-vd-here>gamma<\/mark>/);
  for (const i of [3,4]) assert.match(V.renderSource(doc(), `1:${i}:new:0`).html, /Written in the draft; no spoken source/);
});
test('shifted anchor visibly refuses rather than silently highlighting', () => {
  const d = doc(); d.paragraphs[0].ops[0].source_start++; d.paragraphs[0].ops[0].source_end++;
  for (const mode of ['full','additions','clean']) assert.match(V.render(d, '1:' + mode).html, /anchor does not match source/);
  const html = V.renderSource(d, '1:0:text:0').html;
  assert.match(html, /anchor does not match source/);
  assert.doesNotMatch(html, /data-vd-here/);
});
test('source text and identifiers escaped; no source markup executes', () => {
  const d = doc(); d.paragraphs[0].sources[0].text += '<img>'; d.paragraphs[0].sources[0].id = '<turn>';
  d.paragraphs[0].ops[0].source = '<turn>';
  const html = V.renderSource(d, '1:0:text:0').html;
  assert.match(html, /&lt;turn&gt;/); assert.match(html, /&lt;img&gt;/);
});
test('keyboard word links identify each word inside a multiword kept operation', () => {
  const d = doc();
  d.paragraphs[0].ops[0] = {op:'kept',text:'alpha beta',source:'turn-a',source_start:4,source_end:14};
  assert.match(V.render(d, '1:clean').html, /vds:1:0:text:1/);
  const html = V.renderSource(d, '1:0:text:1').html;
  assert.match(html, /<mark data-vd-here>beta<\/mark>/);
  assert.doesNotMatch(html, /<mark data-vd-here>alpha/);
});

test('a paragraph opens with all its quoted sources; a written word keeps them visible', () => {
  const v = require('../variants');
  const d = v.parse(require('fs').readFileSync('/home/joe/storage/arxana-variants/draft2-variants.json', 'utf8'));
  const p15 = d.paragraphs.find((p) => p.n === 15);
  const all = v.renderSource(d, '15').html;
  assert.equal((all.match(/spoken /g) || []).length, p15.sources.length);
  assert.ok(p15.sources.length >= 1 && all.includes('class="vd-used"'));
  // The nipped words themselves are marked, and are exactly the kept text.
  const kept = p15.ops.filter((o) => o.op === 'kept').map((o) => o.text).join(' ');
  const marked = [...all.matchAll(/<span class="vd-nipped">([^<]*)<\/span>/g)].map((m) => m[1]).join(' ');
  assert.ok(marked.length > 0);
  assert.equal(marked.replace(/\s+/g, ' ').trim().toLowerCase().replace(/[^a-z ]/g, ''),
               kept.replace(/\s+/g, ' ').trim().toLowerCase().replace(/[^a-z ]/g, ''));
  const written = p15.ops.findIndex((o) => o.op === 'new' || o.op === 'added');
  if (written >= 0) assert.ok(v.renderSource(d, `15:${written}:text:0`).html.includes('vd-used'));
  const none = d.paragraphs.find((p) => !p.sources.length);
  assert.match(v.renderSource(d, String(none.n)).html, /No sentence of this paragraph is linked/);
});
