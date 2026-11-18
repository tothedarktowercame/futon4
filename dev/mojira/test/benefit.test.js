'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const http = require('http');
const { render, reading: renderReading, clusters } = require('../benefit-view');
const { check, readings } = require('../benefit-check');
const data = require('../benefit-data');
const reading = id => ({ record: { id, 'asserted-content': { 'item/id': id } }, evidence: [], 'evidence-completeness': ':complete' });
const fixture = () => ({ status: ':ok', mode: ':detail', benefit: reading('H-test'),
  items: [{ item: reading('M-named'), serves: [reading('serves-a')], names: [{ link: reading('names-a'), entity: reading('entity-a') }] },
    { item: reading('M-unnamed'), serves: [reading('serves-b')], names: [] }],
  substrate: [{ 'entity/id': 'substrate-only', serves: reading('serves-substrate') }] });
test('list includes separate counts and on-demand detail links', () => {
  const html = render({ status: ':ok', mode: ':list', benefits: [{ benefit: { id: 'benefit-id', 'asserted-content': { 'benefit/line': 'H-test' } }, 'serves-v1': 2, 'serves-v2': 3 }] }).html;
  assert.match(html, /data-href="bf:benefit-id"/);
  assert.match(html, /item filings 2 · substrate identities 3/);
});
test('detail retains unnamed parent, separates substrate lane, escapes data, cycles raw', () => {
  const v = fixture(); v.items[1].item.record['asserted-content']['item/id'] = 'M-unnamed<script>';
  const html = render(v, { id: 'H', check: check(v) }).html;
  assert.match(html, /M-unnamed&lt;script&gt;/); assert.match(html, /No names recorded/);
  assert.ok(!html.split('<h2>Substrate identities')[0].includes('substrate-only'));
  assert.match(html, /check \(ramo\): <strong>pass/); assert.match(html, /data-cycle="bf-raw:H"/);
  assert.match(render(v, { raw: true, id: 'H' }).html, /data-cycle="bf:H"/);
});
test('inconclusive and loading cannot look like an empty success', () => {
  assert.match(render({ status: ':inconclusive', reason: ':truncated' }).html, /inconclusive/);
  assert.doesNotMatch(render({ status: ':inconclusive' }).html, /0 benefit lines/);
  assert.match(render({ status: 'reading' }).html, /Reading…/);
  assert.equal(check({ status: ':inconclusive' }).result, 'not-run');
});
test('ramo mirrors each fibre position; planted missing id and incomplete evidence violate', () => {
  assert.equal(check(fixture()).result, 'pass');
  const count = readings(fixture()).length;
  for (let n = 0; n < count; n++) {
    const v = fixture(); const [path, r] = readings(v)[n]; delete r.record.id;
    assert.deepEqual(check(v).violations.map(x => x.path), [path]);
  }
  const v = fixture(); v.items[0].names[0].entity['evidence-completeness'] = ':partial';
  assert.equal(check(v).result, 'violation');
  v.items[0].names[0].entity['evidence-completeness'] = ':complete';
  v.items[0].names[0].entity.evidence = null;
  assert.equal(check(v).result, 'violation');
});
test('ready file default and configuration stay in host and validate loopback', () => {
  assert.equal(data.endpoint('', (p) => { assert.equal(p, '/run/user/99/futon4a-catalogue-99/ready.edn'); return '{:port 7209 :read-only true}'; }, 99).href, 'http://127.0.0.1:7209/arxana/v1');
  assert.throws(() => data.endpoint('https://example.com'));
  assert.throws(() => data.endpoint('', () => '{}', 99));
});
test('host sends only the read operation and preserves refusal / tagged text', async () => {
  let received; let reply = { ok: { status: ':ok', mode: ':list', benefits: [], text: { $string: ':literal' } } };
  const server = http.createServer((req, res) => { let body = ''; req.on('data', c => { body += c; }); req.on('end', () => { received = JSON.parse(body); res.end(JSON.stringify(reply)); }); });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  try {
    const url = data.endpoint(`http://127.0.0.1:${server.address().port}`);
    assert.equal((await data.read(url, 'benefit-id', '2026-01-01T00:00:00.000Z')).text, ':literal');
    assert.equal(received.operation, ':cascade/benefit-filings');
    assert.deepEqual(received.body, { 'benefit-id': 'benefit-id', 'valid-time': '2026-01-01T00:00:00.000Z' });
    reply = { error: { 'error/code': ':denied' } };
    assert.equal((await data.read(url)).status, ':refused');
  } finally { await new Promise(r => server.close(r)); }
});
test('each reading has a keyboard evidence link and the evidence page returns that reading', () => {
  const v = fixture();
  assert.match(render(v, { id: 'H', check: check(v) }).html, /data-href="bf-evidence:H:1"/);
  const evidence = render(v, { id: 'H', evidence: 1 }).html;
  assert.match(evidence, /Reading evidence/);
  assert.match(evidence, /M-named/);
  assert.doesNotMatch(evidence, /M-unnamed/);
});
test('opening either column reads immediately, shows progress and reuses the page', async () => {
  const fs = require('fs'), os = require('os'), path = require('path'), Module = require('module');
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'mojira-benefit-'));
  fs.writeFileSync(path.join(dir, 'lex.json'), '[]'); fs.writeFileSync(path.join(dir, 'pending.tsv'), 'id\n');
  let requests = 0;
  const server = http.createServer((_req, res) => { requests++; res.end(JSON.stringify({ ok: fixture() })); });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  const config = { lexiconPath: path.join(dir, 'lex.json'), pendingPath: path.join(dir, 'pending.tsv'), catalogueUrl: `http://127.0.0.1:${server.address().port}` };
  const original = Module._load;
  Module._load = function(req, ...rest) { return req === 'vscode' ? { workspace: { getConfiguration: () => ({ get: k => config[k] }) } } : original.call(this, req, ...rest); };
  let listener; const messages = [];
  const panel = { webview: { postMessage: m => messages.push(m), onDidReceiveMessage: f => { listener = f; return { dispose() {} }; } } };
  try {
    delete require.cache[require.resolve('../core')];
    const core = require('../core'); core.attach(panel, { history: [] });
    listener({ type: 'peek', href: 'bf:benefit-one' });
    assert.match(messages.filter(m => m.type === 'peek').at(-1).html, /Reading…/);
    listener({ type: 'go', href: 'bf:benefit-one' });
    assert.match(messages.filter(m => m.type === 'page').at(-1).html, /Reading…/);
    for (let i = 0; i < 50 && !messages.some(m => m.html?.includes('M-unnamed')); i++) await new Promise(r => setTimeout(r, 10));
    assert.equal(requests, 1);
    assert.ok(messages.some(m => m.type === 'page' && m.html.includes('M-unnamed')));
    listener({ type: 'go', href: 'bf-evidence:benefit-one:1' });
    assert.equal(messages.at(-1).type, 'peek'); assert.match(messages.at(-1).html, /M-named/);
    assert.equal(requests, 1);
  } finally { Module._load = original; await new Promise(r => server.close(r)); fs.rmSync(dir, { recursive: true }); }
});
test('overview groups named trunks by filings and keeps unknown codes explicit', () => {
  const row = (code, trunk, count) => ({ benefit: { id: code, 'asserted-content': { 'benefit/line': code } },
    label: trunk ? { trunk, 'trunk-name': trunk + ' group', name: 'A named line', gloss: 'A useful gloss' } : null,
    'serves-v1': count, 'serves-v2': 0 });
  const benefits = [row('L2', 'L', 1), row('G1', 'G', 5), row('L1', 'L', 1), row('Z9', null, 100)];
  assert.deepEqual(clusters(benefits).map(g => g.key), ['G', 'L', 'unnamed']);
  assert.deepEqual(clusters(benefits)[1].rows.map(b => b.benefit.id), ['L1', 'L2']);
  const html = render({ status: 'ok', mode: 'list', benefits }).html;
  assert.match(html, /G group/); assert.match(html, /Unnamed codes/);
  assert.match(html, /A useful gloss/); assert.match(html, /not yet records/);
  assert.match(html, /data-href="bf:Z9"/);
});
test('reading has readable names and one provenance line; raw remains behind c', () => {
  const r = reading('M-invented');
  r.evidence = [{ basis: ':parser', standing: ':unverified', coverage: ':complete',
    'event-time': '2026-11-01T00:00:00.000Z', 'source-refs': [{ path: 'invented.md', line: 7 }],
    content: { 'sheet/one-line': 'An invented item summary', secretShape: { arbitrary: 1 } } }];
  const html = renderReading(r);
  assert.match(html, /M-invented/); assert.match(html, /An invented item summary/);
  assert.match(html, /invented.md:7 · parser · 2026-11-01/);
  assert.equal((html.match(/class="dim provenance"/g) || []).length, 1);
  assert.doesNotMatch(html, /<pre>|secretShape|arbitrary/);
  const f = fixture(); f.items[0].item = r;
  assert.doesNotMatch(render(f, { evidence: 1 }).html, /<pre>/);
  assert.match(render(f, { raw: true }).html, /secretShape/);
});
