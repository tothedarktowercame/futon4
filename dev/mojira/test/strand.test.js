'use strict';
const test = require('node:test');
const assert = require('node:assert');
const S = require('../strand-view');

const F = (f) => require('./strand-fixture/' + f);
const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a data-href="${esc(href)}">${esc(text)}</a>`;

test('renders every 馬 connection status and reuses the supplied count formatter', () => {
  let calls = 0;
  const counts = (c) => { calls++; return `COUNTS:${c['驴'] || 0}/${c['馬'] || 0}`; };
  const V = S({ esc, link, counts, now: () => Date.parse('2026-11-18T03:02:00Z') });
  const h = V.render(F('initial.json')).html;
  assert.match(h, /馬 previous pending · 2m old/);
  assert.match(h, /馬 previous absent: no-reply/);
  assert.match(h, /馬 session start/);
  assert.match(h, /COUNTS:2\/0/);
  assert.strictEqual(calls, 3, 'the shared formatter is called once per turn');
});

test('linked connection is a short predecessor link and items use gi links', () => {
  const V = S({ esc, link, counts: () => '' });
  const h = V.render(F('delta.json')).html;
  assert.match(h, /馬 ← <a data-href="pt:turn-003">turn-003<\/a>/);
  assert.match(h, /data-href="gi:M-four"/);
  assert.match(h, /data-href="pt:turn-004"/);
  assert.match(h, /data-href="step:turn-004"/);
});

test('since merge prepends new turns and replaces only refreshed turns', () => {
  const initial = F('initial.json');
  const oldOne = initial.turns[2];
  const merged = S.merge(initial, F('delta.json'), F('refresh.json'));
  assert.deepStrictEqual(merged.turns.map((t) => t.id), ['turn-004', 'turn-003', 'turn-002', 'turn-001']);
  assert.strictEqual(merged.turns[3], oldOne, 'an old turn is retained, not rebuilt');
  assert.strictEqual(merged.turns[1].ma_connection.status, 'linked', 'newest prior turn is refreshed in place');
  assert.strictEqual(merged.turns[0].gcc['馬'].count, 3);
});

test('tally reports the accumulating connected component', () => {
  const merged = S.merge(F('initial.json'), F('delta.json'), F('refresh.json'));
  assert.deepStrictEqual(S.tally(merged), {
    turns: 4,
    edges: { strand: 3, mention: 2, focus: 1, proposed: 1, occurs: 1,
      partial: 0, occurrence: 0, quotes: 0 },
    items: 3,
    sessionLinks: 3,
  });
});

test('merge preserves distinct 馬 quote parts between the same turns', () => {
  const initial = F('initial.json');
  const delta = { turns: [], edges: [
    { from: 'turn-003', to: 'turn-001', kind: 'quotes', pattern: 'p-one', part: 'I1' },
    { from: 'turn-003', to: 'turn-001', kind: 'quotes', pattern: 'p-one', part: 'G' },
  ] };
  const merged = S.merge(initial, delta, null);
  assert.strictEqual(merged.edges.filter((e) => e.kind === 'quotes').length, 2);
});

// Joe 2026-11-18: the monitor's old message named a setting key and borrowed
// a session from top items; "I don't see why we should even have 11s
// lookups". With no session set, it follows Joe's most recent conversation
// from 象's turn record and lists the others to switch to.
test('monitor with no session set follows the newest conversation in 象\'s turn record', async () => {
  const fs = require('fs'), os = require('os'), path = require('path'), Module = require('module'), http = require('http');
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'mojira-recent-'));
  fs.writeFileSync(path.join(dir, 'lex.json'), '[]'); fs.writeFileSync(path.join(dir, 'pending.tsv'), 'id\n');
  const initial = fs.readFileSync(path.join(__dirname, 'strand-fixture', 'initial.json'), 'utf8');
  const turns = [
    { 'turn-id': 'agent-a-session-end-20261118T050000Z', 'session-id': 'session-ended', 'agent-id': 'agent-a', 'created-at': '2026-11-18T05:00:00Z' },
    { 'turn-id': 'agent-b-turn-9', 'session-id': 'session-invented', 'agent-id': 'agent-b', 'created-at': '2026-11-18T04:30:00Z' },
    { 'turn-id': 'agent-c-turn-1', 'session-id': 'pending', 'agent-id': 'agent-c', 'created-at': '2026-11-18T04:20:00Z' },
    { 'turn-id': 'agent-b-turn-8', 'session-id': 'session-invented', 'agent-id': 'agent-b', 'created-at': '2026-11-18T04:10:00Z' },
    { 'turn-id': 'agent-d-turn-3', 'session-id': 'session-other', 'agent-id': 'agent-d', 'created-at': '2026-11-18T03:50:00Z' }];
  const asked = [];
  let turnsUp = true;
  const server = http.createServer((req, res) => {
    asked.push(req.url);
    if (req.url.startsWith('/api/alpha/xiang/turns')) {
      if (!turnsUp) { res.statusCode = 503; res.end(JSON.stringify({ reason: 'down' })); return; }
      res.end(JSON.stringify({ ok: true, turns }));
    } else if (req.url.startsWith('/api/alpha/circus/strand')) res.end(initial);
    else { res.statusCode = 404; res.end('{}'); }
  });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  const config = { lexiconPath: path.join(dir, 'lex.json'), pendingPath: path.join(dir, 'pending.tsv'), agencyUrl: `http://127.0.0.1:${server.address().port}` };
  const original = Module._load;
  Module._load = function(req, ...rest) { return req === 'vscode' ? { workspace: { getConfiguration: () => ({ get: k => config[k] }) } } : original.call(this, req, ...rest); };
  let listener; const messages = [];
  const panel = { webview: { postMessage: m => messages.push(m), onDidReceiveMessage: f => { listener = f; return { dispose() {} }; } } };
  const lastPage = () => messages.filter(m => m.type === 'page').at(-1);
  const states = [{ history: [] }, { history: [] }];
  try {
    delete require.cache[require.resolve('../core')];
    const core = require('../core'); core.attach(panel, states[0]);
    listener({ type: 'go', href: 'strand:' });
    for (let i = 0; i < 100 && !(lastPage() && /turn-003/.test(lastPage().html)); i++) await new Promise(r => setTimeout(r, 10));
    const html = lastPage().html;
    assert.ok(asked.some(u => u.includes('session=session-invented')), 'opened the newest conversation: ' + asked);
    // switch line: the newest bold, the other a link; session-end and "pending" dropped
    assert.match(html, /your recent conversations: <b>agent-b<\/b>/);
    assert.match(html, /data-href="strand:session-other"/);
    assert.doesNotMatch(html, /session-ended|agent-c/);
    assert.doesNotMatch(html, /monitorSession|top items/, 'no setting keys or borrowed routes in what Joe reads');
    // 象's record down: a plain message, not a setting name
    delete require.cache[require.resolve('../core')];
    turnsUp = false; messages.length = 0;
    if (states[0].strandTimer) clearInterval(states[0].strandTimer);
    const core2 = require('../core'); core2.attach(panel, states[1]);
    listener({ type: 'go', href: 'strand:' });
    for (let i = 0; i < 100 && !(lastPage() && /did not answer/.test(lastPage().html)); i++) await new Promise(r => setTimeout(r, 10));
    assert.match(lastPage().html, /象's turn record did not answer/);
    assert.doesNotMatch(lastPage().html, /monitorSession/);
    // Joe: "r is supposed to 'reload'" -- once 象's record answers again, r clears the error
    turnsUp = true;
    listener({ type: 'reload' });
    for (let i = 0; i < 100 && !(lastPage() && /turn-003/.test(lastPage().html)); i++) await new Promise(r => setTimeout(r, 10));
    assert.match(lastPage().html, /your recent conversations/, 'r recovered from the error');
  } finally {
    for (const st of states) if (st.strandTimer) clearInterval(st.strandTimer);
    Module._load = original; server.closeAllConnections(); await new Promise(r => server.close(r)); fs.rmSync(dir, { recursive: true });
  }
});
