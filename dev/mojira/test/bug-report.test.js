'use strict';
// Joe 2026-11-18: "mojira bugs attached routing to your REPL". ! asks for one
// line, gathers Mojira's context, runs the futon bug collector with it, and
// bells the report to mojira.bugAgent. The collector and sender here are
// invented stand-ins that record what they were given.
const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs'), os = require('os'), path = require('path'), Module = require('module');

test('! collects Mojira context and bells the report to the bug agent', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'mojira-bug-'));
  const scripts = path.join(dir, 'scripts'); fs.mkdirSync(scripts);
  fs.writeFileSync(path.join(dir, 'lex.json'), '[]'); fs.writeFileSync(path.join(dir, 'pending.tsv'), 'id\n');
  // collector: copy the context it was given into its report, print the path
  fs.writeFileSync(path.join(scripts, 'futon_bug_report.py'), `import sys, json
a = sys.argv[1:]
ctx = open(a[a.index('--emacs-context') + 1]).read()
out = ${JSON.stringify(path.join(dir, 'BUG-invented.md'))}
open(out, 'w').write('# invented report\\n' + json.dumps(a) + '\\n' + ctx)
print(out)
`);
  // sender: record argv and stdin
  fs.writeFileSync(path.join(scripts, 'agency_send.py'), `import sys, json
open(${JSON.stringify(path.join(dir, 'sent.json'))}, 'w').write(json.dumps({'argv': sys.argv[1:], 'stdin': sys.stdin.read()}))
print('{"accepted": true}')
`);
  const config = { lexiconPath: path.join(dir, 'lex.json'), pendingPath: path.join(dir, 'pending.tsv'),
    futon3cScripts: scripts, bugAgent: 'agent-invented' };
  const original = Module._load;
  Module._load = function(req, ...rest) {
    return req === 'vscode'
      ? { workspace: { getConfiguration: () => ({ get: (k) => config[k] }) },
          window: { showInputBox: () => Promise.resolve('the map page is blank') } }
      : original.call(this, req, ...rest);
  };
  let listener; const messages = [];
  const panel = { webview: { postMessage: (m) => messages.push(m), onDidReceiveMessage: (f) => { listener = f; return { dispose() {} }; } } };
  const st = { history: [] };
  try {
    delete require.cache[require.resolve('../core')];
    const core = require('../core'); core.attach(panel, st);
    listener({ type: 'go', href: 'home' });
    listener({ type: 'bug' });
    const sent = path.join(dir, 'sent.json');
    for (let i = 0; i < 300 && !(fs.existsSync(sent) && messages.some((m) => m.type === 'note' && /belled/.test(m.text))); i++) await new Promise((r) => setTimeout(r, 20));
    const report = fs.readFileSync(path.join(dir, 'BUG-invented.md'), 'utf8');
    assert.match(report, /"--summary", "the map page is blank"/);
    assert.match(report, /"--context-label", "Mojira"/);
    assert.match(report, /- page: home/, 'Mojira context: the page Joe was on');
    assert.match(report, /### Page text/);
    const s = JSON.parse(fs.readFileSync(sent, 'utf8'));
    assert.deepStrictEqual(s.argv.slice(0, 4), ['--to', 'agent-invented', '--from', 'mojira-bug']);
    assert.ok(s.argv.includes('--mode') && s.argv.includes('work'));
    assert.match(s.stdin, /^A Mojira bug report follows/);
    assert.match(s.stdin, /# invented report/, 'the whole report travels in the bell');
    assert.ok(messages.some((m) => m.type === 'note' && /bug report belled to agent-invented/.test(m.text)));
  } finally {
    if (st.strandTimer) clearInterval(st.strandTimer);
    Module._load = original; fs.rmSync(dir, { recursive: true, force: true });
  }
});
