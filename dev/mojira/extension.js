// Mojira: a keyboard-first text browser for the GCC (futon4 M-mojira).
// Slice 1 (2026-11-10): reads the lexicon and the pending-work index from
// files and lets Joe walk entity -> relation -> entity -> pending item, in the
// manner of nero.el / Conkeror. Later slices read the JVM Arxana authority.
'use strict';
const vscode = require('vscode');
const fs = require('fs');
const path = require('path');

let panel = null;
let model = null;
const history = [];   // hrefs; last is current

function cfg(k) { return vscode.workspace.getConfiguration('mojira').get(k); }

function esc(s) {
  return String(s == null ? '' : s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

// ---------------------------------------------------------------- model

function loadModel() {
  const lex = JSON.parse(fs.readFileSync(cfg('lexiconPath'), 'utf8'));
  const ents = new Map();
  for (const e of lex) if (!ents.has(e.entity)) ents.set(e.entity, e);
  const alias = new Map();
  for (const e of lex) {
    const names = [e.entity, e.name || ''].concat(String(e.aka || '').split(',').map(x => x.trim()));
    for (const a of names) if (a && !alias.has(a.toLowerCase())) alias.set(a.toLowerCase(), e.entity);
  }
  // Raw relations, as written in the lexicon: "verb a, b; verb2 c".
  const out = new Map(), inc = new Map();
  const seen = new Set();
  for (const e of lex) {
    for (const part of String(e.relations || '').split(';')) {
      const m = part.match(/^\s*([a-z-]+)\s+(.*)$/);
      if (!m) continue;
      for (let t of m[2].replace(/\([^)]*\)/g, '').split(',')) {
        t = alias.get(t.trim().toLowerCase());
        if (!t || t === e.entity) continue;
        const key = e.entity + '\u0000' + m[1] + '\u0000' + t;
        if (seen.has(key)) continue;
        seen.add(key);
        push(out, e.entity, { verb: m[1], other: t });
        push(inc, t, { verb: m[1], other: e.entity });
      }
    }
  }
  // Pending work index.
  const lines = fs.readFileSync(cfg('pendingPath'), 'utf8').split('\n').filter(Boolean);
  const head = lines.shift().split('\t');
  const items = new Map(), naming = new Map();
  for (const l of lines) {
    const c = l.split('\t'), r = {};
    head.forEach((h, i) => { r[h] = c[i] || ''; });
    r.ents = r.entities.split(/\s+/).filter(x => ents.has(x));
    items.set(r.id, r);
    for (const x of r.ents) push(naming, x, r.id);
  }
  return { ents, out, inc, items, naming, files: findHoleFiles(), loadedAt: new Date() };
}

function push(m, k, v) { if (!m.has(k)) m.set(k, []); m.get(k).push(v); }

// Mission / excursion files by basename, so a pending item can open its file.
function findHoleFiles() {
  const root = '/home/joe/code', files = new Map();
  let repos = [];
  try { repos = fs.readdirSync(root); } catch (_) { return files; }
  for (const r of repos) {
    for (const sub of ['holes/missions', 'holes/excursions']) {
      const d = path.join(root, r, sub);
      let names = [];
      try { names = fs.readdirSync(d); } catch (_) { continue; }
      for (const n of names) {
        if (!n.endsWith('.md')) continue;
        const id = n.slice(0, -3);
        if (!files.has(id)) files.set(id, path.join(d, n));
      }
    }
  }
  return files;
}

// ---------------------------------------------------------------- pages

const link = (href, text, cls) => `<a class="ln${cls ? ' ' + cls : ''}" data-href="${esc(href)}">${esc(text)}</a>`;
const entLink = id => {
  const e = model.ents.get(id);
  return link('e:' + id, e ? (e.name || id) : id, e ? 'st-' + (e.status || 'unknown') : '');
};

function render(href) {
  const [kind, ...rest] = href.split(':');
  const arg = rest.join(':');
  if (kind === 'e') return pageEntity(arg);
  if (kind === 'p') return pagePending(arg);
  if (kind === 'status') return pageStatus(arg);
  if (kind === 'label') return pageLabel(arg);
  if (kind === 'search') return pageSearch(arg);
  if (kind === 'all') return pageAllPending();
  return pageHome();
}

function pageHome() {
  const byDemand = [...model.ents.keys()]
    .map(k => [k, (model.naming.get(k) || []).length])
    .filter(([, n]) => n > 0)
    .sort((a, b) => b[1] - a[1]);
  const statuses = count([...model.ents.values()].map(e => e.status || 'unknown'));
  const labels = count([...model.items.values()].map(r => r.pending_label || '(none)'));
  return {
    title: 'Mojira',
    html: `<h1>Mojira <span class="dim">— the GCC, as text</span></h1>
<p class="dim">${model.ents.size} lexicon entities · ${model.items.size} pending items · loaded ${esc(model.loadedAt.toISOString().slice(11, 19))} UTC</p>
<h2>Entities by status</h2><ul>${statuses.map(([s, n]) => `<li>${link('status:' + s, s, 'st-' + s)} <span class="dim">${n}</span></li>`).join('')}</ul>
<h2>Pending work by state</h2><ul>${labels.map(([s, n]) => `<li>${link('label:' + s, s)} <span class="dim">${n}</span></li>`).join('')}
<li>${link('all', 'all pending items')}</li></ul>
<h2>Where the pending work is <span class="dim">(entities named by most pending items)</span></h2>
<ul>${byDemand.slice(0, 40).map(([k, n]) => `<li>${entLink(k)} <span class="dim">${n}</span></li>`).join('')}</ul>`
  };
}

function count(xs) {
  const m = new Map();
  for (const x of xs) m.set(x, (m.get(x) || 0) + 1);
  return [...m.entries()].sort((a, b) => b[1] - a[1]);
}

function pageEntity(id) {
  const e = model.ents.get(id);
  if (!e) return { title: id, html: `<h1>${esc(id)}</h1><p>Not in the lexicon.</p>` };
  const group = rels => {
    const g = new Map();
    for (const r of rels || []) push(g, r.verb, r.other);
    return [...g.entries()].map(([v, os]) => `<li><span class="verb">${esc(v)}</span> ${os.map(entLink).join(', ')}</li>`).join('');
  };
  const pend = model.naming.get(id) || [];
  const field = (k, label) => e[k] ? `<tr><th>${label}</th><td>${esc(e[k])}</td></tr>` : '';
  return {
    title: e.name || id,
    html: `<h1>${esc(e.name || id)} <span class="dim">${esc(id)}</span> ${link('status:' + (e.status || 'unknown'), '[' + (e.status || 'unknown') + ']', 'st-' + (e.status || 'unknown'))}</h1>
<p>${esc(e.what)}</p>
<table>${field('aka', 'aka')}${field('where', 'where')}${field('since', 'since')}${field('src', 'lexicon part')}${field('evidence', 'evidence')}</table>
<h2>Relations</h2><ul>${group(model.out.get(id)) || '<li class="dim">none written</li>'}</ul>
<h2>Named by others</h2><ul>${group(model.inc.get(id)) || '<li class="dim">none</li>'}</ul>
<h2>Pending work naming it <span class="dim">${pend.length}</span></h2>
<ul>${pend.map(p => pendingLi(p)).join('') || '<li class="dim">none</li>'}</ul>`
  };
}

function pendingLi(pid) {
  const r = model.items.get(pid);
  return `<li>${link('p:' + pid, pid)} <span class="dim">${esc(r ? r.pending_label : '')}</span></li>`;
}

function pagePending(pid) {
  const r = model.items.get(pid);
  if (!r) return { title: pid, html: `<h1>${esc(pid)}</h1><p>Not in the pending index.</p>` };
  const file = model.files.get(pid);
  const row = (k, label) => r[k] ? `<tr><th>${label}</th><td>${esc(r[k])}</td></tr>` : '';
  return {
    title: pid,
    html: `<h1>${esc(pid)}</h1>
<p>${esc(r.one_line)}</p>
<table><tr><th>state</th><td>${link('label:' + (r.pending_label || '(none)'), r.pending_label || '(none)')} <span class="dim">(${esc(r.pending)})</span></td></tr>
${row('held', 'held')}${row('benefit', 'benefit')}${row('harm_line', 'harm line')}
<tr><th>file</th><td>${file ? link('file:' + file, file.replace('/home/joe/code/', ''), 'file') : '<span class="dim">not found under */holes</span>'}</td></tr></table>
<h2>Entities it names</h2><ul>${r.ents.map(x => '<li>' + entLink(x) + '</li>').join('') || '<li class="dim">none</li>'}</ul>`
  };
}

function pageStatus(s) {
  const ks = [...model.ents.values()].filter(e => (e.status || 'unknown') === s)
    .sort((a, b) => (a.name || a.entity).localeCompare(b.name || b.entity));
  return { title: 'status ' + s, html: `<h1>Entities with status ${esc(s)} <span class="dim">${ks.length}</span></h1>
<ul>${ks.map(e => `<li>${entLink(e.entity)} <span class="dim">${esc(trim(e.what, 90))}</span></li>`).join('')}</ul>` };
}

function pageLabel(s) {
  const rs = [...model.items.values()].filter(r => (r.pending_label || '(none)') === s);
  return { title: s, html: `<h1>Pending: ${esc(s)} <span class="dim">${rs.length}</span></h1>
<ul>${rs.map(r => `<li>${link('p:' + r.id, r.id)} <span class="dim">${esc(trim(r.one_line, 90))}</span></li>`).join('')}</ul>` };
}

function pageAllPending() {
  const rs = [...model.items.values()].sort((a, b) => a.id.localeCompare(b.id));
  return { title: 'all pending', html: `<h1>All pending items <span class="dim">${rs.length}</span></h1>
<ul>${rs.map(r => `<li>${link('p:' + r.id, r.id)} <span class="dim">${esc(r.pending_label)}</span></li>`).join('')}</ul>` };
}

function pageSearch(q) {
  const ql = q.toLowerCase();
  const hit = s => String(s || '').toLowerCase().includes(ql);
  const es = [...model.ents.values()].filter(e => hit(e.entity) || hit(e.name) || hit(e.aka) || hit(e.what));
  const ps = [...model.items.values()].filter(r => hit(r.id) || hit(r.one_line));
  return { title: '/' + q, html: `<h1>/${esc(q)}</h1>
<h2>Entities <span class="dim">${es.length}</span></h2><ul>${es.map(e => `<li>${entLink(e.entity)} <span class="dim">${esc(trim(e.what, 80))}</span></li>`).join('') || '<li class="dim">none</li>'}</ul>
<h2>Pending items <span class="dim">${ps.length}</span></h2><ul>${ps.map(r => `<li>${link('p:' + r.id, r.id)} <span class="dim">${esc(trim(r.one_line, 80))}</span></li>`).join('') || '<li class="dim">none</li>'}</ul>` };
}

function trim(s, n) { s = String(s || ''); return s.length > n ? s.slice(0, n - 1) + '…' : s; }

// ---------------------------------------------------------------- panel

function go(href, push = true) {
  if (href.startsWith('file:')) {
    // Always the second column, reused: Mojira on the left, one target on the
    // right, never a growing stack of panes.
    vscode.window.showTextDocument(vscode.Uri.file(href.slice(5)),
      { viewColumn: vscode.ViewColumn.Two, preview: true, preserveFocus: true });
    return;
  }
  if (push) history.push(href);
  let page;
  try { page = render(href); } catch (e) { page = { title: 'error', html: `<pre>${esc(e.stack)}</pre>` }; }
  panel.title = 'Mojira: ' + page.title;
  panel.webview.postMessage({ type: 'page', html: page.html, crumbs: history.slice(-6) });
}

// 2-up: the right column follows the selected link on the left.
function peek(href) {
  if (!panel) return;
  let page;
  if (!href) page = { html: '' };
  else if (href.startsWith('file:')) {
    const f = href.slice(5);
    page = { html: `<h1>${esc(path.basename(f))}</h1><p class="dim">${esc(f)}</p><p class="dim">Enter opens it in the editor column on the right.</p>` };
  } else {
    try { page = render(href); } catch (e) { page = { html: `<pre>${esc(e.message)}</pre>` }; }
  }
  panel.webview.postMessage({ type: 'peek', href, html: page.html });
}

function open(context) {
  if (panel) { panel.reveal(); return; }
  panel = vscode.window.createWebviewPanel('mojira', 'Mojira', vscode.ViewColumn.One,
    { enableScripts: true, retainContextWhenHidden: true });
  panel.webview.html = shell();
  panel.onDidDispose(() => { panel = null; }, null, context.subscriptions);
  panel.webview.onDidReceiveMessage(m => {
    if (m.type === 'ready') go(history.length ? history[history.length - 1] : 'home', !history.length);
    else if (m.type === 'go') go(m.href);
    else if (m.type === 'peek') peek(m.href);
    // Browsing from the right column (Miller columns): the right page moves
    // left, and the link chosen there opens on the right.
    else if (m.type === 'shift') { go(m.main); peek(m.peek); }
    else if (m.type === 'back') { if (history.length > 1) { history.pop(); go(history[history.length - 1], false); } }
    else if (m.type === 'home') go('home');
    else if (m.type === 'search' && m.q) go('search:' + m.q);
    else if (m.type === 'reload') { model = loadModel(); go(history[history.length - 1] || 'home', false); }
  }, null, context.subscriptions);
}

function shell() {
  return `<!doctype html><html><head><meta charset="utf-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline';">
<style>
body { font-family: var(--vscode-editor-font-family, monospace); font-size: var(--vscode-editor-font-size, 14px);
  color: var(--vscode-editor-foreground); background: var(--vscode-editor-background); padding: 0 1.2em; margin: 0; line-height: 1.45;
  height: 100vh; display: flex; flex-direction: column; overflow: hidden; }
#cols { flex: 1; display: grid; grid-template-columns: 1fr 1fr; gap: 0 1.5em; min-height: 0; }
body.one #cols { grid-template-columns: 1fr; } body.one #peek { display: none; }
#page, #peek { overflow-y: auto; padding-bottom: 3em; min-width: 0; }
#peek { border-left: 1px solid var(--vscode-panel-border); padding-left: 1.2em; }
body:not(.act-peek) #peek, body.act-peek #page { opacity: .72; }
body.act-peek #peek { border-left-color: var(--vscode-focusBorder); }
#peek:empty::before { content: 'select a link (j/k) to see it here'; color: var(--vscode-descriptionForeground); display: block; margin-top: 1em; }
@media (max-width: 760px) { #cols { grid-template-columns: 1fr; } #peek { display: none; } }
h1 { font-size: 1.25em; margin: .8em 0 .4em; } h2 { font-size: 1em; margin: 1.2em 0 .3em; text-transform: lowercase; color: var(--vscode-textLink-foreground); }
ul { list-style: none; padding-left: 1em; margin: 0; } li { margin: .1em 0; }
table { border-collapse: collapse; } th { text-align: left; vertical-align: top; padding-right: 1em; font-weight: normal; color: var(--vscode-descriptionForeground); white-space: nowrap; }
.dim { color: var(--vscode-descriptionForeground); font-weight: normal; }
.verb { color: var(--vscode-descriptionForeground); display: inline-block; min-width: 7em; }
a.ln { color: var(--vscode-textLink-foreground); text-decoration: none; cursor: pointer; border-bottom: 1px dotted transparent; }
a.ln:hover { border-bottom-color: currentColor; }
a.ln.sel { outline: 1px solid var(--vscode-focusBorder); background: var(--vscode-editor-selectionBackground); }
a.st-exists { } a.st-partly { font-style: italic; } a.st-hypothesised { font-style: italic; opacity: .75; } a.st-retired { text-decoration: line-through; opacity: .6; }
.hint { position: absolute; background: var(--vscode-editorWarning-foreground, #c90); color: #000; font-size: 11px; font-weight: bold; padding: 0 3px; border-radius: 2px; transform: translate(-100%, -2px); z-index: 9; }
#bar { position: sticky; top: 0; background: var(--vscode-editor-background); padding: .4em 0; border-bottom: 1px solid var(--vscode-panel-border); color: var(--vscode-descriptionForeground); font-size: .85em; }
#q { display: none; width: 30em; font: inherit; color: inherit; background: var(--vscode-input-background); border: 1px solid var(--vscode-focusBorder); }
</style></head><body>
<div id="bar"><span id="crumbs"></span> <input id="q" placeholder="search, then Enter"><span id="keys"> · j/k move · Enter follow · Tab other column · f hints · h back · / search · g home · v split · r reload</span></div>
<div id="cols"><div id="page">loading…</div><div id="peek"></div></div>
<script>
const vs = acquireVsCodeApi();
let active = 'page', hints = null, hintBuf = '', peekHref = null, shifting = false;
const sel = { page: -1, peek: -1 };
const $ = s => document.querySelector(s);
const links = (col = active) => [...document.querySelectorAll('#' + col + ' a.ln')];
const peekShown = () => getComputedStyle($('#peek')).display !== 'none';
function setActive(col) {
  active = col;
  document.body.classList.toggle('act-peek', col === 'peek');
}
function select(i) {
  const ls = links(); if (!ls.length) return;
  sel[active] = Math.max(0, Math.min(ls.length - 1, i));
  ls.forEach((a, j) => a.classList.toggle('sel', j === sel[active]));
  ls[sel[active]].scrollIntoView({ block: 'nearest' });
  if (active !== 'page') return;
  clearTimeout(peekTimer);
  const href = ls[sel.page].dataset.href;
  peekTimer = setTimeout(() => vs.postMessage({ type: 'peek', href }), 60);
}
let peekTimer = null;
function enter() {
  const a = links()[sel[active]];
  if (!a) return;
  const href = a.dataset.href;
  if (active === 'peek' && peekHref && !href.startsWith('file:') && !peekHref.startsWith('file:')) {
    shifting = true;
    vs.postMessage({ type: 'shift', main: peekHref, peek: href });
  } else follow(a);
}
function follow(a) { if (a) vs.postMessage({ type: 'go', href: a.dataset.href }); }
const HC = 'asdfghjklqwertyuiopzxcvbnm';
function hintLabel(i, n) {
  if (n <= HC.length) return HC[i];
  return HC[Math.floor(i / HC.length)] + HC[i % HC.length];
}
function showHints() {
  const top = window.scrollY, bot = top + window.innerHeight;
  const vis = [...document.querySelectorAll('#page a.ln, #peek a.ln')].filter(a => { const r = a.getBoundingClientRect(); return r.bottom > 0 && r.top < window.innerHeight && r.width > 0; });
  hints = vis.map((a, i) => {
    const r = a.getBoundingClientRect(), h = document.createElement('span');
    h.className = 'hint'; h.textContent = hintLabel(i, vis.length);
    h.style.left = (r.left + window.scrollX) + 'px'; h.style.top = (r.top + window.scrollY) + 'px';
    document.body.appendChild(h); return { a, h, label: h.textContent };
  });
  hintBuf = '';
}
function clearHints() { (hints || []).forEach(x => x.h.remove()); hints = null; hintBuf = ''; }
window.addEventListener('message', ev => {
  const m = ev.data;
  if (m.type === 'page') {
    clearHints();
    $('#page').innerHTML = m.html;
    $('#crumbs').textContent = m.crumbs.map(c => c.replace(/^[a-z]+:/, '')).join(' › ');
    sel.page = -1; sel.peek = -1; peekHref = null;
    $('#page').scrollTop = 0; $('#peek').innerHTML = '';
    setActive(shifting ? 'peek' : 'page'); shifting = false;
  } else if (m.type === 'peek') {
    $('#peek').innerHTML = m.html; $('#peek').scrollTop = 0;
    peekHref = m.href; sel.peek = -1;
  }
});
document.addEventListener('click', ev => { const a = ev.target.closest('a.ln'); if (a) follow(a); });
$('#q').addEventListener('keydown', ev => {
  if (ev.key === 'Enter') { vs.postMessage({ type: 'search', q: ev.target.value.trim() }); ev.target.style.display = 'none'; ev.target.blur(); }
  if (ev.key === 'Escape') { ev.target.style.display = 'none'; ev.target.blur(); }
  ev.stopPropagation();
});
document.addEventListener('keydown', ev => {
  if (ev.ctrlKey || ev.metaKey || ev.altKey) return;
  if (hints) {
    if (ev.key === 'Escape') { clearHints(); return; }
    hintBuf += ev.key;
    const m = hints.filter(x => x.label.startsWith(hintBuf));
    if (m.length === 1 && m[0].label === hintBuf) { const a = m[0].a; clearHints(); follow(a); }
    else if (!m.length) clearHints();
    ev.preventDefault(); return;
  }
  const k = ev.key;
  if (k === 'j' || k === 'ArrowDown') select(sel[active] + 1);
  else if (k === 'k' || k === 'ArrowUp') select(sel[active] - 1);
  else if (k === 'Enter' || k === 'l') enter();
  else if (k === 'Tab') {
    if (active === 'page' && peekShown() && links('peek').length) setActive('peek');
    else setActive('page');
  }
  else if (k === 'h' || k === 'Backspace') vs.postMessage({ type: 'back' });
  else if (k === 'g') vs.postMessage({ type: 'home' });
  else if (k === 'f') showHints();
  else if (k === 'r') vs.postMessage({ type: 'reload' });
  else if (k === 'v') { document.body.classList.toggle('one'); if (!peekShown()) setActive('page'); try { vs.setState({ one: document.body.classList.contains('one') }); } catch (_) {} }
  else if (k === '/') { const q = $('#q'); q.style.display = 'inline'; q.value = ''; q.focus(); }
  else if (k === ' ') $('#' + active).scrollBy(0, $('#' + active).clientHeight * 0.8);
  else return;
  ev.preventDefault();
});
try { if ((vs.getState() || {}).one) document.body.classList.add('one'); } catch (_) {}
vs.postMessage({ type: 'ready' });
</script></body></html>`;
}

function activate(context) {
  try { model = loadModel(); } catch (e) { vscode.window.showErrorMessage('Mojira: could not load data: ' + e.message); return; }
  context.subscriptions.push(
    vscode.commands.registerCommand('mojira.open', () => open(context)),
    vscode.commands.registerCommand('mojira.reload', () => { model = loadModel(); if (panel) go(history[history.length - 1] || 'home', false); }));
  if (cfg('openOnStartup')) open(context);
}

function deactivate() {}

module.exports = { activate, deactivate, _test: { loadModel: () => (model = loadModel()), render, shell } };
