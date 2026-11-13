// Mojira shortcodes in code-server: the same codes as Emacs's ※ prompt
// (futon4/dev/mojira-dispatch.el), read from the shared table
// futon4/dev/mojira-codes.json and the ~/code allowlist. Pure apart from
// the two readers; core.js runs the prompt and the actions.
//
//   p, i, y, o, g        fixed codes (each says what it does here, if anything)
//   <n> [<kind>]         a futonN repo, or one kind of thing in it: "6 d", "3c m"
//   r <name> [<kind>]    any other repo on the allowlist
//   c 9, x l, ...        agent REPLs: Emacs only, for now
//   ?                    the list
'use strict';
const fs = require('fs');
const path = require('path');

const TABLE = path.join(__dirname, '..', 'mojira-codes.json');
const ALLOWLIST = '/home/joe/code/futon0/system/zone/code-allowlist/allowlist.tsv';

function readTable(file = TABLE) { return JSON.parse(fs.readFileSync(file, 'utf8')); }

// [[code, repo name]] from the allowlist's "repo" rows.
function readRepos(file = ALLOWLIST) {
  let text = '';
  try { text = fs.readFileSync(file, 'utf8'); } catch (_) { return []; }
  const out = [];
  for (const line of text.split('\n')) {
    const [name, kind] = line.split('\t');
    if (kind !== 'repo' || !name) continue;
    out.push(['r ' + name, name]);
    const m = name.match(/^futon([0-9][0-9a-z]*)$/);
    if (m) out.push([m[1], name]);
  }
  return out;
}

// Every complete code, for the prompt's completion.
function allCodes(table, repos) {
  const codes = ['?'];
  for (const c of table.codes) codes.push(c.code);
  for (const [k] of repos) {
    codes.push(k);
    for (const kd of table.kinds) codes.push(k + ' ' + kd.key);
  }
  for (const a of table.agents) for (const m of table.agent_modals || []) codes.push(a.key + ' ' + m.key);
  return codes;
}

// What CODE means here: {page}, {dir, kind}, {help}, or {error}.
function resolve(code, table, repos, codeDir = '/home/joe/code') {
  const s = String(code || '').trim().replace(/\s+/g, ' ');
  if (s === '' || s === 'g') return { page: 'home' };
  if (s === '?') return { help: true };
  const fixed = table.codes.find((c) => c.code === s);
  if (fixed) {
    if (fixed.mojira && fixed.mojira.page) return { page: fixed.mojira.page };
    return { error: `${s} (${fixed.label}) works in Emacs only` };
  }
  const w = s.split(' ');
  const agent = table.agents.find((a) => a.key === w[0]);
  if (agent && w.length === 2 && (/^[0-9]+$/.test(w[1]) || (table.agent_modals || []).some((m) => m.key === w[1]))) {
    return { error: `${s}: ${agent.kind} REPLs are in Emacs; there are no agents in code-server Mojira yet` };
  }
  const named = w[0] === 'r';
  const key = named ? w.slice(0, 2).join(' ') : w[0];
  const kindKey = w[named ? 2 : 1];
  const repo = (repos.find(([k]) => k === key) || [])[1];
  if (!repo) return { error: `${s}: unknown shortcode (try ?)` };
  const root = path.join(codeDir, repo);
  if (!kindKey) return { dir: root, label: repo };
  const kind = table.kinds.find((k) => k.key === kindKey);
  if (!kind) return { error: `${s}: unknown kind ${kindKey} (try ?)` };
  const dir = kind.paths.map((p) => path.join(root, p)).find((d) => { try { return fs.statSync(d).isDirectory(); } catch (_) { return false; } });
  if (dir) return { dir, label: repo + ' ' + kind.label };
  if (kind.docs) return { dir: root, label: repo };   // no docs/: the repo, where its READMEs are
  return { error: `${repo} has no ${kind.label}` };
}

// One keystroke's worth of the Emacs prompt's behaviour, on input S:
// {exit: true} when S is a complete code that starts no longer one;
// {value} with a space added when every continuation needs one, or when the
// last character only fits after a space ("1b" + "d" -> "1b d");
// {next} the characters that can come next, and whether RET would act.
function step(s, codes) {
  let v = s;
  if (v.length > 1 && !/ $/.test(v.slice(0, -1))) {
    const spaced = v.slice(0, -1) + ' ' + v.slice(-1);
    if (!codes.some((c) => c.startsWith(v)) && codes.some((c) => c.startsWith(spaced))) v = spaced;
  }
  const longer = codes.filter((c) => c !== v && c.startsWith(v));
  if (codes.includes(v) && !longer.length) return { value: v, exit: true };
  if (longer.length && !v.endsWith(' ') && longer.every((c) => c.startsWith(v + ' '))) v += ' ';
  const next = [...new Set(codes.filter((c) => c.startsWith(v) && c.length > v.length)
    .map((c) => c[v.length]).filter((ch) => ch !== ' '))].sort();
  return { value: v, exit: false, ret: codes.includes(v.trim()), next };
}

function helpHtml(table, repos, esc) {
  const fixed = table.codes.map((c) => `<li><b>${esc(c.code)}</b> ${esc(c.label)}${c.mojira ? '' : ' <span class="dim">(Emacs only)</span>'}</li>`).join('');
  const futons = repos.filter(([k]) => !k.startsWith('r ')).sort().map(([k, n]) => `${esc(k)} ${esc(n)}`).join(' · ');
  return `<h1>Shortcodes <span class="dim">: here, ※ in Emacs (one table: futon4/dev/mojira-codes.json)</span></h1>
<ul>${fixed}</ul>
<h2>Repos</h2><p><b>&lt;n&gt; [kind]</b> a futon repo, <b>r &lt;name&gt; [kind]</b> any other: opened in Emacs (dired)</p><p class="dim">${futons}</p>
<h2>Kinds</h2><ul>${table.kinds.map((k) => `<li><b>${esc(k.key)}</b> ${esc(k.label)}</li>`).join('')}</ul>
<h2>Agents <span class="dim">(Emacs only, for now)</span></h2><ul>${table.agents.map((a) => `<li><b>${esc(a.key)} N</b> ${esc(a.kind)}-N</li>`).join('')}
${(table.agent_modals || []).map((m) => `<li><b>${table.agents.map((a) => a.key).join('/')} ${esc(m.key)}</b> ${esc(m.label)}</li>`).join('')}</ul>`;
}

module.exports = { TABLE, readTable, readRepos, allCodes, resolve, step, helpHtml };
