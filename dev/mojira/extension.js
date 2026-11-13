// Mojira loader (futon4 M-mojira). Owns the panel and the browsing history,
// and hot-reloads core.js whenever it is saved, so a change shows up without
// reloading the window. Changes to this file or package.json still need
// "Developer: Reload Window".
'use strict';
const vscode = require('vscode');
const fs = require('fs');
const path = require('path');

const CORE = path.join(__dirname, 'core.js');
const state = { history: [] };   // survives core reloads
let core = null, panel = null, sub = null;

function load() {
  delete require.cache[require.resolve(CORE)];
  core = require(CORE);   // throws on a syntax error; `core` keeps the old one
}

function attach() {
  if (sub) sub.dispose();
  sub = core.attach(panel, state);
}

// Adopt P as the Mojira panel: a new one, or one VS Code restored after a
// window reload. Without the serializer below, a restored tab came back
// empty (Joe, 2026-11-13: "just showing a small 4-line hamburger icon").
function adopt(p, context) {
  panel = p;
  panel.webview.options = { enableScripts: true };
  panel.onDidDispose(() => { panel = null; if (sub) { sub.dispose(); sub = null; } }, null, context.subscriptions);
  try { attach(); } catch (e) { vscode.window.showErrorMessage('Mojira: ' + e.message); }
}

function open(context) {
  if (panel) { panel.reveal(); return; }
  adopt(vscode.window.createWebviewPanel('mojira', 'Mojira', vscode.ViewColumn.One,
    { enableScripts: true, retainContextWhenHidden: true }), context);
}

function watchCore(context) {
  let timer = null;
  const w = fs.watch(__dirname, (_ev, file) => {
    if (file !== 'core.js') return;
    clearTimeout(timer);
    timer = setTimeout(() => {
      try {
        load();
        if (panel) { attach(); core.note('reloaded core.js ' + new Date().toTimeString().slice(0, 8)); }
      } catch (e) {
        vscode.window.showErrorMessage('Mojira: core.js did not load, still running the previous one: ' + e.message);
      }
    }, 250);
  });
  context.subscriptions.push({ dispose: () => w.close() });
}

function activate(context) {
  try { load(); } catch (e) { vscode.window.showErrorMessage('Mojira: ' + e.message); return; }
  context.subscriptions.push(
    vscode.commands.registerCommand('mojira.open', () => open(context)),
    vscode.commands.registerCommand('mojira.reload', () => { if (panel) core.reloadData(); }),
    vscode.window.registerWebviewPanelSerializer('mojira', {
      async deserializeWebviewPanel(p) {
        if (panel) { p.dispose(); panel.reveal(); return; }   // one Mojira panel
        adopt(p, context);
      },
    }));
  watchCore(context);
  if (vscode.workspace.getConfiguration('mojira').get('openOnStartup')) open(context);
}

function deactivate() {}

module.exports = { activate, deactivate };
