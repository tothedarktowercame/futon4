// evidence.js — Main application: routing, views, filters.
import { fetchEvidence, fetchEntry, fetchChain } from './evidence-api.js';
import {
  eget, typeLabel, typeClass, claimLabel, formatSubject,
  bodyPreview, formatTime, formatTimeShort, formatTags,
  renderDetail, renderChain
} from './evidence-render.js';

// -- State --

let currentFilters = {};
let refreshTimer = null;

// -- DOM helpers --

const $ = (sel) => document.querySelector(sel);
const $$ = (sel) => document.querySelectorAll(sel);

function replaceView(html) {
  const main = $('#view');
  if (typeof html === 'string') {
    main.innerHTML = html;
  } else {
    main.innerHTML = '';
    main.appendChild(html);
  }
}

function setLoading(msg = 'Loading...') {
  replaceView(`<div class="loading">${msg}</div>`);
}

function setError(msg) {
  replaceView(`<div class="error">${esc(msg)}</div>`);
}

function setTitle(text) {
  const el = $('#view-title');
  if (el) el.textContent = text;
}

function showFilters(show) {
  const el = $('#filter-bar');
  if (el) el.style.display = show ? '' : 'none';
}

// -- Router --

function route() {
  clearInterval(refreshTimer);
  const hash = (location.hash || '#/evidence').slice(1);
  const parts = hash.split('/').filter(Boolean);

  if (parts[0] === 'evidence' && parts.length === 1) {
    showFilters(true);
    showDashboard();
  } else if (parts[0] === 'evidence' && parts.length === 2) {
    showFilters(false);
    showEntryDetail(decodeURIComponent(parts[1]));
  } else if (parts[0] === 'evidence' && parts.length === 3 && parts[2] === 'chain') {
    showFilters(false);
    showChainView(decodeURIComponent(parts[1]));
  } else if (parts[0] === 'sessions' && parts.length === 1) {
    showFilters(false);
    showSessions();
  } else if (parts[0] === 'sessions' && parts.length === 2) {
    showFilters(true);
    showSessionTimeline(decodeURIComponent(parts[1]));
  } else {
    showFilters(true);
    showDashboard();
  }
}

// -- Dashboard view --

async function showDashboard() {
  setTitle('Evidence Landscape');
  setLoading();
  try {
    const data = await fetchEvidence(currentFilters);
    renderDashboard(data);
    // Auto-refresh every 30s
    refreshTimer = setInterval(async () => {
      try {
        const fresh = await fetchEvidence(currentFilters);
        renderDashboard(fresh);
      } catch (_) { /* silent refresh failure */ }
    }, 30000);
  } catch (err) {
    setError(`Failed to load evidence: ${err.message}`);
  }
}

function renderDashboard(data) {
  const { entries = [], count = 0 } = data;

  if (entries.length === 0) {
    replaceView(`<div class="empty-state">
      <p>No evidence entries found.</p>
      <p class="hint">Evidence is emitted by peripherals during agent sessions.</p>
    </div>`);
    updateCount(0);
    return;
  }

  const table = document.createElement('table');
  table.className = 'evidence-table';
  table.innerHTML = `<thead><tr>
    <th class="col-time">Time</th>
    <th class="col-type">Type</th>
    <th class="col-author">Author</th>
    <th class="col-reply">\u21b3</th>
    <th class="col-preview">Preview</th>
    <th class="col-subject">Subject</th>
  </tr></thead>`;

  const tbody = document.createElement('tbody');
  for (const entry of entries) {
    const row = document.createElement('tr');
    row.className = 'evidence-row';
    const id = eget(entry, 'id');
    row.dataset.id = id;
    row.onclick = () => { location.hash = `#/evidence/${encodeURIComponent(id)}`; };

    const type = eget(entry, 'type');
    const tclass = typeClass(type);
    const inReplyTo = eget(entry, 'in-reply-to');

    row.innerHTML = `
      <td class="col-time mono">${esc(formatTime(eget(entry, 'at')))}</td>
      <td class="col-type"><span class="type-badge ${tclass}">${esc(typeLabel(type))}</span></td>
      <td class="col-author">${esc(truncStr(eget(entry, 'author') || '', 12))}</td>
      <td class="col-reply">${inReplyTo ? '\u21b3' : ''}</td>
      <td class="col-preview">${esc(bodyPreview(eget(entry, 'body'), type))}</td>
      <td class="col-subject mono">${esc(formatSubject(eget(entry, 'subject')))}</td>
    `;
    tbody.appendChild(row);
  }
  table.appendChild(tbody);
  replaceView(table);
  updateCount(count);
}

function updateCount(n) {
  const el = $('#entry-count');
  if (el) el.textContent = n != null ? `${n} entries` : '';
}

// -- Detail view --

async function showEntryDetail(id) {
  setTitle('Evidence Entry');
  setLoading();
  try {
    const entry = await fetchEntry(id);
    replaceView(`
      <nav class="detail-nav">
        <a href="#/evidence" class="back-link">\u2190 Back to timeline</a>
      </nav>
      ${renderDetail(entry)}
    `);
  } catch (err) {
    setError(`Failed to load entry: ${err.message}`);
  }
}

// -- Chain view --

async function showChainView(id) {
  setTitle('Evidence Chain');
  setLoading();
  try {
    const data = await fetchChain(id);
    const chain = data.chain || [];
    replaceView(`
      <nav class="detail-nav">
        <a href="#/evidence/${encodeURIComponent(id)}" class="back-link">\u2190 Back to entry</a>
        <a href="#/evidence" class="back-link">\u2190 Timeline</a>
      </nav>
      <h2 class="chain-title">Reply chain (${chain.length} entries)</h2>
      ${renderChain(chain, id)}
    `);
  } catch (err) {
    setError(`Failed to load chain: ${err.message}`);
  }
}

// -- Sessions view --

async function showSessions() {
  setTitle('Sessions');
  setLoading();
  try {
    const data = await fetchEvidence({ limit: 1000 });
    const entries = data.entries || [];

    // Group by session-id
    const sessions = new Map();
    for (const entry of entries) {
      const sid = eget(entry, 'session-id');
      if (!sid) continue;
      if (!sessions.has(sid)) sessions.set(sid, []);
      sessions.get(sid).push(entry);
    }

    if (sessions.size === 0) {
      replaceView('<div class="empty-state"><p>No sessions found.</p></div>');
      return;
    }

    // Sort sessions by latest entry timestamp (newest first)
    const sorted = [...sessions.entries()].sort((a, b) => {
      const latestA = a[1][0] ? eget(a[1][0], 'at') || '' : '';
      const latestB = b[1][0] ? eget(b[1][0], 'at') || '' : '';
      return latestB.localeCompare(latestA);
    });

    const table = document.createElement('table');
    table.className = 'evidence-table sessions-table';
    table.innerHTML = `<thead><tr>
      <th>Session</th>
      <th>Entries</th>
      <th>Types</th>
      <th>Latest</th>
    </tr></thead>`;

    const tbody = document.createElement('tbody');
    for (const [sid, sentries] of sorted) {
      const row = document.createElement('tr');
      row.className = 'evidence-row';
      row.onclick = () => { location.hash = `#/sessions/${encodeURIComponent(sid)}`; };

      // Type summary
      const typeCounts = {};
      for (const e of sentries) {
        const t = typeLabel(eget(e, 'type'));
        typeCounts[t] = (typeCounts[t] || 0) + 1;
      }
      const typeSummary = Object.entries(typeCounts)
        .map(([t, c]) => `${t}:${c}`)
        .join(' ');

      const latest = sentries[0] ? formatTime(eget(sentries[0], 'at')) : '';

      row.innerHTML = `
        <td class="mono">${esc(truncStr(sid, 38))}</td>
        <td>${sentries.length}</td>
        <td>${esc(typeSummary)}</td>
        <td class="mono">${esc(latest)}</td>
      `;
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    replaceView(table);
  } catch (err) {
    setError(`Failed to load sessions: ${err.message}`);
  }
}

// -- Session timeline --

async function showSessionTimeline(sessionId) {
  setTitle(`Session: ${truncStr(sessionId, 30)}`);
  // Override filters temporarily
  const saved = { ...currentFilters };
  currentFilters['session-id'] = sessionId;
  setLoading();
  try {
    const data = await fetchEvidence(currentFilters);
    replaceView('');
    const nav = document.createElement('nav');
    nav.className = 'detail-nav';
    nav.innerHTML = `<a href="#/sessions" class="back-link">\u2190 Back to sessions</a>
      <a href="#/evidence" class="back-link">\u2190 Timeline</a>`;
    $('#view').appendChild(nav);

    const container = document.createElement('div');
    renderDashboardInto(container, data);
    $('#view').appendChild(container);
  } catch (err) {
    setError(`Failed to load session: ${err.message}`);
  } finally {
    currentFilters = saved;
  }
}

function renderDashboardInto(container, data) {
  const { entries = [] } = data;
  const table = document.createElement('table');
  table.className = 'evidence-table';
  table.innerHTML = `<thead><tr>
    <th class="col-time">Time</th>
    <th class="col-type">Type</th>
    <th class="col-author">Author</th>
    <th class="col-reply">\u21b3</th>
    <th class="col-preview">Preview</th>
    <th class="col-subject">Subject</th>
  </tr></thead>`;
  const tbody = document.createElement('tbody');
  for (const entry of entries) {
    const row = document.createElement('tr');
    row.className = 'evidence-row';
    const id = eget(entry, 'id');
    row.dataset.id = id;
    row.onclick = () => { location.hash = `#/evidence/${encodeURIComponent(id)}`; };
    const type = eget(entry, 'type');
    const tclass = typeClass(type);
    const inReplyTo = eget(entry, 'in-reply-to');
    row.innerHTML = `
      <td class="col-time mono">${esc(formatTime(eget(entry, 'at')))}</td>
      <td class="col-type"><span class="type-badge ${tclass}">${esc(typeLabel(type))}</span></td>
      <td class="col-author">${esc(truncStr(eget(entry, 'author') || '', 12))}</td>
      <td class="col-reply">${inReplyTo ? '\u21b3' : ''}</td>
      <td class="col-preview">${esc(bodyPreview(eget(entry, 'body'), type))}</td>
      <td class="col-subject mono">${esc(formatSubject(eget(entry, 'subject')))}</td>
    `;
    tbody.appendChild(row);
  }
  table.appendChild(tbody);
  container.appendChild(table);
}

// -- Filters --

function initFilters() {
  const typeSelect = $('#filter-type');
  const authorInput = $('#filter-author');
  const clearBtn = $('#filter-clear');

  if (typeSelect) {
    typeSelect.onchange = () => {
      const v = typeSelect.value;
      if (v) currentFilters.type = v;
      else delete currentFilters.type;
      route();
    };
  }
  if (authorInput) {
    let debounce;
    authorInput.oninput = () => {
      clearTimeout(debounce);
      debounce = setTimeout(() => {
        const v = authorInput.value.trim();
        if (v) currentFilters.author = v;
        else delete currentFilters.author;
        route();
      }, 400);
    };
  }
  if (clearBtn) {
    clearBtn.onclick = () => {
      currentFilters = {};
      if (typeSelect) typeSelect.value = '';
      if (authorInput) authorInput.value = '';
      route();
    };
  }
}

// -- Navigation --

function initNav() {
  const links = $$('[data-nav]');
  for (const link of links) {
    link.onclick = (e) => {
      e.preventDefault();
      location.hash = link.dataset.nav;
    };
  }
}

// -- Helpers --

function truncStr(s, max) {
  if (!s) return '';
  s = String(s);
  return s.length > max ? s.slice(0, max - 1) + '\u2026' : s;
}

function esc(s) {
  if (!s) return '';
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

// -- Init --

export function init() {
  initFilters();
  initNav();
  window.addEventListener('hashchange', route);
  route();
}

// Auto-init when loaded as module
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}
