// evidence.js — Main application: routing, views, filters.
import { fetchEvidence, fetchEntry, fetchChain } from './evidence-api.js';
import {
  eget, typeLabel, typeClass, claimLabel, formatSubject,
  bodyPreview, formatTime, formatTimeShort, formatTags,
  renderDetail, renderChain, renderThreadCard
} from './evidence-render.js';

// -- State --

const FILTER_KEYS = ['type', 'author'];

let currentFilters = {};
let refreshTimer = null;
const filterEls = { type: null, author: null };
let livePaused = false;
let liveRefreshFn = null;
const liveControls = { container: null, dot: null, label: null, toggle: null };
let detailRowEl = null;
let themeToggleEl = null;
let openDetailId = null;

// -- DOM helpers --

const $ = (sel) => document.querySelector(sel);

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

function parseHashParts() {
  let raw = location.hash || '';
  if (raw.startsWith('#')) raw = raw.slice(1);
  raw = raw || '/evidence';
  if (!raw.startsWith('/')) raw = `/${raw}`;
  const [pathPartRaw, queryString = ''] = raw.split('?');
  const pathPart = pathPartRaw || '/evidence';
  const parts = pathPart.split('/').filter(Boolean);
  return { pathPart, parts, queryString };
}

function filtersFromQuery(queryString = '') {
  const params = new URLSearchParams(queryString);
  const result = {};
  for (const key of FILTER_KEYS) {
    const value = params.get(key);
    if (value) result[key] = value;
  }
  return result;
}

function filtersToQuery(filters = currentFilters) {
  const params = new URLSearchParams();
  for (const key of FILTER_KEYS) {
    const value = filters[key];
    if (value) params.set(key, value);
  }
  return params.toString();
}

function updateFilterControls() {
  if (filterEls.type) filterEls.type.value = currentFilters.type || '';
  if (filterEls.author) filterEls.author.value = currentFilters.author || '';
}

function syncFiltersFromQuery(queryString) {
  currentFilters = filtersFromQuery(queryString);
  updateFilterControls();
}

function buildHashWithFilters(targetHash) {
  const normalized = targetHash.startsWith('#') ? targetHash.slice(1) : targetHash;
  const base = normalized ? (normalized.startsWith('/') ? normalized : `/${normalized}`) : '/evidence';
  const query = filtersToQuery();
  return query ? `#${base}?${query}` : `#${base}`;
}

function navigateWithFilters(targetHash) {
  location.hash = buildHashWithFilters(targetHash);
}

function persistFiltersToHash() {
  const { pathPart } = parseHashParts();
  const target = pathPart || '/evidence';
  const nextHash = buildHashWithFilters(target);
  if (location.hash === nextHash) {
    route();
  } else {
    location.hash = nextHash;
  }
}

function wireNavLinks(root = document) {
  if (!root) return;
  const links = root.querySelectorAll('[data-nav]');
  for (const link of links) {
    if (link.dataset.navBound === 'true') continue;
    link.addEventListener('click', (e) => {
      e.preventDefault();
      const target = link.dataset.nav || link.getAttribute('href') || '#/evidence';
      navigateWithFilters(target);
    });
    link.dataset.navBound = 'true';
  }
}

function setLiveIndicatorVisible(show) {
  if (!liveControls.container) return;
  liveControls.container.style.display = show ? 'flex' : 'none';
}

function updateLiveIndicatorUI() {
  if (!liveControls.container) return;
  const hasRefresh = typeof liveRefreshFn === 'function';
  const active = hasRefresh && !livePaused;
  if (liveControls.dot) {
    liveControls.dot.classList.toggle('live', active);
    liveControls.dot.classList.toggle('paused', !active);
  }
  if (liveControls.label) {
    liveControls.label.textContent = active ? 'Live' : 'Paused';
  }
  if (liveControls.toggle) {
    liveControls.toggle.textContent = livePaused ? '▶' : '⏸';
    liveControls.toggle.setAttribute('aria-label', livePaused ? 'Resume live updates' : 'Pause live updates');
    liveControls.toggle.setAttribute('aria-pressed', livePaused ? 'true' : 'false');
    liveControls.toggle.disabled = !hasRefresh;
  }
  if (!hasRefresh) {
    liveControls.container.style.display = 'none';
  }
}

function restartLiveRefreshTimer() {
  clearInterval(refreshTimer);
  if (!livePaused && typeof liveRefreshFn === 'function') {
    refreshTimer = setInterval(() => {
      const result = liveRefreshFn();
      if (result && typeof result.catch === 'function') {
        result.catch(() => {});
      }
    }, 30000);
  } else {
    refreshTimer = null;
  }
  updateLiveIndicatorUI();
}

function configureLiveRefresh(fn) {
  liveRefreshFn = fn;
  setLiveIndicatorVisible(true);
  restartLiveRefreshTimer();
}

function disableLiveRefresh() {
  liveRefreshFn = null;
  clearInterval(refreshTimer);
  refreshTimer = null;
  setLiveIndicatorVisible(false);
  updateLiveIndicatorUI();
}

function closeInlineDetail() {
  if (detailRowEl && detailRowEl.parentNode) {
    detailRowEl.parentNode.removeChild(detailRowEl);
  }
  detailRowEl = null;
  openDetailId = null;
}

function toggleInlineDetail(row, entryId) {
  if (!row) return;
  if (openDetailId === entryId) {
    closeInlineDetail();
  } else {
    openInlineDetail(row, entryId);
  }
}

function openInlineDetail(row, entryId, { preserve = false, autoScroll = true } = {}) {
  if (!preserve) {
    closeInlineDetail();
  } else if (detailRowEl && detailRowEl.parentNode) {
    detailRowEl.parentNode.removeChild(detailRowEl);
    detailRowEl = null;
  } else {
    detailRowEl = null;
  }
  openDetailId = entryId;
  const detailRow = document.createElement('tr');
  detailRow.className = 'evidence-detail-row';
  const detailCell = document.createElement('td');
  const table = row.closest('table');
  const columnCount = row.children.length || table?.querySelectorAll('thead th').length || 6;
  detailCell.colSpan = columnCount;
  const card = document.createElement('div');
  card.className = 'inline-card';
  card.innerHTML = '<p class="loading">Loading entry...</p>';
  detailCell.appendChild(card);
  detailRow.appendChild(detailCell);
  row.insertAdjacentElement('afterend', detailRow);
  detailRowEl = detailRow;
  if (autoScroll) {
    row.scrollIntoView({ behavior: 'smooth', block: 'center' });
  }
  renderInlineDetail(card, entryId);
}

async function renderInlineDetail(container, entryId) {
  try {
    const entry = await fetchEntry(entryId);
    if (openDetailId !== entryId || !container) return;
    const detailHtml = renderDetail(entry);
    const footer = `<div class="detail-nav inline-detail-footer"><a data-nav="#/evidence/${encodeURIComponent(entryId)}" class="back-link">Open full detail \u2192</a></div>`;
    container.innerHTML = detailHtml + footer;
    decorateInlineLinks(container);
  } catch (err) {
    if (openDetailId === entryId && container) {
      container.innerHTML = `<p class="error">Failed to load entry: ${esc(err?.message || err)}</p>`;
    }
  }
}

function decorateInlineLinks(root) {
  if (!root) return;
  root.querySelectorAll('a[href^="#/"]').forEach((anchor) => {
    if (!anchor.dataset.nav) {
      anchor.dataset.nav = anchor.getAttribute('href');
    }
  });
  wireNavLinks(root);
}

function reopenInlineDetail(root, entryId) {
  if (!entryId || !root) return;
  const safeId = cssEscape(entryId);
  const targetRow = root.querySelector(`tr[data-id="${safeId}"]`);
  if (targetRow) {
    openInlineDetail(targetRow, entryId, { preserve: true, autoScroll: false });
  } else {
    closeInlineDetail();
  }
}

// -- Router --

function route() {
  disableLiveRefresh();
  closeInlineDetail();
  const { parts, queryString } = parseHashParts();
  syncFiltersFromQuery(queryString);
  const primary = parts[0] || 'evidence';

  if (primary === 'evidence' && parts.length === 1) {
    showFilters(true);
    showThreads();
  } else if (primary === 'timeline' && parts.length === 1) {
    showFilters(true);
    showDashboard();
  } else if (primary === 'evidence' && parts.length === 2) {
    showFilters(false);
    showEntryDetail(decodeURIComponent(parts[1]));
  } else if (primary === 'evidence' && parts.length === 3 && parts[2] === 'chain') {
    showFilters(false);
    showChainView(decodeURIComponent(parts[1]));
  } else if (primary === 'sessions' && parts.length === 1) {
    showFilters(false);
    showSessions();
  } else if (primary === 'sessions' && parts.length === 2) {
    showFilters(true);
    showSessionTimeline(decodeURIComponent(parts[1]));
  } else {
    showFilters(true);
    showThreads();
  }
}

// -- Dashboard view --

async function showDashboard() {
  setTitle('Evidence Landscape');
  setLoading();
  try {
    let firstLoad = true;
    const refresh = async () => {
      const data = await fetchEvidence(currentFilters);
      renderDashboard(data, { preserveDetail: !firstLoad });
      firstLoad = false;
    };
    await refresh();
    configureLiveRefresh(refresh);
  } catch (err) {
    setError(`Failed to load evidence: ${err.message}`);
    disableLiveRefresh();
  }
}

function renderDashboard(data, { preserveDetail = false } = {}) {
  const previousDetailId = preserveDetail ? openDetailId : null;
  if (!preserveDetail) {
    closeInlineDetail();
  } else {
    detailRowEl = null;
  }
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
    row.onclick = () => toggleInlineDetail(row, id);

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
  if (preserveDetail && previousDetailId) {
    reopenInlineDetail(table, previousDetailId);
  }
  updateCount(count);
}

function updateCount(n) {
  const el = $('#entry-count');
  if (el) el.textContent = n != null ? `${n} entries` : '';
}

// -- Thread view --

function groupIntoThreads(entries) {
  const sessionMap = new Map();
  const orphans = [];

  for (const entry of entries) {
    const sid = eget(entry, 'session-id');
    if (!sid) {
      orphans.push(entry);
      continue;
    }
    if (!sessionMap.has(sid)) {
      sessionMap.set(sid, []);
    }
    sessionMap.get(sid).push(entry);
  }

  const threads = [];
  for (const [sid, sentries] of sessionMap) {
    const participants = new Set();
    let transport = null;
    let turnCount = 0;
    let lastMessage = '';

    for (const e of sentries) {
      const author = eget(e, 'author');
      if (author) participants.add(author);

      const body = eget(e, 'body');
      if (!transport && body?.transport) {
        transport = body.transport;
      }

      if (body?.event === 'chat-turn') {
        turnCount++;
        if (!lastMessage && body.text) lastMessage = body.text;
      }
    }

    // Entries come newest-first from API
    const lastAt = sentries.length > 0 ? eget(sentries[0], 'at') : null;
    const firstAt = sentries.length > 0 ? eget(sentries[sentries.length - 1], 'at') : null;

    threads.push({
      sessionId: sid,
      participants: [...participants],
      transport,
      entryCount: sentries.length,
      turnCount,
      firstAt,
      lastAt,
      lastMessage
    });
  }

  // Sort threads by latest entry (newest first)
  threads.sort((a, b) => (b.lastAt || '').localeCompare(a.lastAt || ''));

  // Add system events card if any orphans
  if (orphans.length > 0) {
    threads.push({
      sessionId: null,
      participants: [...new Set(orphans.map(e => eget(e, 'author')).filter(Boolean))],
      transport: null,
      entryCount: orphans.length,
      turnCount: 0,
      firstAt: orphans.length > 0 ? eget(orphans[orphans.length - 1], 'at') : null,
      lastAt: orphans.length > 0 ? eget(orphans[0], 'at') : null,
      lastMessage: ''
    });
  }

  return threads;
}

async function showThreads() {
  setTitle('Evidence Landscape');
  setLoading();
  try {
    let firstLoad = true;
    const refresh = async () => {
      const data = await fetchEvidence({ ...currentFilters, limit: 1000 });
      const threads = groupIntoThreads(data.entries || []);
      renderThreadList(threads, data.count || 0);
      firstLoad = false;
    };
    await refresh();
    configureLiveRefresh(refresh);
  } catch (err) {
    setError(`Failed to load evidence: ${err.message}`);
    disableLiveRefresh();
  }
}

function renderThreadList(threads, totalCount) {
  if (threads.length === 0) {
    replaceView(`<div class="empty-state">
      <p>No threads found.</p>
      <p class="hint">Evidence is emitted by peripherals during agent sessions.</p>
    </div>`);
    updateCount(0);
    return;
  }

  const container = document.createElement('div');
  container.className = 'thread-list';

  for (const thread of threads) {
    const card = document.createElement('div');
    card.className = 'thread-card';
    if (thread.sessionId) {
      card.onclick = () => {
        navigateWithFilters(`#/sessions/${encodeURIComponent(thread.sessionId)}`);
      };
    }
    card.innerHTML = renderThreadCard(thread);
    container.appendChild(card);
  }

  replaceView(container);
  updateCount(totalCount);
}

// -- Detail view --

async function showEntryDetail(id) {
  setTitle('Evidence Entry');
  setLoading();
  try {
    const entry = await fetchEntry(id);
    replaceView(`
      <nav class="detail-nav">
        <a href="#/evidence" data-nav="#/evidence" class="back-link">\u2190 Back to timeline</a>
      </nav>
      ${renderDetail(entry)}
    `);
    wireNavLinks($('#view'));
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
        <a href="#/evidence/${encodeURIComponent(id)}" data-nav="#/evidence/${encodeURIComponent(id)}" class="back-link">\u2190 Back to entry</a>
        <a href="#/evidence" data-nav="#/evidence" class="back-link">\u2190 Timeline</a>
      </nav>
      <h2 class="chain-title">Reply chain (${chain.length} entries)</h2>
      ${renderChain(chain, id)}
    `);
    wireNavLinks($('#view'));
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
      row.onclick = () => { navigateWithFilters(`#/sessions/${encodeURIComponent(sid)}`); };

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
  setLoading();
  try {
    replaceView('');
    const nav = document.createElement('nav');
    nav.className = 'detail-nav';
    nav.innerHTML = `<a href="#/sessions" data-nav="#/sessions" class="back-link">\u2190 Back to sessions</a>
      <a href="#/evidence" data-nav="#/evidence" class="back-link">\u2190 Timeline</a>`;
    $('#view').appendChild(nav);
    wireNavLinks(nav);

    const container = document.createElement('div');
    $('#view').appendChild(container);

    let firstLoad = true;
    const refresh = async () => {
      const filters = { ...currentFilters, 'session-id': sessionId };
      const data = await fetchEvidence(filters);
      renderDashboardInto(container, data, { preserveDetail: !firstLoad });
      firstLoad = false;
    };

    await refresh();
    configureLiveRefresh(refresh);
  } catch (err) {
    setError(`Failed to load session: ${err.message}`);
    disableLiveRefresh();
  }
}

function renderDashboardInto(container, data, { preserveDetail = false } = {}) {
  if (!container) return;
  const previousDetailId = preserveDetail ? openDetailId : null;
  if (!preserveDetail) {
    closeInlineDetail();
  } else {
    detailRowEl = null;
  }
  container.innerHTML = '';
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
    row.onclick = () => toggleInlineDetail(row, id);
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
  if (preserveDetail && previousDetailId) {
    reopenInlineDetail(container, previousDetailId);
  }
}

// -- Filters --

function initFilters() {
  filterEls.type = $('#filter-type');
  filterEls.author = $('#filter-author');
  const clearBtn = $('#filter-clear');

  if (filterEls.type) {
    filterEls.type.onchange = () => {
      const v = filterEls.type.value;
      if (v) currentFilters.type = v;
      else delete currentFilters.type;
      persistFiltersToHash();
    };
  }
  if (filterEls.author) {
    let debounce;
    filterEls.author.oninput = () => {
      clearTimeout(debounce);
      debounce = setTimeout(() => {
        const v = filterEls.author.value.trim();
        if (v) currentFilters.author = v;
        else delete currentFilters.author;
        persistFiltersToHash();
      }, 400);
    };
  }
  if (clearBtn) {
    clearBtn.onclick = () => {
      currentFilters = {};
      updateFilterControls();
      persistFiltersToHash();
    };
  }
}

function initLiveControls() {
  liveControls.container = $('#live-tail');
  liveControls.dot = $('#live-dot');
  liveControls.label = $('#live-label');
  liveControls.toggle = $('#live-toggle');

  if (liveControls.toggle) {
    liveControls.toggle.onclick = () => {
      livePaused = !livePaused;
      restartLiveRefreshTimer();
      if (!livePaused && typeof liveRefreshFn === 'function') {
        scrollToLatest();
        const result = liveRefreshFn();
        if (result && typeof result.catch === 'function') {
          result.catch(() => {});
        }
      }
    };
  }

  setLiveIndicatorVisible(false);
  updateLiveIndicatorUI();
}

// -- Navigation --

function initNav() {
  wireNavLinks(document);
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

function cssEscape(value) {
  if (window.CSS && typeof window.CSS.escape === 'function') {
    return window.CSS.escape(value);
  }
  return String(value).replace(/"/g, '\\"');
}

function scrollToLatest() {
  window.scrollTo({ top: 0, behavior: 'smooth' });
}

function initThemeToggle() {
  const saved = localStorage.getItem('evidence-theme');
  const initial = saved === 'light' ? 'light' : 'dark';
  document.documentElement.dataset.theme = initial;
  themeToggleEl = $('#theme-toggle');
  if (themeToggleEl) {
    themeToggleEl.textContent = initial === 'light' ? '☀️' : '◑';
    themeToggleEl.onclick = () => {
      const next = document.documentElement.dataset.theme === 'light' ? 'dark' : 'light';
      document.documentElement.dataset.theme = next;
      localStorage.setItem('evidence-theme', next);
      themeToggleEl.textContent = next === 'light' ? '☀️' : '◑';
    };
  }
}

// -- Init --

export function init() {
  initFilters();
  initThemeToggle();
  initLiveControls();
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
