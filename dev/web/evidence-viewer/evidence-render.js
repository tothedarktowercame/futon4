// evidence-render.js — Type-specific rendering for evidence entries.
// Transplanted from arxana-lab.el and arxana-browser-lab.el.

// -- Accessor helpers for namespaced JSON keys --

export function eget(entry, key) {
  // Try namespaced first, then bare
  return entry[`evidence/${key}`] ?? entry[key];
}

function bget(body, key) {
  // Body keys may be namespaced or bare depending on source
  return body?.[key] ?? body?.[`evidence/${key}`];
}

// -- Type labels (arxana-lab--evidence-type-label, lines 691-707) --

const TYPE_LABELS = {
  'pattern-selection': 'PSR',
  'pattern-outcome': 'PUR',
  'reflection': 'PAR',
  'gate-traversal': 'GATE',
  'coordination': 'COORD',
  'forum-post': 'FORUM',
  'mode-transition': 'MODE',
  'presence-event': 'PRESENCE',
  'correction': 'CORR',
  'conjecture': 'CONJ',
};

export function typeLabel(etype) {
  if (!etype) return '?';
  const clean = String(etype).replace(/^:/, '');
  return TYPE_LABELS[clean] || clean.toUpperCase().slice(0, 8);
}

// -- Type CSS classes --

const TYPE_CLASSES = {
  'pattern-selection': 'type-psr',
  'pattern-outcome': 'type-pur',
  'reflection': 'type-par',
  'gate-traversal': 'type-gate',
  'coordination': 'type-coord',
  'forum-post': 'type-forum',
  'mode-transition': 'type-mode',
  'presence-event': 'type-presence',
  'correction': 'type-corr',
  'conjecture': 'type-conj',
};

export function typeClass(etype) {
  if (!etype) return 'type-default';
  const clean = String(etype).replace(/^:/, '');
  return TYPE_CLASSES[clean] || 'type-default';
}

// -- Claim type labels --

export function claimLabel(ctype) {
  if (!ctype) return '?';
  return String(ctype).replace(/^:/, '');
}

// -- Subject formatting --

export function formatSubject(subject) {
  if (!subject) return '';
  const rtype = subject['ref/type'] || subject.type || '';
  const rid = subject['ref/id'] || subject.id || '';
  const cleanType = String(rtype).replace(/^:/, '');
  const cleanId = String(rid);
  return cleanType ? `${cleanType}:${cleanId}` : cleanId;
}

// -- Smart preview (arxana-lab--evidence-body-preview, lines 772-813) --

export function bodyPreview(body, etype, maxLen = 40) {
  if (!body) return '';
  const clean = String(etype || '').replace(/^:/, '');

  let text = '';
  if (clean === 'pattern-selection') {
    const q = body.query || body['psr/query'] || '';
    const s = body.selected || body['psr/selected'] || '';
    text = s ? `${q} \u2192 ${s}` : q;
  } else if (clean === 'pattern-outcome') {
    const o = body.outcome || body['pur/outcome'] || '';
    const a = body.actions || body['pur/actions'] || '';
    text = a ? `${o}: ${a}` : o;
  } else if (clean === 'reflection') {
    const well = body.what_went_well || body['par/what-went-well'] || [];
    text = Array.isArray(well) && well.length > 0 ? well[0] : String(well || '');
  } else if (body.tool) {
    // Peripheral step
    const args = body.args ? `(${truncStr(JSON.stringify(body.args), 20)})` : '';
    text = `${body.tool}${args}`;
  } else if (body.fruit) {
    // Peripheral stop
    text = truncStr(JSON.stringify(body.fruit), maxLen);
  } else if (typeof body === 'string') {
    text = body;
  } else {
    // Generic: first key-value pair
    const keys = Object.keys(body);
    if (keys.length > 0) {
      const k = keys[0];
      const v = typeof body[k] === 'string' ? body[k] : JSON.stringify(body[k]);
      text = `${k}: ${truncStr(v || '', 30)}`;
    }
  }
  return truncStr(text, maxLen);
}

function truncStr(s, max) {
  if (!s) return '';
  s = String(s);
  return s.length > max ? s.slice(0, max - 1) + '\u2026' : s;
}

// -- Timestamp formatting --

export function formatTime(ts) {
  if (!ts) return '';
  // Show YYYY-MM-DD HH:MM:SS
  return String(ts).replace('T', ' ').slice(0, 19);
}

export function formatTimeShort(ts) {
  if (!ts) return '';
  // Show HH:MM:SS only
  const t = String(ts);
  const idx = t.indexOf('T');
  return idx >= 0 ? t.slice(idx + 1, idx + 9) : t.slice(0, 8);
}

// -- Tags formatting --

export function formatTags(tags) {
  if (!tags || !Array.isArray(tags)) return '';
  return tags.map(t => String(t).replace(/^:/, '')).join(', ');
}

// -- Full body rendering (returns HTML string) --

export function renderBody(body, etype) {
  if (!body) return '<p class="empty">No body</p>';
  const clean = String(etype || '').replace(/^:/, '');

  switch (clean) {
    case 'pattern-selection': return renderPSR(body);
    case 'pattern-outcome': return renderPUR(body);
    case 'reflection': return renderPAR(body);
    default: return renderGeneric(body);
  }
}

function renderPSR(body) {
  return `<dl class="body-fields">
    ${field('Query', body.query || body['psr/query'])}
    ${field('Selected', body.selected || body['psr/selected'])}
    ${field('Sigil', body.sigil || body['psr/sigil'])}
    ${field('Confidence', body.confidence || body['psr/confidence'])}
    ${field('Rationale', body.rationale || body['psr/rationale'])}
    ${listField('Candidates', body.candidates || body['psr/candidates'])}
  </dl>`;
}

function renderPUR(body) {
  return `<dl class="body-fields">
    ${field('Outcome', body.outcome || body['pur/outcome'])}
    ${field('Actions', body.actions || body['pur/actions'])}
    ${field('Expected', body.expected || body['pur/expected'])}
    ${field('Actual', body.actual || body['pur/actual'])}
    ${field('Prediction Error', body['prediction-error'] || body['pur/prediction-error'])}
    ${field('Notes', body.notes || body['pur/notes'])}
  </dl>`;
}

function renderPAR(body) {
  return `<dl class="body-fields">
    ${listField('Patterns Used', body['patterns-used'] || body['par/patterns-used'])}
    ${listField('What Went Well', body.what_went_well || body['par/what-went-well'])}
    ${listField('What Could Improve', body.what_could_improve || body['par/what-could-improve'])}
    ${listField('Prediction Errors', body.prediction_errors || body['par/prediction-errors'])}
    ${listField('Suggestions', body.suggestions || body['par/suggestions'])}
    ${listField('Commits', body.commits || body['par/commits'])}
    ${listField('Files Touched', body.files_touched || body['par/files-touched'])}
  </dl>`;
}

function renderGeneric(body) {
  if (typeof body === 'string') return `<pre class="body-raw">${esc(body)}</pre>`;
  const entries = Object.entries(body);
  if (entries.length === 0) return '<p class="empty">Empty body</p>';
  return `<dl class="body-fields">
    ${entries.map(([k, v]) => field(k, v)).join('\n')}
  </dl>`;
}

function field(label, value) {
  if (value == null || value === '') return '';
  const rendered = typeof value === 'object' ? `<pre>${esc(JSON.stringify(value, null, 2))}</pre>`
                 : `<span>${esc(String(value))}</span>`;
  return `<dt>${esc(String(label))}</dt><dd>${rendered}</dd>`;
}

function listField(label, items) {
  if (!items) return '';
  if (!Array.isArray(items)) return field(label, items);
  if (items.length === 0) return '';
  const lis = items.map(i => `<li>${esc(String(i))}</li>`).join('');
  return `<dt>${esc(String(label))}</dt><dd><ul>${lis}</ul></dd>`;
}

// -- Detail view (Summary + Links + Body) --

export function renderDetail(entry) {
  const type = eget(entry, 'type');
  const claimType = eget(entry, 'claim-type');
  const author = eget(entry, 'author');
  const at = eget(entry, 'at');
  const id = eget(entry, 'id');
  const subject = eget(entry, 'subject');
  const tags = eget(entry, 'tags');
  const body = eget(entry, 'body');
  const sessionId = eget(entry, 'session-id');
  const inReplyTo = eget(entry, 'in-reply-to');
  const forkOf = eget(entry, 'fork-of');
  const patternId = eget(entry, 'pattern-id');
  const isConjecture = eget(entry, 'conjecture?');
  const isEphemeral = eget(entry, 'ephemeral?');

  const tclass = typeClass(type);

  return `
    <div class="detail-view">
      <header class="detail-header">
        <span class="type-badge ${tclass}">${esc(typeLabel(type))}</span>
        <span class="claim-label">${esc(claimLabel(claimType))}</span>
        <span class="detail-author">by ${esc(author || '?')}</span>
        ${isConjecture ? '<span class="flag">conjecture</span>' : ''}
        ${isEphemeral ? '<span class="flag ephemeral">ephemeral</span>' : ''}
      </header>

      <section class="detail-summary">
        <h3>Summary</h3>
        <dl class="summary-fields">
          <dt>ID</dt><dd class="mono">${esc(id || '')}</dd>
          <dt>Type</dt><dd><span class="type-badge ${tclass}">${esc(typeLabel(type))}</span> ${esc(String(type || '').replace(/^:/, ''))}</dd>
          <dt>Claim</dt><dd>${esc(claimLabel(claimType))}</dd>
          <dt>Author</dt><dd>${esc(author || '')}</dd>
          <dt>Time</dt><dd>${esc(formatTime(at))}</dd>
          ${subject ? `<dt>Subject</dt><dd>${esc(formatSubject(subject))}</dd>` : ''}
          ${tags && tags.length ? `<dt>Tags</dt><dd>${esc(formatTags(tags))}</dd>` : ''}
        </dl>
      </section>

      <section class="detail-links">
        <h3>Links</h3>
        <dl class="summary-fields">
          ${sessionId ? `<dt>Session</dt><dd><a href="#/sessions/${encodeURIComponent(sessionId)}" class="evidence-link">${esc(sessionId)}</a></dd>` : ''}
          ${inReplyTo ? `<dt>In Reply To</dt><dd><a href="#/evidence/${encodeURIComponent(inReplyTo)}" class="evidence-link">${esc(inReplyTo)}</a></dd>` : ''}
          ${forkOf ? `<dt>Fork Of</dt><dd><a href="#/evidence/${encodeURIComponent(forkOf)}" class="evidence-link">${esc(forkOf)}</a></dd>` : ''}
          ${patternId ? `<dt>Pattern</dt><dd class="mono">${esc(String(patternId))}</dd>` : ''}
        </dl>
        ${inReplyTo ? `<a href="#/evidence/${encodeURIComponent(id)}/chain" class="chain-link">View full chain</a>` : ''}
      </section>

      <section class="detail-body">
        <h3>Body</h3>
        <div class="body-content ${tclass}-body">
          ${renderBody(body, type)}
        </div>
      </section>
    </div>
  `;
}

// -- Chain rendering --

export function renderChain(chain, currentId) {
  if (!chain || chain.length === 0) return '<p class="empty">Empty chain</p>';
  return `<div class="chain-view">
    ${chain.map((entry, i) => {
      const type = eget(entry, 'type');
      const id = eget(entry, 'id');
      const isCurrent = id === currentId;
      return `<div class="chain-entry ${isCurrent ? 'chain-current' : ''}" style="margin-left: ${Math.min(i, 6) * 16}px">
        <div class="chain-connector">${i > 0 ? '<span class="chain-arrow">\u2514\u2500</span>' : ''}</div>
        <div class="chain-card">
          <div class="chain-meta">
            <span class="type-badge ${typeClass(type)}">${esc(typeLabel(type))}</span>
            <span class="chain-author">${esc(eget(entry, 'author') || '?')}</span>
            <span class="chain-time">${esc(formatTimeShort(eget(entry, 'at')))}</span>
            <a href="#/evidence/${encodeURIComponent(id)}" class="chain-detail-link">view</a>
          </div>
          <div class="chain-preview">${esc(bodyPreview(eget(entry, 'body'), type))}</div>
        </div>
      </div>`;
    }).join('\n')}
  </div>`;
}

// -- Thread rendering --

const TRANSPORT_LABELS = {
  'emacs-codex-repl': 'Codex REPL',
  'emacs-chat': 'Claude Chat',
  'irc': 'IRC',
};

export function transportLabel(transport) {
  if (!transport) return 'System';
  return TRANSPORT_LABELS[transport] || transport;
}

export function transportClass(transport) {
  if (!transport) return 'transport-system';
  if (transport.includes('codex')) return 'transport-codex';
  if (transport.includes('chat') || transport.includes('claude')) return 'transport-claude';
  if (transport.includes('irc')) return 'transport-irc';
  return 'transport-system';
}

export function relativeTime(isoString) {
  if (!isoString) return '';
  const date = new Date(isoString);
  const now = new Date();
  const diffMs = now - date;
  const diffMin = Math.floor(diffMs / 60000);
  const diffHr = Math.floor(diffMin / 60);
  const diffDay = Math.floor(diffHr / 24);

  if (diffMin < 1) return 'just now';
  if (diffMin < 60) return `${diffMin} min ago`;
  if (diffHr < 24) return `${diffHr} hr ago`;
  if (diffDay < 7) return `${diffDay}d ago`;
  return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
}

export function renderThreadCard(thread) {
  const label = transportLabel(thread.transport);
  const tclass = transportClass(thread.transport);
  const participants = thread.participants.join(', ') || 'unknown';
  const turnLabel = thread.turnCount > 0
    ? `${thread.turnCount} turns`
    : `${thread.entryCount} entries`;
  const time = relativeTime(thread.lastAt);
  const preview = thread.lastMessage ? truncStr(thread.lastMessage, 120) : '';

  return `
    <div class="thread-card-header">
      <span class="transport-badge ${tclass}">${esc(label)}</span>
      <span class="thread-participants">${esc(participants)}</span>
    </div>
    <div class="thread-card-summary">${esc(turnLabel)} \u00b7 ${esc(time)}</div>
    ${preview ? `<div class="thread-card-preview">${esc(preview)}</div>` : ''}
  `;
}

// -- Notebook rendering --

function isChatMessage(body) {
  return body?.text && typeof body.text === 'string';
}

function authorClass(author) {
  const name = (author || '').toLowerCase();
  if (name === 'joe') return 'author-joe';
  if (name === 'codex' || name.startsWith('codex-')) return 'author-codex';
  if (name === 'claude' || name.startsWith('claude-')) return 'author-claude';
  return 'author-default';
}

export function renderNotebook(entries) {
  if (!entries || entries.length === 0) {
    return '<div class="empty-state"><p>No entries in this session.</p></div>';
  }

  // Sort oldest-first for reading order
  const sorted = [...entries].sort((a, b) => {
    const atA = eget(a, 'at') || '';
    const atB = eget(b, 'at') || '';
    return atA.localeCompare(atB);
  });

  // Build thread header
  const participants = new Set();
  let transport = null;
  for (const e of sorted) {
    const author = eget(e, 'author');
    if (author) participants.add(author);
    const body = eget(e, 'body');
    if (!transport && body?.transport) transport = body.transport;
  }

  const firstAt = eget(sorted[0], 'at');
  const lastAt = eget(sorted[sorted.length - 1], 'at');
  const tLabel = transportLabel(transport);
  const tClass = transportClass(transport);

  let html = '<div class="notebook">';
  html += `<div class="notebook-header">
    <span class="transport-badge ${tClass}">${esc(tLabel)}</span>
    <span class="notebook-participants">${esc([...participants].join(', '))}</span>
    <span class="notebook-timerange">${esc(formatTime(firstAt))} \u2014 ${esc(formatTime(lastAt))}</span>
  </div>`;

  for (const entry of sorted) {
    const body = eget(entry, 'body');
    const isChatTurn = isChatMessage(body);

    if (isChatTurn) {
      const role = body.role || 'unknown';
      const author = eget(entry, 'author') || role;
      const text = body.text || '';
      const at = eget(entry, 'at');
      const aclass = authorClass(author);

      html += `<div class="chat-message ${aclass}">
        <div class="message-meta">
          <span class="message-author ${aclass}">${esc(author)}</span>
          <span class="message-time">${esc(formatTimeShort(at))}</span>
        </div>
        <div class="message-text">${formatMessageText(text)}</div>
      </div>`;
    } else {
      // System event — compact inline marker
      const type = eget(entry, 'type');
      const tclass = typeClass(type);
      const at = eget(entry, 'at');
      const preview = bodyPreview(body, type, 60);
      const id = eget(entry, 'id');

      html += `<div class="system-event" data-id="${esc(id || '')}">
        <span class="system-event-time">${esc(formatTimeShort(at))}</span>
        <span class="type-badge ${tclass}">${esc(typeLabel(type))}</span>
        <span class="system-event-preview">${esc(preview)}</span>
      </div>`;
    }
  }

  html += '</div>';
  return html;
}

export function renderWall(entries) {
  if (!entries || entries.length === 0) {
    return '<div class="empty-state"><p>No entries yet.</p></div>';
  }

  // Sort oldest-first for reading order
  const sorted = [...entries].sort((a, b) => {
    const atA = eget(a, 'at') || '';
    const atB = eget(b, 'at') || '';
    return atA.localeCompare(atB);
  });

  let html = '<div class="notebook wall">';

  for (const entry of sorted) {
    const body = eget(entry, 'body');
    const isChatTurn = isChatMessage(body);

    if (isChatTurn) {
      const author = eget(entry, 'author') || body.role || 'unknown';
      const text = body.text || '';
      const at = eget(entry, 'at');
      const aclass = authorClass(author);
      const transport = body.transport;
      const tLabel = transportLabel(transport);
      const tClass = transportClass(transport);

      html += `<div class="chat-message ${aclass}">
        <div class="message-meta">
          <span class="message-author ${aclass}">${esc(author)}</span>
          <span class="transport-badge ${tClass} transport-badge-sm">${esc(tLabel)}</span>
          <span class="message-time">${esc(formatTimeShort(at))}</span>
        </div>
        <div class="message-text">${formatMessageText(text)}</div>
      </div>`;
    } else {
      const type = eget(entry, 'type');
      const tclass = typeClass(type);
      const at = eget(entry, 'at');
      const preview = bodyPreview(body, type, 60);
      const id = eget(entry, 'id');

      html += `<div class="system-event" data-id="${esc(id || '')}">
        <span class="system-event-time">${esc(formatTimeShort(at))}</span>
        <span class="type-badge ${tclass}">${esc(typeLabel(type))}</span>
        <span class="system-event-preview">${esc(preview)}</span>
      </div>`;
    }
  }

  html += '</div>';
  return html;
}

function formatMessageText(text) {
  if (!text) return '';
  let html = esc(text);
  // Code blocks: ```...``` → <pre>
  html = html.replace(/```([^`]*?)```/gs, '<pre class="message-code">$1</pre>');
  // Inline code: `...` → <code>
  html = html.replace(/`([^`]+?)`/g, '<code>$1</code>');
  // Line breaks
  html = html.replace(/\n/g, '<br>');
  return html;
}

// -- HTML escaping --

function esc(s) {
  if (!s) return '';
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}
