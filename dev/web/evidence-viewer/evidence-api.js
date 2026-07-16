// evidence-api.js — HTTP client for the futon1a evidence API.
// Same-origin relative URLs (served alongside the API from futon1a).

// Behind a fronting proxy (e.g. https://evidence.paragogy.net) the API is
// same-origin at /api/...; when served directly from the store's static
// port (7071) it lives on the neighbouring port 7070.
const API_BASE = (window.location.port === '7071')
  ? `${window.location.protocol}//${window.location.hostname}:7070/api/alpha/evidence`
  : `${window.location.origin}/api/alpha/evidence`;
export const DEFAULT_EVIDENCE_LIMIT = 50;

/**
 * Fetch evidence entries with optional filters.
 * @param {Object} params - Query parameters: type, claim-type, session-id, author, since, limit
 * @returns {Promise<{entries: Array, count: number}>}
 */
export async function fetchEvidence(params = {}) {
  const qs = new URLSearchParams();
  for (const [k, v] of Object.entries(params)) {
    if (v != null && v !== '') qs.set(k, String(v));
  }
  if (!qs.has('limit')) qs.set('limit', String(DEFAULT_EVIDENCE_LIMIT));
  const url = qs.toString() ? `${API_BASE}?${qs}` : `${API_BASE}?limit=${DEFAULT_EVIDENCE_LIMIT}`;
  const resp = await fetch(url, {
    cache: 'no-store',
    headers: { 'Accept': 'application/json' }
  });
  if (!resp.ok) throw new Error(`Evidence API ${resp.status}: ${resp.statusText}`);
  return resp.json();
}

/**
 * Fetch a single evidence entry by ID.
 * @param {string} id - Evidence entry ID
 * @returns {Promise<Object>} The evidence entry
 */
export async function fetchEntry(id) {
  const resp = await fetch(`${API_BASE}/${encodeURIComponent(id)}`, {
    headers: { 'Accept': 'application/json' }
  });
  if (!resp.ok) throw new Error(`Evidence entry ${resp.status}: ${resp.statusText}`);
  return resp.json();
}

/**
 * Fetch the reply chain for an evidence entry (oldest-first).
 * @param {string} id - Evidence entry ID
 * @returns {Promise<{chain: Array}>}
 */
export async function fetchChain(id) {
  const resp = await fetch(`${API_BASE}/${encodeURIComponent(id)}/chain`, {
    headers: { 'Accept': 'application/json' }
  });
  if (!resp.ok) throw new Error(`Evidence chain ${resp.status}: ${resp.statusText}`);
  return resp.json();
}
