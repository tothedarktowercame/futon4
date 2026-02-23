// evidence-api.js — HTTP client for the futon1a evidence API.
// Same-origin relative URLs (served alongside the API from futon1a).

const API_BASE = '/api/alpha/evidence';

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
  if (!qs.has('limit')) qs.set('limit', '200');
  const url = qs.toString() ? `${API_BASE}?${qs}` : `${API_BASE}?limit=200`;
  const resp = await fetch(url, {
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
