'use strict';
// Extension-host only. The sole operation this client can send is read-only.
const fs = require('fs');
const http = require('http');
const { randomUUID } = require('crypto');
function endpoint(configured, read = fs.readFileSync, uid = process.getuid()) {
  let value = configured;
  if (!value) {
    const ready = read(`/run/user/${uid}/futon4a-catalogue-${uid}/ready.edn`, 'utf8');
    const match = ready.match(/:port\s+(\d+)/);
    if (!match) throw new Error('Catalogue ready file has no port');
    value = `http://127.0.0.1:${match[1]}/arxana/v1`;
  }
  const url = new URL(value);
  if (url.protocol !== 'http:' || !['127.0.0.1', 'localhost', '[::1]'].includes(url.hostname)
      || url.username || url.password || url.search || url.hash)
    throw new Error('Catalogue endpoint must be loopback HTTP without credentials or query');
  if (url.pathname === '/') url.pathname = '/arxana/v1';
  if (url.pathname !== '/arxana/v1') throw new Error('Catalogue endpoint must use /arxana/v1');
  return url;
}
function unwrap(value) {
  if (Array.isArray(value)) return value.map(unwrap);
  if (value && typeof value === 'object') {
    if (Object.keys(value).length === 1 && typeof value.$string === 'string') return value.$string;
    return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, unwrap(v)]));
  }
  return value;
}
function read(url, benefitId, at = new Date().toISOString()) {
  const body = { 'valid-time': at };
  if (benefitId) body['benefit-id'] = benefitId;
  const data = JSON.stringify({ 'protocol/version': 'arxana.v1', 'message/id': `mojira/benefits/${randomUUID()}`,
    'principal/id': 'reader', operation: ':cascade/benefit-filings', body });
  return new Promise((resolve, reject) => {
    const req = http.request(url, { method: 'POST', headers: { 'Content-Type': 'application/json', 'Content-Length': Buffer.byteLength(data) } }, res => {
      let text = '';
      res.setEncoding('utf8');
      res.on('data', chunk => { text += chunk; });
      res.on('error', reject);
      res.on('end', () => {
        try {
          if (res.statusCode !== 200) throw new Error(`Catalogue HTTP ${res.statusCode}`);
          const reply = unwrap(JSON.parse(text));
          if (reply.error) resolve({ status: ':refused', error: reply.error });
          else if (!reply.ok) throw new Error('Catalogue response has neither ok nor error');
          else resolve(reply.ok);
        } catch (e) { reject(e); }
      });
    });
    req.setTimeout(180000, () => req.destroy(new Error('Catalogue read timed out')));
    req.on('error', reject);
    req.end(data);
  });
}
module.exports = { endpoint, read, unwrap };
