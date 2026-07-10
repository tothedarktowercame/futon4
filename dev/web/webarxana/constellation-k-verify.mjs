// Verify the k=3 -> k=2 fix on the constellation focus view.
// At k=2 the gray neighbour ring + its edges should drop, but the core
// star-to-star edges must SURVIVE (was: total disconnect, 0 edges).
// Run: cd futon4/dev/web/webarxana && node constellation-k-verify.mjs
import { chromium } from 'playwright';

const BASE = 'http://localhost:3100/wa';
const URL = BASE + '#/dev/xdiagram/arxana%2Fdiagram%2Fconstellation/focus/'
  + 'arxana%2Finterest%2Fconstellation%2Fglasgow-cogito-cover-letter-final-distributed-extended-mind'
  + '/view/organic';

const browser = await chromium.launch();
const page = await (await browser.newContext()).newPage();
const errors = [];
page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
page.on('pageerror', e => errors.push(String(e)));

await page.goto(BASE);
await page.evaluate(async () => {
  await fetch('/api/auth/login', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ username: 'joe', password: 'arxana' }), credentials: 'include' });
});
await page.goto(URL);
await page.reload();
await page.waitForSelector('svg circle', { timeout: 20000 });
await page.waitForTimeout(3000);

const count = async () => ({
  k: await page.$eval('.hop-label', el => el.textContent),
  nodes: await page.$$eval('svg circle', els => els.length),
  edges: await page.$$eval('svg line', els => els.length),
});

const labels = async () =>
  (await page.$$eval('svg text', els => els.map(e => e.textContent.trim())))
    .filter(t => t && t.length > 1);

const k3 = await count();
await page.screenshot({ path: '/tmp/constellation-k3.png' });

// k=3 -> k=2
await page.$eval('.hop-controls .hop-btn', el => el.click());
await page.waitForTimeout(3000);
const k2 = await count();
await page.screenshot({ path: '/tmp/constellation-k2.png' });

// k=2 -> k=1 (super-core retraction)
await page.$eval('.hop-controls .hop-btn', el => el.click());
await page.waitForTimeout(3000);
const k1 = await count();
const k1labels = await labels();
await page.screenshot({ path: '/tmp/constellation-k1.png' });

console.log(JSON.stringify({ k3, k2, k1, k1labels, errors }, null, 2));
await browser.close();
