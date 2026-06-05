// Playwright verification for the interest-network surface (M-INC step d).
// WebArxana is served at /wa (distinct from the War Machine at /). Logs in,
// routes to #/interest-network, asserts the graph rendered, screenshots.
// Run: cd futon4/dev/web/webarxana && node interest-network-verify.mjs
import { chromium } from 'playwright';

const BASE = 'http://localhost:3100/wa';
const browser = await chromium.launch();
const page = await (await browser.newContext()).newPage();
const errors = [];
page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
page.on('pageerror', e => errors.push(String(e)));

await page.goto(BASE);
const login = await page.evaluate(async () => {
  const r = await fetch('/api/auth/login', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ username: 'joe', password: 'arxana' }), credentials: 'include' });
  return r.status;
});
// route to the interest-network view and re-init authed
await page.goto(BASE + '#/interest-network');
await page.reload();
await page.waitForSelector('svg circle', { timeout: 20000 });
await page.waitForTimeout(2500); // force layout + fetch settle

const circles = await page.$$eval('svg circle', els => els.length);
const lines = await page.$$eval('svg line', els => els.length);
const headerText = await page.evaluate(() => document.body.innerText.split('\n').filter(Boolean).slice(0, 6));
await page.screenshot({ path: '/tmp/interest-network.png', fullPage: false });
console.log(JSON.stringify({ login, circles, lines, headerText, errors }, null, 2));
await browser.close();
