// Verify reduced interest diagrams in compressed and expanded modes.
// Run: cd futon4/dev/web/webarxana && node diagram-verify.mjs
import { chromium } from 'playwright';

const BASE = 'http://localhost:3100/wa';
const TARGETS = [
  { name: 'COGITO Interests', frag: '#/diagram/COGITO%20Interests/compressed', mode: 'compressed', min: 9, max: 14 },
  { name: 'COGITO Interests', frag: '#/diagram/COGITO%20Interests/expanded', mode: 'expanded', min: 20, max: 80 },
  { name: 'Interest Constellation', frag: '#/diagram/Interest%20Constellation/compressed', mode: 'compressed', min: 11, max: 16 },
  { name: 'Interest Constellation', frag: '#/diagram/Interest%20Constellation/expanded', mode: 'expanded', min: 20, max: 60 },
];

const browser = await chromium.launch();

const out = { runs: [] };
let failed = false;
for (const target of TARGETS) {
  const ctx = await browser.newContext();
  const page = await ctx.newPage();
  const errors = [];
  page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
  page.on('pageerror', e => errors.push(String(e)));

  const loginResponse = await ctx.request.post('http://localhost:3100/api/auth/login', {
    data: { username: 'joe', password: 'arxana' }
  });
  const login = loginResponse.status();

  await page.goto(BASE + target.frag);
  await page.reload();
  await page.waitForTimeout(4500);

  const circles = await page.$$eval('svg circle', els => els.length).catch(() => -1);
  const lines = await page.$$eval('svg line', els => els.length).catch(() => -1);
  const labels = await page.$$eval('svg text', els => els.map(e => e.textContent).filter(Boolean).slice(0, 40)).catch(() => []);
  const bodyText = await page.evaluate(() =>
    document.body.innerText.split('\n').map(s => s.trim()).filter(Boolean).slice(0, 16));
  const screenshot = `/tmp/diagram-${target.name.toLowerCase().replaceAll(' ', '-')}-${target.mode}.png`;
  await page.screenshot({ path: screenshot });

  const ok = login === 200 && errors.length === 0 && circles >= target.min && circles <= target.max;
  if (!ok) failed = true;
  out.runs.push({ ...target, login, finalUrl: page.url(), circles, lines, errors, ok, labels, bodyText, screenshot });
  await ctx.close();
}

console.log(JSON.stringify(out, null, 2));
await browser.close();
if (failed) process.exit(1);
