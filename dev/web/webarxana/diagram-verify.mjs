// Verify reduced interest diagrams in compressed and expanded modes.
// Run: cd futon4/dev/web/webarxana && node diagram-verify.mjs
import { chromium } from 'playwright';

const BASE = 'http://localhost:3100/wa';
const COGITO_LABELS = [
  'Epistemology ⋈ ethics in practice',
  'Agentic loop "eats its own tail"',
  'Primary research record',
  'Distributed / extended mind',
  'Knowledge via dialogue & co-design',
  'Design patterns as guidance',
  'Formal reasoning',
  'Research-entrepreneur / missing customer',
  'Bridge → analytic epistemology',
  'Tension: free-solo vs funding-pursuit',
];

const CONSTELLATION_LABELS = [
  'Epistemology ⋈ ethics in practice',
  'Agentic loop / closing the loop',
  'Agentic coding as primary research record',
  'Distributed / extended mind',
  'Knowledge via dialogue & co-design',
  'Design patterns / Alexander patterns as guidance',
  'Formal reasoning / proof / type theory',
  'Research-entrepreneur / missing customer',
  'Eightfold-composite backbone',
  'Free-solo vs funding-pursuit tension',
  'Constellational thinking / VSATARCS',
  'Anthropic Institute / Economics & Policy fit',
];

const shortLabel = s => s.length > 16 ? `${s.slice(0, 14)}...` : s;

const TARGETS = [
  { name: 'COGITO Interests', frag: '#/diagram/COGITO%20Interests/compressed', mode: 'compressed', min: 9, max: 14 },
  { name: 'COGITO Interests', frag: '#/diagram/COGITO%20Interests/expanded', mode: 'expanded', min: 20, max: 80, expectedLabels: COGITO_LABELS, hiddenDiagramLabel: 'COGITO Interests', fixedScreenshot: '/tmp/cogito-fixed.png' },
  { name: 'Interest Constellation', frag: '#/diagram/Interest%20Constellation/compressed', mode: 'compressed', min: 11, max: 16, expectedLabels: CONSTELLATION_LABELS },
  { name: 'Interest Constellation', frag: '#/diagram/Interest%20Constellation/expanded', mode: 'expanded', min: 20, max: 80, expectedLabels: CONSTELLATION_LABELS, hiddenDiagramLabel: 'Interest Constellation', fixedScreenshot: '/tmp/constellation-fixed.png' },
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
  const allLabels = await page.$$eval('svg text', els => els.map(e => e.textContent).filter(Boolean)).catch(() => []);
  const labels = allLabels.slice(0, 40);
  const expectedShortLabels = (target.expectedLabels || []).map(shortLabel);
  const missingExpectedLabels = expectedShortLabels.filter(label => !allLabels.includes(label));
  const prefixedConstellationLabels = allLabels.filter(label => label.startsWith('Constellation:'));
  const visibleHiddenDiagramLabels = target.hiddenDiagramLabel
    ? allLabels.filter(label => label === target.hiddenDiagramLabel || label === shortLabel(target.hiddenDiagramLabel))
    : [];
  const visibleContainerEdges = target.hiddenDiagramLabel
    ? allLabels.filter(label => label === 'diagram/core' || label === 'diagram/includes')
    : [];
  const labelOk = (!target.expectedLabels ||
    (missingExpectedLabels.length === 0 && prefixedConstellationLabels.length === 0)) &&
    visibleHiddenDiagramLabels.length === 0 && visibleContainerEdges.length === 0;

  const bodyText = await page.evaluate(() =>
    document.body.innerText.split('\n').map(s => s.trim()).filter(Boolean).slice(0, 16));
  const screenshot = target.fixedScreenshot || `/tmp/diagram-${target.name.toLowerCase().replaceAll(' ', '-')}-${target.mode}.png`;
  await page.screenshot({ path: screenshot });

  const ok = login === 200 && errors.length === 0 && labelOk && circles >= target.min && circles <= target.max;
  if (!ok) failed = true;
  out.runs.push({ ...target, login, finalUrl: page.url(), circles, lines, errors, ok, labels, expectedFullLabels: target.expectedLabels || [], missingExpectedLabels, prefixedConstellationLabels, visibleHiddenDiagramLabels, visibleContainerEdges, bodyText, screenshot });
  await ctx.close();
}

console.log(JSON.stringify(out, null, 2));
await browser.close();
if (failed) process.exit(1);
