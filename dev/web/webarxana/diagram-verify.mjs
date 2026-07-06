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

const COGITO_SECTION_IDS = [
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/1-right-view',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/2-right-intention',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/3-right-speech',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/4-right-mindfulness',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/5-right-livelihood',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/6-tension-free-solo',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/7-tension-held-space',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/8-tension-foreclosure',
  'arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1/section/9-tension-indicators',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p03-open-research-ethics',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p04-phd-extended-mind',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p05-niven-methodology',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p06-codesign-oneill',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p07-background-leadership',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p08-design-patterns',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p09-routine-role-debruine',
  'arxana/essay/glasgow-cogito-cover-letter-final/section/p10-publications',
];

const COGITO_STAR_IDS = COGITO_LABELS.map((_, i) => [
  'arxana/interest/cogito/epistemology-ethics-practice',
  'arxana/interest/cogito/agentic-loop-eats-tail',
  'arxana/interest/cogito/primary-research-record',
  'arxana/interest/cogito/distributed-extended-mind',
  'arxana/interest/cogito/knowledge-dialogue-codesign',
  'arxana/interest/cogito/design-patterns-guidance',
  'arxana/interest/cogito/formal-reasoning',
  'arxana/interest/cogito/research-entrepreneur-missing-customer',
  'arxana/interest/cogito/bridge-analytic-epistemology',
  'arxana/interest/cogito/tension-free-solo-funding',
][i]);

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
  { name: 'COGITO Interests', frag: '#/diagram/COGITO%20Interests/compressed', mode: 'compressed', min: 9, max: 14, noSectionLabels: true },
  { name: 'COGITO Interests', frag: '#/diagram/COGITO%20Interests/expanded', mode: 'expanded', min: 20, max: 100, expectedLabels: COGITO_LABELS, hiddenDiagramLabel: 'COGITO Interests', sectionWitnesses: true, fixedScreenshot: '/tmp/cogito-evidence-fixed.png' },
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
  const graphAssertions = await page.evaluate(({ sectionIds, starIds, checkWitnesses }) => {
    const c = window.cljs?.core;
    const st = window.webarxana?.client?.state;
    if (!c || !st) return { available: false };
    const pins = c.get(c.deref(st.ui_state), c.keyword('pins'));
    const hood = c.clj__GT_js(st.multi_neighbourhood(pins));
    const nemas = hood.nemas || [];
    const links = hood.links || [];
    const ids = new Set(nemas.map(n => n.id));
    const sectionSet = new Set(sectionIds);
    const starSet = new Set(starIds);
    const sectionNames = nemas.filter(n => sectionSet.has(n.id)).map(n => n.name);
    const missingSections = sectionIds.filter(id => !ids.has(id));
    const witnessLinks = links.filter(l => l.type === 'interest/witnessed-in'
      && starSet.has(l.src?.id) && sectionSet.has(l.dst?.id));
    const witnessedSections = new Set(witnessLinks.map(l => l.dst.id));
    const orphanSections = checkWitnesses ? sectionIds.filter(id => !witnessedSections.has(id)) : [];
    return { available: true, missingSections, orphanSections, witnessCount: witnessLinks.length, sectionNames };
  }, { sectionIds: COGITO_SECTION_IDS, starIds: COGITO_STAR_IDS, checkWitnesses: !!target.sectionWitnesses });
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
  const visibleSectionLabels = allLabels.filter(label => label.startsWith('§') || label.startsWith('¶'));
  const unexpectedSectionLabels = target.noSectionLabels ? visibleSectionLabels : [];
  const sectionShortLabels = (graphAssertions.sectionNames || []).map(shortLabel);
  const missingRenderedSections = target.sectionWitnesses
    ? sectionShortLabels.filter(label => !allLabels.includes(label))
    : [];
  const sectionOk = (!target.noSectionLabels || unexpectedSectionLabels.length === 0) &&
    (!target.sectionWitnesses ||
      (graphAssertions.available &&
       graphAssertions.missingSections.length === 0 &&
       graphAssertions.orphanSections.length === 0 &&
       missingRenderedSections.length === 0));
  const labelOk = (!target.expectedLabels ||
    (missingExpectedLabels.length === 0 && prefixedConstellationLabels.length === 0)) &&
    visibleHiddenDiagramLabels.length === 0 && visibleContainerEdges.length === 0 && sectionOk;

  const bodyText = await page.evaluate(() =>
    document.body.innerText.split('\n').map(s => s.trim()).filter(Boolean).slice(0, 16));
  const screenshot = target.fixedScreenshot || `/tmp/diagram-${target.name.toLowerCase().replaceAll(' ', '-')}-${target.mode}.png`;
  await page.screenshot({ path: screenshot });

  const ok = login === 200 && errors.length === 0 && labelOk && circles >= target.min && circles <= target.max;
  if (!ok) failed = true;
  out.runs.push({ ...target, login, finalUrl: page.url(), circles, lines, errors, ok, labels, expectedFullLabels: target.expectedLabels || [], missingExpectedLabels, prefixedConstellationLabels, visibleHiddenDiagramLabels, visibleContainerEdges, visibleSectionLabels, unexpectedSectionLabels, missingRenderedSections, graphAssertions, bodyText, screenshot });
  await ctx.close();
}

console.log(JSON.stringify(out, null, 2));
await browser.close();
if (failed) process.exit(1);
