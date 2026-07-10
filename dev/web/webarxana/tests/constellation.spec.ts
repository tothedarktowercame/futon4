import { test, expect } from "@playwright/test";

test.use({ viewport: { width: 1440, height: 900 } });

// Look-and-feel guard for the cross-EOI Interest Constellation:
//  - it must render CONNECTED (typed semantic-arc edges) -- regression guard for
//    the witness-layer mis-classification that filtered every arc out, and for
//    the force-layout NaN that wiped edges as the sim ticked;
//  - it must spread NATURALLY across the viewport, not shrink into a small box.
test("Interest Constellation: connected + naturally spread", async ({ page }) => {
  await page.goto("/wa#/diagram/Interest%20Constellation/expanded");

  // Login only if a wall appears (viewing is usually open).
  const pw = page.locator('input[type="password"]');
  if (await pw.isVisible().catch(() => false)) {
    await page.locator('input[type="text"]').fill("joe");
    await pw.fill("arxana");
    await page.locator("button", { hasText: "Sign in" }).click();
  }

  // Wait for the graph, then let pins load + the force layout settle.
  await expect(page.locator("g.graph-node").first()).toBeVisible({ timeout: 15000 });
  await page.waitForTimeout(4500);

  const s = await page.evaluate(() => {
    const nodes = [...document.querySelectorAll("g.graph-node")];
    const edges = document.querySelectorAll("svg line").length;
    let minX = 1e9, minY = 1e9, maxX = -1e9, maxY = -1e9;
    for (const n of nodes) {
      const r = (n as Element).getBoundingClientRect();
      minX = Math.min(minX, r.left); minY = Math.min(minY, r.top);
      maxX = Math.max(maxX, r.right); maxY = Math.max(maxY, r.bottom);
    }
    return {
      nodes: nodes.length,
      edges,
      spreadW: (maxX - minX) / window.innerWidth,
      spreadH: (maxY - minY) / window.innerHeight,
    };
  });
  console.log("constellation stats:", JSON.stringify(s));

  await page.screenshot({ path: "tests/constellation.png", fullPage: false });

  // Connectivity: nodes AND a substantial number of edges.
  expect(s.nodes).toBeGreaterThan(30);
  expect(s.edges).toBeGreaterThan(50);

  // Natural spread: nodes occupy most of the viewport, not a shrunk rectangle.
  expect(s.spreadW).toBeGreaterThan(0.6);
  expect(s.spreadH).toBeGreaterThan(0.5);

  // Zoom + pan must actually move the canvas transform (regression guard: the
  // auto-fit used to re-fit every render and revert wheel/drag).
  const tf = () => page.evaluate(
    () => document.querySelector(".graph-zoom-layer")?.getAttribute("transform") ?? "");
  const t0 = await tf();
  await page.mouse.move(700, 450);
  await page.waitForTimeout(150);
  await page.mouse.wheel(0, -400);            // zoom in
  await page.waitForTimeout(500);
  const t1 = await tf();
  expect(t1).not.toBe(t0);                     // wheel changed the transform

  await page.mouse.move(700, 450);
  await page.mouse.down();
  await page.mouse.move(500, 300, { steps: 5 });
  await page.mouse.up();
  await page.waitForTimeout(300);
  const t2 = await tf();
  expect(t2).not.toBe(t1);                     // drag panned
});
