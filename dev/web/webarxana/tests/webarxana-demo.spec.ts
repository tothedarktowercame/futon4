import { test, expect } from "@playwright/test";

test("login and search renders the arxana ego graph", async ({ page }) => {
  await page.goto("/index.html");

  // Should see the login form
  await expect(page.locator("h2")).toHaveText("WebArxana");

  // Fill in credentials and sign in
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();

  // After login, the search bar should appear
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // We should see the "No nema in focus" empty state
  await expect(page.locator(".empty-graph")).toContainText("No nema in focus");

  // Search for "arxana"
  const searchInput = page.locator(
    'input[placeholder="Search by name..."]'
  );
  await searchInput.fill("arxana");
  await searchInput.press("Enter");

  // Wait for the SVG graph to render with neighbour nodes
  const svg = page.locator("svg");
  await expect(svg).toBeVisible({ timeout: 10000 });

  // Wait for neighbour nodes to appear (arxana + 4 neighbours = at least 5 circles)
  await expect(svg.locator("circle")).not.toHaveCount(0, { timeout: 10000 });
  // The focus node "arxana" should be rendered in the SVG
  await expect(svg.locator("text", { hasText: "arxana" })).toBeVisible({
    timeout: 5000,
  });

  // Wait for neighbour nodes to render (may take a moment after the async go block)
  await expect(svg.locator("text", { hasText: "nema" })).toBeVisible({
    timeout: 5000,
  });

  // We should have at least 5 circles (focus glow+fill = 2, plus 4 neighbours = 6)
  const count = await svg.locator("circle").count();
  expect(count).toBeGreaterThanOrEqual(5);

  // Check that relation labels are rendered
  await expect(
    svg.locator("text", { hasText: "defines" }).first()
  ).toBeVisible({ timeout: 5000 });
  await expect(
    svg.locator("text", { hasText: "implemented-by" }).first()
  ).toBeVisible();
  await expect(
    svg.locator("text", { hasText: "inspired-by" }).first()
  ).toBeVisible();

  // Take a screenshot for visual verification
  await page.screenshot({ path: "tests/webarxana-demo.png", fullPage: true });
});

test("clicking a neighbour node shifts focus", async ({ page }) => {
  await page.goto("/index.html");

  // Login
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Search for arxana
  const searchInput = page.locator(
    'input[placeholder="Search by name..."]'
  );
  await searchInput.fill("arxana");
  await searchInput.press("Enter");

  // Wait for the graph to fully render with neighbours
  await expect(
    page.locator("svg text", { hasText: "nema" }).first()
  ).toBeVisible({ timeout: 10000 });

  // The focus card should show arxana details
  await expect(page.locator(".focus-card.active-pin .card-name")).toHaveText("arxana");

  // Click the "nema" node label in the SVG — use the bold name label (font-weight bold)
  // There are two text elements per node (type label + name), we want the name.
  // The node's <g> has an on-click handler.
  const nemaNodeText = page
    .locator("svg text", { hasText: /^nema$/ })
    .last();
  await nemaNodeText.click({ force: true });

  // Focus card should now show "nema"
  await expect(page.locator(".focus-card.active-pin .card-name")).toHaveText("nema", {
    timeout: 5000,
  });

  await page.screenshot({
    path: "tests/webarxana-focus-shift.png",
    fullPage: true,
  });
});
