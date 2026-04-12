import { test, expect } from "@playwright/test";

test("+ button instantly creates a node in the scratchpad", async ({
  page,
}) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Click + — should instantly create a node, open sidebar, show scratchpad
  await page.locator(".btn-new-node").click();

  // Sidebar should open with a scratchpad item
  await expect(page.locator(".sidebar-scratchpad")).toBeVisible({
    timeout: 5000,
  });
  await expect(page.locator(".scratchpad-item")).toBeVisible({ timeout: 5000 });

  // Scratchpad item should show (unnamed) and have Focus/Connect buttons
  await expect(page.locator(".scratchpad-name em")).toHaveText("(unnamed)");
  await expect(
    page.locator(".scratchpad-btn", { hasText: "Focus" })
  ).toBeVisible();
  await expect(
    page.locator(".scratchpad-btn", { hasText: "Connect" })
  ).toBeVisible();
});

test("scratchpad Focus button focuses the new node", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Create a scratch node
  await page.locator(".btn-new-node").click();
  await expect(page.locator(".scratchpad-item")).toBeVisible({ timeout: 5000 });

  // Click Focus
  await page.locator(".scratchpad-btn", { hasText: "Focus" }).click();

  // Should now show a focus card for the new node
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 5000 });
  // Type should be article
  await expect(page.locator(".focus-card .card-type")).toHaveText("article");
});

test("scratchpad Connect button enters connect mode", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // First, navigate to a node so we have a graph to connect to
  await page.locator('input[placeholder="Search by name..."]').fill("Abi");
  await page.locator('input[placeholder="Search by name..."]').press("Enter");
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  // Create a scratch node
  await page.locator(".btn-new-node").click();
  await expect(page.locator(".scratchpad-item")).toBeVisible({ timeout: 5000 });

  // Click Connect
  await page.locator(".scratchpad-btn", { hasText: "Connect" }).click();

  // Should show connect banner
  await expect(page.locator(".connect-banner")).toBeVisible({ timeout: 3000 });
  await expect(page.locator(".connect-banner")).toContainText(
    "Click a node in the graph"
  );

  // Click the focused Abi node in the SVG to complete the connection
  await page
    .locator("svg text", { hasText: /^Abi$/ })
    .last()
    .click({ force: true });

  // Connect banner should disappear
  await expect(page.locator(".connect-banner")).not.toBeVisible({
    timeout: 5000,
  });
});

test("+ Adjacent still opens creation dialog", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Search for Abi
  await page.locator('input[placeholder="Search by name..."]').fill("Abi");
  await page.locator('input[placeholder="Search by name..."]').press("Enter");
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  // Click + Adjacent
  await page.locator(".btn-new", { hasText: "+ Adjacent" }).click();
  await expect(page.locator(".creation-dialog")).toBeVisible({ timeout: 3000 });
  // Should have relation picker
  await expect(page.locator(".creation-field select")).toHaveCount(2);
});

test("hop depth controls adjust neighbourhood size", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  await expect(page.locator(".hop-label")).toHaveText("k=3");
  await page.locator(".hop-btn").first().click();
  await expect(page.locator(".hop-label")).toHaveText("k=2");
  await page.locator(".hop-btn").first().click();
  await expect(page.locator(".hop-label")).toHaveText("k=1");
  await page.locator(".hop-btn").first().click();
  await expect(page.locator(".hop-label")).toHaveText("k=1");
  await page.locator(".hop-btn").last().click();
  await expect(page.locator(".hop-label")).toHaveText("k=2");
});
