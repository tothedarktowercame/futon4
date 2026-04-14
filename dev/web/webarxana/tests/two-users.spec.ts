import { test, expect, chromium } from "@playwright/test";

test("two browsers see each other's changes via WebSocket", async () => {
  const browser = await chromium.launch();

  // Create two independent browser contexts (like two users)
  const contextA = await browser.newContext();
  const contextB = await browser.newContext();
  const pageA = await contextA.newPage();
  const pageB = await contextB.newPage();

  // Login both
  for (const page of [pageA, pageB]) {
    await page.goto("http://localhost:3100/index.html");
    await page.locator('input[type="text"]').fill("joe");
    await page.locator('input[type="password"]').fill("arxana");
    await page.locator("button", { hasText: "Sign in" }).click();
    await expect(
      page.locator('input[placeholder="Search by name..."]')
    ).toBeVisible({ timeout: 5000 });
  }

  // Both should show "live" (WebSocket connected)
  await expect(pageA.locator(".status-indicator")).toHaveText("live", {
    timeout: 5000,
  });
  await expect(pageB.locator(".status-indicator")).toHaveText("live", {
    timeout: 5000,
  });

  // User A searches for Nets
  await pageA.locator('input[placeholder="Search by name..."]').fill("Nets");
  await pageA
    .locator('input[placeholder="Search by name..."]')
    .press("Enter");
  await expect(pageA.locator(".focus-card").first()).toBeVisible({
    timeout: 10000,
  });

  // User A creates a new node via scratch card
  await pageA.locator(".btn-new-node").click();
  await expect(pageA.locator(".scratch-card")).toBeVisible({ timeout: 5000 });
  await pageA
    .locator('.scratch-card input[placeholder="Name..."]')
    .fill("Collab test");
  await pageA
    .locator(".scratch-card textarea")
    .fill("Created by user A");
  await pageA.locator(".scratch-card .btn-save").click();

  // Wait for the save to propagate via WebSocket
  await pageA.waitForTimeout(2000);

  // User B searches for the newly created entity
  await pageB
    .locator('input[placeholder="Search by name..."]')
    .fill("Collab test");
  await pageB
    .locator('input[placeholder="Search by name..."]')
    .press("Enter");

  // User B should find it
  await expect(pageB.locator(".focus-card").first()).toBeVisible({
    timeout: 10000,
  });

  // Check that the content is there
  const cardText = await pageB
    .locator(".focus-card.active-pin .card-body")
    .first()
    .textContent();
  console.log(`User B sees: ${cardText}`);

  // The entity was persisted to futon1a, so B can find it via search
  // (The WS sync updates B's local Datascript, but the search goes via ego)
  expect(cardText).toContain("Created by user A");

  await pageA.screenshot({ path: "tests/collab-a.png", fullPage: true });
  await pageB.screenshot({ path: "tests/collab-b.png", fullPage: true });

  await contextA.close();
  await contextB.close();
  await browser.close();
});
