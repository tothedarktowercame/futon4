import { test, expect } from "@playwright/test";

test("+ Adjacent creates a new entity and shifts focus", async ({ page }) => {
  await page.goto("/index.html");

  // Login
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Open sidebar, pick a song
  await page.locator(".sidebar-toggle").click();
  await expect(
    page.locator(".sidebar-type-item", { hasText: "arxana/song" })
  ).toBeVisible({ timeout: 5000 });
  await page
    .locator(".sidebar-type-item", { hasText: "arxana/song" })
    .click();
  await expect(page.locator(".sidebar-entity-item").first()).toBeVisible({
    timeout: 5000,
  });
  await page.locator(".sidebar-entity-item").first().click();

  // Wait for focus card
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });
  const originalName = await page.locator(".focus-card .card-name").textContent();
  console.log(`Focused on: ${originalName}`);

  // Click + Adjacent
  await page.locator(".btn-new", { hasText: "+ Adjacent" }).click();

  // Focus should shift to "New nema"
  await expect(page.locator(".focus-card .card-name")).toHaveText("New nema", {
    timeout: 10000,
  });

  console.log("New adjacent nema created and focused");
  await page.screenshot({ path: "tests/adjacent.png", fullPage: true });
});
