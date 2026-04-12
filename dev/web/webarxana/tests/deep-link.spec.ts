import { test, expect } from "@playwright/test";

test("navigating updates the URL hash", async ({ page }) => {
  await page.goto("/index.html");

  // Login
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Browse a type
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

  // Click an entity
  await page.locator(".sidebar-entity-item").first().click();
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  // The URL hash should now contain type and focus
  const url = page.url();
  console.log(`URL after navigation: ${url}`);
  expect(url).toContain("#/type/");
  expect(url).toContain("/focus/");

  // Grab the hash for the restore test
  const hash = new URL(url).hash;
  console.log(`Hash: ${hash}`);

  // Remember what entity is focused
  const focusedName = await page.locator(".focus-card .card-name").textContent();
  console.log(`Focused entity: ${focusedName}`);
});

test("deep link restores view on page load", async ({ page }) => {
  // First, navigate to get a valid entity ID
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

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
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  const hash = new URL(page.url()).hash;
  const focusedName = await page
    .locator(".focus-card .card-name")
    .textContent();
  console.log(`Original: ${focusedName} at ${hash}`);

  // Now reload the page with the same hash
  await page.goto(`/index.html${hash}`);

  // Need to log in again (session cookie should persist)
  // The hash restore happens after auth check
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 15000 });

  const restoredName = await page
    .locator(".focus-card .card-name")
    .textContent();
  console.log(`Restored: ${restoredName}`);
  expect(restoredName).toBe(focusedName);
});
