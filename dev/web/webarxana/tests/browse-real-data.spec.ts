import { test, expect } from "@playwright/test";

test("browse real data via type sidebar", async ({ page }) => {
  await page.goto("/index.html");

  // Login
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Open sidebar (hamburger button)
  await page.locator(".sidebar-toggle").click();

  // Wait for types to load
  await expect(page.locator(".sidebar-types")).toBeVisible({ timeout: 5000 });
  await expect(
    page.locator(".sidebar-type-item", { hasText: "arxana/song" })
  ).toBeVisible({ timeout: 5000 });

  // Click on arxana/song type
  await page
    .locator(".sidebar-type-item", { hasText: "arxana/song" })
    .click();

  // Wait for entities to load in sidebar
  await expect(page.locator(".sidebar-entities")).toBeVisible({
    timeout: 5000,
  });
  await expect(page.locator(".sidebar-entity-item").first()).toBeVisible({
    timeout: 5000,
  });

  // Should show real song names
  const entityCount = await page.locator(".sidebar-entity-item").count();
  expect(entityCount).toBeGreaterThan(0);
  console.log(`Found ${entityCount} songs`);

  // Click the first song to focus it
  const firstEntity = page.locator(".sidebar-entity-item").first();
  const songName = await firstEntity.textContent();
  console.log(`Clicking song: ${songName}`);
  await firstEntity.click();

  // Wait for the focus card to show the entity
  await expect(page.locator(".focus-card").first()).toBeVisible({ timeout: 10000 });

  await page.screenshot({
    path: "tests/browse-real-data.png",
    fullPage: true,
  });
});

test("browse pattern languages", async ({ page }) => {
  await page.goto("/index.html");

  // Login
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Open sidebar and browse pattern/language
  await page.locator(".sidebar-toggle").click();
  await expect(
    page.locator(".sidebar-type-item", { hasText: "pattern/language" }).first()
  ).toBeVisible({ timeout: 5000 });
  await page
    .locator(".sidebar-type-item", { hasText: "pattern/language" })
    .first()
    .click();

  // Wait for entities
  await expect(page.locator(".sidebar-entity-item").first()).toBeVisible({
    timeout: 5000,
  });

  const count = await page.locator(".sidebar-entity-item").count();
  console.log(`Found ${count} pattern languages`);
  expect(count).toBeGreaterThan(10);

  // Click one
  await page.locator(".sidebar-entity-item").first().click();
  await expect(page.locator(".focus-card").first()).toBeVisible({ timeout: 10000 });

  await page.screenshot({
    path: "tests/browse-patterns.png",
    fullPage: true,
  });
});
