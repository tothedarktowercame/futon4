import { test, expect } from "@playwright/test";

test("+ Adjacent opens dialog, creates entity with relation", async ({
  page,
}) => {
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

  // Click + Adjacent — should open creation dialog
  await page.locator(".btn-new", { hasText: "+ Adjacent" }).click();
  await expect(page.locator(".creation-dialog")).toBeVisible({ timeout: 3000 });

  // Fill in name and create
  await page
    .locator('.creation-field input[type="text"]')
    .fill("Adjacent test node");
  await page.locator(".btn-create").click();

  // Focus should shift to the new node
  await expect(page.locator(".focus-card .card-name")).toHaveText(
    "Adjacent test node",
    { timeout: 10000 }
  );

  await page.screenshot({ path: "tests/adjacent.png", fullPage: true });
});
