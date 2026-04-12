import { test, expect } from "@playwright/test";

test("top-level + button opens creation dialog and creates node", async ({
  page,
}) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Click the + button
  await page.locator(".btn-new-node").click();

  // Creation dialog should appear
  await expect(page.locator(".creation-dialog")).toBeVisible({ timeout: 3000 });
  await expect(page.locator(".creation-header")).toHaveText("New node");

  // Should have name input, type dropdown, but no relation dropdown
  await expect(page.locator('.creation-field input[type="text"]')).toBeVisible();
  await expect(page.locator(".creation-field select")).toHaveCount(1); // only type, no relation

  // Fill in name and create
  await page.locator('.creation-field input[type="text"]').fill("Test node");
  await page.locator(".btn-create").click();

  // Dialog should close and focus card should show the new node
  await expect(page.locator(".creation-dialog")).not.toBeVisible({
    timeout: 5000,
  });
  await expect(page.locator(".focus-card .card-name")).toHaveText("Test node", {
    timeout: 5000,
  });
});

test("+ Adjacent opens dialog with relation picker", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Search for Abi to get a focused entity
  await page.locator('input[placeholder="Search by name..."]').fill("Abi");
  await page.locator('input[placeholder="Search by name..."]').press("Enter");
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  // Click + Adjacent
  await page.locator(".btn-new", { hasText: "+ Adjacent" }).click();

  // Dialog should show with relation picker
  await expect(page.locator(".creation-dialog")).toBeVisible({ timeout: 3000 });
  await expect(page.locator(".creation-header")).toHaveText(
    "New adjacent node"
  );
  // Should have 2 selects: type + relation
  await expect(page.locator(".creation-field select")).toHaveCount(2);

  // Fill in and create
  await page
    .locator('.creation-field input[type="text"]')
    .fill("Abi annotation");
  await page.locator(".btn-create").click();

  await expect(page.locator(".focus-card .card-name")).toHaveText(
    "Abi annotation",
    { timeout: 5000 }
  );
});

test("hop depth controls adjust neighbourhood size", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Should show k=3 by default
  await expect(page.locator(".hop-label")).toHaveText("k=3");

  // Click minus to reduce
  await page.locator(".hop-btn").first().click();
  await expect(page.locator(".hop-label")).toHaveText("k=2");

  // Click minus again
  await page.locator(".hop-btn").first().click();
  await expect(page.locator(".hop-label")).toHaveText("k=1");

  // Can't go below 1
  await page.locator(".hop-btn").first().click();
  await expect(page.locator(".hop-label")).toHaveText("k=1");

  // Click plus
  await page.locator(".hop-btn").last().click();
  await expect(page.locator(".hop-label")).toHaveText("k=2");
});
