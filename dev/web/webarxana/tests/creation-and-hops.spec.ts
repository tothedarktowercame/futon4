import { test, expect } from "@playwright/test";

test("+ button creates a scratch card floating left", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Click + — should create a scratch card
  await page.locator(".btn-new-node").click();

  // Scratch card should appear with name input, text area, and buttons
  await expect(page.locator(".scratch-card")).toBeVisible({ timeout: 5000 });
  await expect(
    page.locator('.scratch-card input[placeholder="Name..."]')
  ).toBeVisible();
  await expect(
    page.locator('.scratch-card textarea[placeholder="Write here..."]')
  ).toBeVisible();
  await expect(
    page.locator(".scratch-card .btn-save", { hasText: "Save" })
  ).toBeVisible();
  await expect(
    page.locator(".scratch-card .btn-edit", { hasText: "Focus" })
  ).toBeVisible();

  // Type something and save
  await page
    .locator('.scratch-card input[placeholder="Name..."]')
    .fill("My new node");
  await page
    .locator('.scratch-card textarea')
    .fill("Some content here");
  await page.locator(".scratch-card .btn-save").click();

  // Scratch card should still be visible with the saved name
  await expect(page.locator(".scratch-card")).toBeVisible();
});

test("scratch card Focus button focuses the new node", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  await page.locator(".btn-new-node").click();
  await expect(page.locator(".scratch-card")).toBeVisible({ timeout: 5000 });

  // Give it a name first
  await page
    .locator('.scratch-card input[placeholder="Name..."]')
    .fill("Focus test");

  // Click Focus — should focus this node and dismiss scratch card
  await page.locator(".scratch-card .btn-edit", { hasText: "Focus" }).click();

  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 5000 });
  // Scratch card should be gone (node removed from scratchpad)
  await expect(page.locator(".scratch-card")).not.toBeVisible({ timeout: 3000 });
});

test("scratch card Connect enters connect mode", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Navigate to Abi so we have a graph
  await page.locator('input[placeholder="Search by name..."]').fill("Abi");
  await page.locator('input[placeholder="Search by name..."]').press("Enter");
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  // Create scratch node
  await page.locator(".btn-new-node").click();
  await expect(page.locator(".scratch-card")).toBeVisible({ timeout: 5000 });

  // Click Connect on the scratch card
  await page
    .locator(".scratch-card .scratchpad-btn", { hasText: "Connect" })
    .click();

  // Should show connect banner in sidebar
  await page.locator(".sidebar-toggle").click();
  await expect(page.locator(".connect-banner")).toBeVisible({ timeout: 3000 });

  // Click a node in the graph to complete
  await page
    .locator("svg text", { hasText: /^Abi$/ })
    .last()
    .click({ force: true });

  await expect(page.locator(".connect-banner")).not.toBeVisible({
    timeout: 5000,
  });
});

test("scratch card dismiss button removes it", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  await page.locator(".btn-new-node").click();
  await expect(page.locator(".scratch-card")).toBeVisible({ timeout: 5000 });

  // Click the × button
  await page.locator(".scratch-dismiss").click();
  await expect(page.locator(".scratch-card")).not.toBeVisible({ timeout: 3000 });
});

test("name auto-populates from first line on Enter", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  await page.locator(".btn-new-node").click();
  await expect(page.locator(".scratch-card")).toBeVisible({ timeout: 5000 });

  // Name should be empty
  await expect(
    page.locator('.scratch-card input[placeholder="Name..."]')
  ).toHaveValue("");

  // Type a first line then press Enter
  const textarea = page.locator(".scratch-card textarea");
  await textarea.fill("My first line");
  await textarea.press("Enter");

  // Name should auto-populate from the first line
  await expect(
    page.locator('.scratch-card input[placeholder="Name..."]')
  ).toHaveValue("My first line", { timeout: 3000 });
});

test("+ Adjacent still opens creation dialog", async ({ page }) => {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  await page.locator('input[placeholder="Search by name..."]').fill("Abi");
  await page.locator('input[placeholder="Search by name..."]').press("Enter");
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });

  await page.locator(".btn-new", { hasText: "+ Adjacent" }).click();
  await expect(page.locator(".creation-dialog")).toBeVisible({ timeout: 3000 });
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
