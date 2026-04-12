import { test, expect } from "@playwright/test";

test("Abi song shows hyperedge connections", async ({ page }) => {
  await page.goto("/index.html");

  // Login
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });

  // Search for the Abi song by name
  const searchInput = page.locator('input[placeholder="Search by name..."]');
  await searchInput.fill("Abi");
  await searchInput.press("Enter");

  // Wait for focus card
  await expect(page.locator(".focus-card")).toBeVisible({ timeout: 10000 });
  await expect(page.locator(".focus-card .card-name")).toHaveText("Abi");

  // Wait for hyperedge connections to render in the SVG
  await expect(page.locator("svg")).toBeVisible({ timeout: 5000 });

  // Wait for hyperedge-sourced nodes to appear (async after ego fetch)
  await expect(
    page
      .locator("svg text", { hasText: "annotation/supports" })
      .first()
  ).toBeVisible({ timeout: 10000 });

  // Should have the focus node + connected nodes from hyperedges
  const circleCount = await page.locator("svg circle").count();
  console.log(`Circles: ${circleCount}`);
  expect(circleCount).toBeGreaterThan(2);

  const svgTexts = await page.locator("svg text").allTextContents();
  console.log(`SVG texts: ${svgTexts.join(", ")}`);

  await page.screenshot({
    path: "tests/hyperedges-abi.png",
    fullPage: true,
  });
});
