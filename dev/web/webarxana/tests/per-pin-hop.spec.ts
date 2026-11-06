import { test, expect } from "@playwright/test";

// Per-pin hop-depth controls (M-webarxana acceptance item).
// Regression shape: state/set-pin-k! existed but graph.cljs overwrote
// every pin's :k with the global toolbar value, so per-pin adjustment
// could never become visible. These checks exercise the per-pin controls
// in the multi-focus UI and their retention across a reload (pins route
// segment id@k round-trip).
// Requires the dev stack on :3100 (server + futon1a backend).

async function login(page: any) {
  await page.goto("/index.html");
  await page.locator('input[type="text"]').fill("joe");
  await page.locator('input[type="password"]').fill("arxana");
  await page.locator("button", { hasText: "Sign in" }).click();
  await expect(
    page.locator('input[placeholder="Search by name..."]')
  ).toBeVisible({ timeout: 5000 });
}

async function pinViaSearch(page: any, name: string) {
  const search = page.locator('input[placeholder="Search by name..."]');
  await search.fill(name);
  await search.press("Enter");
  await expect(page.locator(".focus-card").first()).toBeVisible({
    timeout: 10000,
  });
  // Search focuses + auto-pins; ensure it is a pin via the card's pin
  // button if the card exists but is not pinned yet.
  const card = page.locator(".focus-card").first();
  if ((await card.locator(".pin-k-controls").count()) === 0) {
    await page.locator(".focus-card").first().locator("button", { hasText: "○" }).first().click();
    await expect(
      page.locator(".focus-card").first().locator(".pin-k-controls")
    ).toBeVisible({ timeout: 5000 });
  }
}

test("per-pin hop controls adjust one pin without disturbing another", async ({
  page,
}) => {
  await login(page);
  await pinViaSearch(page, "Abi");

  // Pin a second entity via the sidebar
  await page.locator(".sidebar-toggle").click();
  await expect(
    page.locator(".sidebar-type-item", { hasText: "arxana/song" })
  ).toBeVisible({ timeout: 5000 });
  await page.locator(".sidebar-type-item", { hasText: "arxana/song" }).click();
  await page.locator(".sidebar-entity-item").first().waitFor({ timeout: 5000 });
  await page.locator(".pin-btn").first().click();
  await page.waitForTimeout(2000);

  const cards = page.locator(".focus-card");
  expect(await cards.count()).toBeGreaterThanOrEqual(2);

  const firstCard = cards.first();
  const secondCard = cards.nth(1);
  const firstBefore = await firstCard.locator(".pin-k-label").textContent();
  const secondBefore = await secondCard.locator(".pin-k-label").textContent();

  // Adjust only the first pin
  await firstCard.locator(".pin-k-inc").click();
  await expect(firstCard.locator(".pin-k-label")).not.toHaveText(firstBefore);

  // The other pin's label is unchanged — per-pin retention
  await expect(secondCard.locator(".pin-k-label")).toHaveText(secondBefore);
});

test("per-pin hop depth survives a reload via the pins route segment", async ({
  page,
}) => {
  await login(page);
  await pinViaSearch(page, "Abi");

  const card = page.locator(".focus-card").first();
  const before = await card.locator(".pin-k-label").textContent();
  await card.locator(".pin-k-inc").click();
  const after = await card.locator(".pin-k-label").textContent();
  expect(after).not.toBe(before);

  // The URL encodes the adjusted pin as id@k
  const hash = new URL(page.url()).hash;
  expect(hash).toMatch(/@(\d+)/);

  await page.reload();
  await expect(page.locator(".focus-card").first()).toBeVisible({
    timeout: 15000,
  });
  await page.waitForTimeout(4000); // allow async pin restore + set-pin-k!
  await expect(
    page.locator(".focus-card").first().locator(".pin-k-label")
  ).toHaveText(after);
});
