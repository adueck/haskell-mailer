import { test, expect } from "@playwright/test";
import { execSync } from "node:child_process";

test.describe.configure({ mode: "serial" });

async function resetState() {
  execSync("cabal run clean-db");
  await fetch(`http://localhost:8025/api/v1/messages`, {
    method: "DELETE",
  });
}

test.beforeEach(resetState);

test.afterEach(resetState);

// TODO test:
//  - allow contacts to unsubscribe
//  - allow contacts to update contact info

test("add/update/delete contacts", async ({ page }) => {
  page.on("dialog", async (dialog) => {
    expect(dialog.type()).toContain("confirm");
    expect(dialog.message()).toContain("Delete contact?");
    await dialog.accept();
  });
  await page.goto("http://localhost:8080/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("button", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bill J");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bill@bill.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').fill("friends");
  await page.locator('input[name="group"]').press("Tab");
  await page.locator('input[name="notes"]').fill("from school");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("button", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Frank J");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("frank@guy.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').press("Enter");
  await expect(page.getByRole("cell", { name: "Bill J" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "bill@bill.com" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "from school" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "frank@guy.com" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "Frank J" })).toBeVisible();
  await page.getByRole("link", { name: "Bill J" }).click();
  await page.getByRole("button", { name: "Delete Contact" }).click();
  await expect(page.getByRole("cell", { name: "Bill J" })).not.toBeVisible();
  await expect(
    page.getByRole("cell", { name: "bill@bill.com" })
  ).not.toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).not.toBeVisible();
  await expect(
    page.getByRole("cell", { name: "from school" })
  ).not.toBeVisible();
  await page.getByRole("link", { name: "Frank J" }).click();
  await page.locator('input[name="group"]').click();
  await page.locator('input[name="group"]').fill("co-workers");
  await page.locator('input[name="notes"]').click();
  await page.locator('input[name="notes"]').fill("from accounting");
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Frank James");
  await page.locator('input[name="email"]').click();
  await page.locator('input[name="email"]').fill("frank@guy.ca");
  await page.getByRole("button", { name: "Edit Contact" }).click();
  await expect(page.getByRole("cell", { name: "Frank James" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "frank@guy.ca" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "co-workers" })).toBeVisible();
  await expect(
    page.getByRole("cell", { name: "from accounting" })
  ).toBeVisible();
});

test("create and send mailing", async ({ page }) => {
  page.on("dialog", async (dialog) => {
    expect(dialog.type()).toContain("confirm");
    await dialog.accept();
  });
  await page.goto("http://localhost:8080/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("button", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bill B");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bill@b.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("link", { name: "New Mailing" }).click();
  await page.locator('input[name="subject"]').click();
  await page.locator('input[name="subject"]').fill("My Test");
  await page.locator("trix-editor").click();
  await page.locator("trix-editor").fill("Hi there");
  await page.getByRole("button", { name: "Create Mailing" }).click();
  await expect(page.getByRole("heading", { name: "Mailings" })).toBeVisible();
  await page.getByRole("link", { name: "My Test Draft" }).click();
  await page.getByRole("button", { name: "Send Mailing" }).click();
  await sleep(500);
  const msgSummary = (
    await (await fetch("http://localhost:8025/api/v1/messages")).json()
  ).messages[0];
  expect(msgSummary.To[0].Name).toBe("Bill B");
  expect(msgSummary.To[0].Address).toBe("bill@b.com");
  const msg = await (
    await fetch(`http://localhost:8025/api/v1/message/${msgSummary.ID}`)
  ).json();
  expect(msg.Subject).toBe("My Test");
  expect(msg.Text).toContain("Hi there");
  expect(msg.HTML).toContain("Hi there");
});

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
