import { test, expect } from "@playwright/test";
import { execSync } from "node:child_process";

test("has title", async ({ page }) => {
  await page.goto("/");

  // Expect a title "to contain" a substring.
  await expect(page).toHaveTitle(/mailings/i);
});

test.describe.configure({ mode: "serial" });

test("add/update/delete contacts", async ({ page }) => {
  page.on("dialog", async (dialog) => {
    expect(dialog.type()).toContain("confirm");
    expect(dialog.message()).toContain("Delete contact?");
    await dialog.accept();
  });
  await page.goto("/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bill Joe");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bill@joe.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').fill("friends");
  await page.locator('input[name="group"]').press("Tab");
  await page.locator('input[name="notes"]').fill("His name is bill");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Frank Smith");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("frank@smith.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').fill("abc");
  await page.locator('input[name="group"]').press("Enter");
  await expect(page.getByRole("link", { name: "Bill Joe" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "bill@joe.com" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).toBeVisible();
  await expect(
    page.getByRole("cell", { name: "His name is bill" })
  ).toBeVisible();
  await expect(page.getByRole("cell", { name: "abc" })).toBeVisible();
  await expect(
    page.getByRole("cell", { name: "frank@smith.com" })
  ).toBeVisible();
  await expect(page.getByRole("cell", { name: "Frank Smith" })).toBeVisible();
  await page.getByRole("link", { name: "Frank Smith" }).click();
  await page.getByRole("button", { name: "Delete Contact" }).click();
  await expect(
    page.getByRole("cell", { name: "Frank Smith" })
  ).not.toBeVisible();
  await page.getByRole("link", { name: "Bill Joe" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bill Jim");
  await page.locator('input[name="email"]').click();
  await page.locator('input[name="email"]').fill("bill@jim.com");
  await page.locator('input[name="group"]').click();
  await page.locator('input[name="group"]').fill("");
  await page.locator('input[name="notes"]').click();
  await page.locator('input[name="notes"]').fill("His name was bill");
  await page.getByRole("button", { name: "Edit Contact" }).click();
  await expect(page.getByRole("cell", { name: "Bill Joe" })).not.toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).not.toBeVisible();
  await expect(page.getByRole("cell", { name: "Bill Jim" })).toBeVisible();
  await page.getByRole("cell", { name: "bill@jim.com" }).click();
  await page.getByRole("cell", { name: "His name was bill" }).click();
  await page.getByRole("link", { name: "Bill Jim" }).click();
  await page.getByRole("button", { name: "Delete Contact" }).click();
  await expect(page.getByRole("cell", { name: "Bill Jim" })).not.toBeVisible();
});

test("create and send mailing", async ({ page }) => {
  execSync("cabal run clean-db");
  await fetch(`http://localhost:8025/api/v1/messages`, {
    method: "DELETE",
  });
  page.on("dialog", async (dialog) => {
    expect(dialog.type()).toContain("confirm");
    expect(dialog.message()).toContain("Send to all contacts?");
    await dialog.accept();
  });
  await page.goto("/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("bill smith");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bill@example.com");
  await page.locator('input[name="email"]').press("Enter");
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Frank Jones");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("frank@example.ca");
  await page.locator('input[name="email"]').press("Enter");
  await page.getByRole("link", { name: "New Mailing" }).click();
  await page.locator('input[name="subject"]').click();
  await page.locator('input[name="subject"]').fill("Test Mailing");
  await page.locator('input[name="subject"]').press("Tab");
  await page.locator("trix-editor").fill("mailing content here\n\n");
  await page.getByRole("button", { name: "Create Mailing" }).click();
  await page.getByRole("link", { name: "Test Mailing Draft" }).click();
  await page.getByRole("button", { name: "Send Mailing" }).click();
  await expect(
    page.getByRole("link", { name: "Test Mailing Sent" })
  ).toBeVisible();
  // allow time for sends
  // await new Promise((r) => setTimeout(r, 3000));
  // const res = await fetch(`http://localhost:8025/api/v1/messages`);
  // const resj = await res.json();
  // expect(resj.messages.length).toBe(2);
  // expect(resj.messages.every((m) => m.Subject === "Test Mailing")).toBeTruthy();
  // expect(
  //   resj.messages.some(
  //     (m) =>
  //       m.To[0].Name === "bill smith" && m.To[0].Address === "bill@example.com"
  //   )
  // ).toBeTruthy();
  // execSync("cabal run clean-db");
  // await fetch(`http://localhost:8025/api/v1/messages`, {
  //   method: "DELETE",
  // });
});
