import { test, expect } from "@playwright/test";

test("has title", async ({ page }) => {
  await page.goto("/");

  // Expect a title "to contain" a substring.
  await expect(page).toHaveTitle(/mailings/i);
});

test.describe.configure({ mode: "serial" });

test("add/update contacts", async ({ page }) => {
  await page.goto("/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bill Smith");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bill@example.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').fill("friends");
  await page.locator('input[name="group"]').press("Tab");
  await page.locator('input[name="notes"]').fill("nice guy");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await expect(page.getByRole("cell", { name: "Bill Smith" })).toBeVisible();
  await expect(
    page.getByRole("cell", { name: "bill@example.com" })
  ).toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "nice guy" })).toBeVisible();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Frank Jones");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("frank@example.ca");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').press("Enter");
  await expect(page.getByRole("cell", { name: "Frank Jones" })).toBeVisible();
  await expect(
    page.getByRole("cell", { name: "frank@example.ca" })
  ).toBeVisible();
  await expect(
    page
      .getByRole("row", { name: "Frank Jones frank@example.ca" })
      .getByRole("button")
  ).toBeVisible();
  await page.getByRole("link", { name: "Bill Smith" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').press("ArrowLeft");
  await page.locator('input[name="name"]').press("ArrowLeft");
  await page.locator('input[name="name"]').press("ArrowLeft");
  await page.locator('input[name="name"]').press("ArrowLeft");
  await page.locator('input[name="name"]').press("ArrowLeft");
  await page.locator('input[name="name"]').fill("Bill W. Smith");
  await page.locator('input[name="email"]').click();
  await page.locator('input[name="email"]').fill("bill@example.org");
  await page.locator('input[name="notes"]').click();
  await page.locator('input[name="notes"]').click();
  await page.locator('input[name="notes"]').press("ArrowLeft");
  await page.locator('input[name="notes"]').press("ArrowLeft");
  await page.locator('input[name="notes"]').press("ArrowLeft");
  await page.locator('input[name="notes"]').press("ArrowLeft");
  await page.locator('input[name="notes"]').fill("cool guy");
  await page.locator('input[name="group"]').click();
  await page.locator('input[name="group"]').fill("");
  await page.getByRole("button", { name: "Edit Contact" }).click();
  await expect(page.getByRole("cell", { name: "Bill W. Smith" })).toBeVisible();
  await expect(
    page.getByRole("cell", { name: "bill@example.org" })
  ).toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).not.toBeVisible();
  await expect(page.getByRole("cell", { name: "cool guy" })).toBeVisible();
});

// test("create and send mailing", async ({ page }) => {
//   page.on("dialog", async (dialog) => {
//     expect(dialog.type()).toContain("confirm");
//     expect(dialog.message()).toContain("Send to all");
//     await dialog.accept();
//   });
//   await fetch(`${process.env.MAILPIT_DOMAIN}/api/v1/messages`, {
//     method: "DELETE",
//   });
//   await new Promise((r) => setTimeout(r, 1000));
//   await page.goto("/");
//   await page.getByRole("link", { name: "Contacts" }).click();
//   await page.getByRole("link", { name: "Add Contact" }).click();
//   await page.locator('input[name="name"]').click();
//   await page.locator('input[name="name"]').fill("Test R");
//   await page.locator('input[name="name"]').press("Tab");
//   await page.locator('input[name="email"]').fill("bill@bill.com");
//   await page.getByRole("button", { name: "Create Contact" }).click();
//   await page.getByRole("link", { name: "New Mailing" }).click();
//   await page.locator('input[name="subject"]').click();
//   await page.locator('input[name="subject"]').fill("abc");
//   await page.locator('input[name="subject"]').press("Tab");
//   await page.locator("trix-editor").fill("content");
//   await page.getByRole("button", { name: "Create Mailing" }).click();
//   await expect(page.getByRole("link", { name: "abc Draft" })).toBeVisible();
//   await page.getByRole("link", { name: "abc Draft" }).click();
//   await page.getByRole("button", { name: "Send Mailing" }).click();
//   await expect(page.getByRole("link", { name: "abc Sent" })).toBeVisible();
//   // wait for mailing to be sent
//   await new Promise((r) => setTimeout(r, 5000));
//   const res = await fetch(`${process.env.MAILPIT_DOMAIN}/api/v1/messages`);
//   const resj = await res.json();
//   expect(resj.messages.length).toBe(2);
//   expect(resj.messages.every((m) => m.Subject === "abc")).toBeTruthy();
//   expect(
//     resj.messages.some(
//       (m) => m.To[0].Name === "Test R" && m.To[0].Address === "bill@bill.com"
//     )
//   ).toBeTruthy();
// });
