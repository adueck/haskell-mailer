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

// test("create and send mailing", async ({ page }) => {
//   page.on("dialog", async (dialog) => {
//     expect(dialog.type()).toContain("confirm");
//     expect(dialog.message()).toContain("Send to all contacts?");
//     await dialog.accept();
//   });
//   await page.goto("http://localhost:8080");
//   await page.locator("html").click();
//   await page.getByRole("link", { name: "Contacts" }).click();
//   await page.getByRole("link", { name: "Add Contact" }).click();
//   await page
//     .locator("div")
//     .filter({ hasText: /^Name:$/ })
//     .click();
//   await page.locator('input[name="name"]').fill("Frank Jones");
//   await page.locator('input[name="name"]').press("Tab");
//   await page.locator('input[name="email"]').fill("frank@example.com");
//   await page.getByRole("button", { name: "Create Contact" }).click();
//   await page.getByRole("link", { name: "Add Contact" }).click();
//   await page.locator('input[name="name"]').click();
//   await page.locator('input[name="name"]').fill("Bob Smith");
//   await page.locator('input[name="name"]').press("Tab");
//   await page.locator('input[name="email"]').fill("bob@bob.org");
//   await page.locator('input[name="email"]').press("Tab");
//   await page.getByRole("button", { name: "Create Contact" }).click();
//   await page.getByRole("link", { name: "New Mailing" }).click();
//   await page.locator('input[name="subject"]').click();
//   await page.locator('input[name="subject"]').fill("My First Mailing");
//   await page.locator("trix-editor").click();
//   await page.locator("trix-editor").fill("Hi everyone.\n\nThis is my ");
//   await page.getByRole("button", { name: "Bold" }).click();
//   await page
//     .locator("trix-editor")
//     .fill("Hi everyone.\n\nThis is my formatted");
//   await page.getByRole("button", { name: "Bold" }).click();
//   await page
//     .locator("trix-editor")
//     .fill("Hi everyone.\n\nThis is my formatted mailing");
//   await page.getByRole("button", { name: "Create Mailing" }).click();
//   await page.getByRole("link", { name: "My First Mailing Draft" }).click();
//   await page.getByRole("button", { name: "Send Mailing" }).click();
//   await expect(
//     page.getByRole("link", { name: "My First Mailing Sent" })
//   ).toBeVisible();

//   await page.goto("http://localhost:8025/");
//   await expect(
//     page.getByRole("link", { name: "sender@example.com bob@bob." })
//   ).toBeVisible();
//   await page.getByRole("link", { name: "sender@example.com frank@" }).click();
//   await expect(
//     page
//       .frameLocator("#preview-html")
//       .getByText(
//         "Hi everyone.This is my formatted mailing No more updates please or update your"
//       )
//   ).toBeVisible();
//   execSync("cabal run clean-db");
//   await fetch(`http://localhost:8025/api/v1/messages`, {
//     method: "DELETE",
//   });
// });

// test("allow recipient to unsubscribe", async ({ page }) => {
//   page.on("dialog", async (dialog) => {
//     await dialog.accept();
//   });
//   await page.goto("http://localhost:8080/");
//   await page.getByRole("link", { name: "Contacts" }).click();
//   await page.getByRole("link", { name: "Add Contact" }).click();
//   await page.locator('input[name="name"]').click();
//   await page.locator('input[name="name"]').fill("Frank Jones");
//   await page.locator('input[name="name"]').press("Tab");
//   await page.locator('input[name="email"]').fill("frank@example.com");
//   await page.getByRole("button", { name: "Create Contact" }).click();
//   await page.getByRole("link", { name: "Add Contact" }).click();
//   await page.locator('input[name="name"]').click();
//   await page.locator('input[name="name"]').fill("Bob Smith");
//   await page.locator('input[name="name"]').press("Tab");
//   await page.locator('input[name="email"]').fill("bob@example.com");
//   await page.locator('input[name="email"]').press("Tab");
//   await page.locator('input[name="group"]').press("Enter");
//   await page.getByRole("link", { name: "New Mailing" }).click();
//   await page.locator('input[name="subject"]').click();
//   await page.locator('input[name="subject"]').fill("Hi");
//   await page.locator("trix-editor").click();
//   await page.locator("trix-editor").fill("Hello");
//   await page.getByRole("button", { name: "Create Mailing" }).click();
//   await page.getByRole("link", { name: "Hi Draft" }).click();
//   await page.getByRole("button", { name: "Send Mailing" }).click();
//   await page.goto("http://localhost:8025/");
//   await page.getByRole("link", { name: "sender@example.com bob@" }).click();
//   const page1Promise = page.waitForEvent("popup");
//   await page
//     .frameLocator("#preview-html")
//     .getByRole("link", { name: "No more updates please" })
//     .click();
//   const page1 = await page1Promise;
//   await page1.locator("#unsubscribe-box").check();
//   await page1.getByRole("button", { name: "Unsubscribe" }).click();
//   await expect(
//     page1.getByRole("heading", { name: "Unsubscribed" })
//   ).toBeVisible();
//   await page1.goto("http://localhost:8025/");
//   await page1.getByRole("link", { name: "sender@example.com admin@" }).click();
//   await expect(page1.getByText("Bob Smith (bob@example.com)")).toBeVisible();
//   await expect(page1.getByText("Unsubscription Notice")).toBeVisible();
//   await page1.goto("http://localhost:8080/");
//   await page1.getByRole("link", { name: "Contacts" }).click();
//   await expect(
//     page1.getByRole("cell", { name: "Bob Smith" })
//   ).not.toBeVisible();
// });

// test("allow recipient to update their contact info", async ({ page }) => {
//   page.on("dialog", async (dialog) => {
//     await dialog.accept();
//   });
//   await page.goto("http://localhost:8080/");
//   await page.getByRole("link", { name: "Contacts" }).click();
//   await page.getByRole("link", { name: "Add Contact" }).click();
//   await page.locator('input[name="name"]').click();
//   await page.locator('input[name="name"]').fill("Bob Smith");
//   await page.locator('input[name="name"]').press("Tab");
//   await page.locator('input[name="email"]').fill("bob@example.com");
//   await page.locator('input[name="email"]').press("Enter");
//   await page.getByRole("link", { name: "New Mailing" }).click();
//   await page.locator('input[name="subject"]').click();
//   await page.locator('input[name="subject"]').fill("Hi There");
//   await page.locator('input[name="subject"]').press("Tab");
//   await page.locator("trix-editor").fill("Hello");
//   await page.getByRole("button", { name: "Create Mailing" }).click();
//   await page.getByRole("link", { name: "Hi There Draft" }).click();
//   await page.getByRole("button", { name: "Send Mailing" }).click();
//   await page.goto("http://localhost:8025/");
//   await page.getByRole("link", { name: "sender@example.com bob@" }).click();
//   const page1Promise = page.waitForEvent("popup");
//   await page
//     .frameLocator("#preview-html")
//     .getByRole("link", { name: "update your contact info" })
//     .click();
//   const page1 = await page1Promise;
//   await page1.getByLabel("Name").click();
//   await page1.getByLabel("Name").click();
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").fill("Frank Smith");
//   await page1.locator('input[name="email"]').click();
//   await page1.locator('input[name="email"]').click();
//   await page1.locator('input[name="email"]').press("ControlOrMeta+ArrowLeft");
//   await page1.locator('input[name="email"]').press("ControlOrMeta+ArrowLeft");
//   await page1.locator('input[name="email"]').press("ArrowRight");
//   await page1.locator('input[name="email"]').press("ArrowRight");
//   await page1.locator('input[name="email"]').press("ArrowRight");
//   await page1.locator('input[name="email"]').fill("frank@example.com");
//   await page1.getByRole("button", { name: "Update" }).click();
//   await expect(
//     page1.getByRole("heading", { name: "Your contact info has been" })
//   ).toBeVisible();
//   await page1.getByText("Name: Frank Smith").click();
//   await expect(page1.getByText("Name: Frank Smith")).toBeVisible();
//   await expect(page1.getByText("Email: frank@example.com")).toBeVisible();
//   await page.getByRole("link", { name: "MailpitMailpit" }).click();
//   await page.getByRole("link", { name: "sender@example.com admin@" }).click();
//   await expect(page.getByText("Old contact info: Bob Smith")).toBeVisible();
//   await page.getByRole("button", { name: " Delete" }).click();
//   await page1.getByRole("link", { name: "Made a mistake?" }).click();
//   await page1.getByLabel("Name").click();
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").press("ArrowLeft");
//   await page1.getByLabel("Name").fill("John Smith");
//   await page1.locator('input[name="email"]').click();
//   await page1.locator('input[name="email"]').press("Alt+ArrowLeft");
//   await page1.locator('input[name="email"]').press("Alt+ArrowLeft");
//   await page1.locator('input[name="email"]').press("Alt+ArrowLeft");
//   await page1.locator('input[name="email"]').press("Alt+ArrowLeft");
//   await page1.locator('input[name="email"]').fill("john@example.com");
//   await page1.locator('input[name="email"]').press("Enter");
//   await expect(
//     page1.getByRole("heading", { name: "Your contact info has been" })
//   ).toBeVisible();
//   await page1.getByText("Name: John Smith").click();
//   await expect(page1.getByText("Name: John Smith")).toBeVisible();
//   await expect(page1.getByText("Email: john@example.com")).toBeVisible();
//   await page.getByRole("link", { name: "sender@example.com admin@" }).click();
//   await expect(page.getByText("Old contact info: Frank Smith")).toBeVisible();
//   await expect(page.getByText("New contact info: John Smith")).toBeVisible();
// });
