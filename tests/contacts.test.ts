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
  page.on("dialog", async (dialog) => {
    expect(dialog.type()).toContain("confirm");
    expect(dialog.message()).toContain("Send to all contacts?");
    await dialog.accept();
  });
  await page.goto("http://localhost:8080");
  await page.locator("html").click();
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page
    .locator("div")
    .filter({ hasText: /^Name:$/ })
    .click();
  await page.locator('input[name="name"]').fill("Frank Jones");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("frank@example.com");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bob Smith");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bob@bob.org");
  await page.locator('input[name="email"]').press("Tab");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("link", { name: "New Mailing" }).click();
  await page.locator('input[name="subject"]').click();
  await page.locator('input[name="subject"]').fill("My First Mailing");
  await page.locator("trix-editor").click();
  await page.locator("trix-editor").fill("Hi everyone.\n\nThis is my ");
  await page.getByRole("button", { name: "Bold" }).click();
  await page
    .locator("trix-editor")
    .fill("Hi everyone.\n\nThis is my formatted");
  await page.getByRole("button", { name: "Bold" }).click();
  await page
    .locator("trix-editor")
    .fill("Hi everyone.\n\nThis is my formatted mailing");
  await page.getByRole("button", { name: "Create Mailing" }).click();
  await page.getByRole("link", { name: "My First Mailing Draft" }).click();
  await page.getByRole("button", { name: "Send Mailing" }).click();
  await expect(
    page.getByRole("link", { name: "My First Mailing Sent" })
  ).toBeVisible();

  await page.goto("http://localhost:8025/");
  await expect(
    page.getByRole("link", { name: "sender@example.com bob@bob." })
  ).toBeVisible();
  await page.getByRole("link", { name: "sender@example.com frank@" }).click();
  await expect(
    page
      .frameLocator("#preview-html")
      .getByText(
        "Hi everyone.This is my formatted mailing No more updates please or update your"
      )
  ).toBeVisible();
  execSync("cabal run clean-db");
  await fetch(`http://localhost:8025/api/v1/messages`, {
    method: "DELETE",
  });
});

test("allow recipient to unsubscribe", async ({ page }) => {
  page.on("dialog", async (dialog) => {
    await dialog.accept();
  });
  await page.goto("http://localhost:8080/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Frank Jones");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("frank@example.com");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bob Smith");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bob@example.com");
  await page.locator('input[name="email"]').press("Tab");
  await page.locator('input[name="group"]').press("Enter");
  await page.getByRole("link", { name: "New Mailing" }).click();
  await page.locator('input[name="subject"]').click();
  await page.locator('input[name="subject"]').fill("Hi");
  await page.locator("trix-editor").click();
  await page.locator("trix-editor").fill("Hello");
  await page.getByRole("button", { name: "Create Mailing" }).click();
  await page.getByRole("link", { name: "Hi Draft" }).click();
  await page.getByRole("button", { name: "Send Mailing" }).click();
  await page.goto("http://localhost:8025/");
  await page.getByRole("link", { name: "sender@example.com bob@" }).click();
  const page1Promise = page.waitForEvent("popup");
  await page
    .frameLocator("#preview-html")
    .getByRole("link", { name: "No more updates please" })
    .click();
  const page1 = await page1Promise;
  await page1.locator("#unsubscribe-box").check();
  await page1.getByRole("button", { name: "Unsubscribe" }).click();
  await expect(
    page1.getByRole("heading", { name: "Unsubscribed" })
  ).toBeVisible();
  await page1.goto("http://localhost:8025/");
  await page1.getByRole("link", { name: "sender@example.com admin@" }).click();
  await expect(page1.getByText("Bob Smith (bob@example.com)")).toBeVisible();
  await expect(page1.getByText("Unsubscription Notice")).toBeVisible();
  await page1.goto("http://localhost:8080/");
  await page1.getByRole("link", { name: "Contacts" }).click();
  await expect(
    page1.getByRole("cell", { name: "Bob Smith" })
  ).not.toBeVisible();
});

test("allow recipient to update their contact info", async ({ page }) => {
  page.on("dialog", async (dialog) => {
    await dialog.accept();
  });
  await page.goto("http://localhost:8080/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("link", { name: "Add Contact" }).click();
  await page.locator('input[name="name"]').click();
  await page.locator('input[name="name"]').fill("Bob Smith");
  await page.locator('input[name="name"]').press("Tab");
  await page.locator('input[name="email"]').fill("bob@example.com");
  await page.getByRole("button", { name: "Create Contact" }).click();
  await page.getByRole("link", { name: "New Mailing" }).click();
  await page.locator('input[name="subject"]').click();
  await page.locator('input[name="subject"]').fill("Hi");
  await page.locator('input[name="subject"]').press("Tab");
  await page.locator("trix-editor").fill("hello");
  await page.getByRole("button", { name: "Create Mailing" }).click();
  await page.getByRole("link", { name: "Hi Draft" }).click();
  await page.getByRole("button", { name: "Send Mailing" }).click();
  await page.goto("http://localhost:8025/");
  await page.getByRole("link", { name: "sender@example.com bob@" }).click();
  const page1Promise = page.waitForEvent("popup");
  await page
    .frameLocator("#preview-html")
    .getByRole("link", { name: "update your contact info" })
    .click();
  const page1 = await page1Promise;
  await page1.getByLabel("Name").click();
  await page1.getByLabel("Name").fill("Bob Smath");
  await page1.locator('input[name="email"]').click();
  await page1.locator('input[name="email"]').fill("bob@example.ca");
  await page1.getByRole("button", { name: "Update" }).click();
  await expect(
    page1.getByRole("heading", { name: "Your contact info has been" })
  ).toBeVisible();
  await expect(page1.getByText("Name: Bob Smath")).toBeVisible();
  await expect(page1.getByText("Email: bob@example.ca")).toBeVisible();
  await page1.getByRole("link", { name: "Made a mistake?" }).click();
  await page1.getByLabel("Name").click();
  await page1.getByLabel("Name").press("ArrowLeft");
  await page1.getByLabel("Name").press("ArrowLeft");
  await page1.getByLabel("Name").fill("Bob Smoth");
  await page1.locator('input[name="email"]').click();
  await page1.locator('input[name="email"]').fill("bob@example.co");
  await page1.getByRole("button", { name: "Update" }).click();
  await expect(page1.getByText("Name: Bob Smoth")).toBeVisible();
  await expect(page1.getByText("Email: bob@example.co")).toBeVisible();
  await page1.goto("http://localhost:8080/");
  await page1.getByRole("link", { name: "Contacts" }).click();
  await expect(
    page1.getByRole("cell", { name: "bob@example.co" })
  ).toBeVisible();
  await page1.getByRole("cell", { name: "Bob Smoth" }).click();
  await page1.goto("http://localhost:8025/");
  await page1
    .getByRole("link", {
      name: "sender@example.com admin@example.com Contact Updated Old contact info: Bob Smath bob@example.ca New contact info: Bob Smoth bob@example.co 545 B a few seconds ago",
      exact: true,
    })
    .click();
  await expect(page1.getByText("Contact Updated")).toBeVisible();
  await expect(page1.getByText("Old contact info: Bob Smath")).toBeVisible();
  await expect(page1.getByText("New contact info: Bob Smoth")).toBeVisible();
});
