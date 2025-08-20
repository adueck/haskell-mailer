import { test, expect } from "@playwright/test";
import { execSync } from "node:child_process";
import * as cheerio from "cheerio";
import { stringify } from "csv-stringify/sync";
import { parse } from "csv-parse/sync";
import * as fs from "fs";
import { fail } from "node:assert";

test.describe.configure({ mode: "serial" });

async function resetState() {
  execSync("./init_db.sh");
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
    page.getByRole("cell", { name: "bill@bill.com" }),
  ).not.toBeVisible();
  await expect(page.getByRole("cell", { name: "friends" })).not.toBeVisible();
  await expect(
    page.getByRole("cell", { name: "from school" }),
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
    page.getByRole("cell", { name: "from accounting" }),
  ).toBeVisible();
});

test("upload and download contats CSV", async ({ page }) => {
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
  const csvData = [
    ["name", "email", "group", "notes"],
    ["joe abc", "abc@joe.com", "", "added later"],
    ["frank f", "frank@example.co", "carpool", ""],
    ["frank g", "frankg@example.com", "work", "in accounting"],
  ];
  const csvContent = stringify(csvData);
  const csvFilePath = "csv-sample.csv";
  fs.writeFileSync(csvFilePath, csvContent);
  await page.goto("http://localhost:8080/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await page.getByRole("button", { name: "Upload CSV" }).click();
  await page.getByLabel("Contacts CSV file for import").click();
  await page
    .getByLabel("Contacts CSV file for import")
    .setInputFiles(csvFilePath);
  await page.getByRole("button", { name: "Submit" }).click();
  for (const row of csvData.slice(1)) {
    for (const name of row) {
      if (name) {
        await expect(page.getByRole("cell", { name })).toBeVisible();
      }
    }
  }
  const downloadPromise = page.waitForEvent("download");
  await page.getByRole("button", { name: "Download CSV" }).click();
  const download = await downloadPromise;
  const fn = download.suggestedFilename();
  expect(fn).toBe("contacts.csv");
  await download.saveAs(fn);
  const records = parse(fs.readFileSync(fn));
  expect(records).toEqual([
    csvData[0],
    ["Bill J", "bill@bill.com", "friends", "from school"],
    ...csvData.slice(1),
  ]);
  fs.rmSync(csvFilePath);
  fs.rmSync(fn);
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
  await sleep(300);
  // need to use mailpit api here because for some reason the mailpit GUI
  // isn't working well with Playwright in CI / GitHub Actions
  const res = await (
    await fetch("http://localhost:8025/api/v1/messages")
  ).json();
  const msgSummary = res.messages[0];
  expect(msgSummary.To[0].Name).toBe("Bill B");
  expect(msgSummary.To[0].Address).toBe("bill@b.com");
  const msg = await (
    await fetch(`http://localhost:8025/api/v1/message/${msgSummary.ID}`)
  ).json();
  expect(msg.Subject).toBe("My Test");
  expect(msg.Text).toContain("Hi there");
  expect(msg.HTML).toContain("Hi there");
  const $ = cheerio.load(msg.HTML);
  // test user update of contact info
  const updateContactLink = $(`a:contains("update your contact info")`).attr(
    "href",
  );
  const unsubscribeLink = $(`a:contains("No more updates please")`).attr(
    "href",
  );
  if (!updateContactLink) {
    fail("update contact link not found");
  }
  await page.goto(updateContactLink);
  await expect(
    page.getByRole("heading", { name: "Update Contact Info" }),
  ).toBeVisible();
  await page.getByLabel("Name").click();
  await page.getByLabel("Name").fill("Bill J");
  await page.locator('input[name="email"]').click();
  await page.locator('input[name="email"]').press("ArrowLeft");
  await page.locator('input[name="email"]').press("ArrowLeft");
  await page.locator('input[name="email"]').press("ArrowLeft");
  await page.locator('input[name="email"]').press("ArrowLeft");
  await page.locator('input[name="email"]').fill("bill@j.com");
  await page.getByRole("button", { name: "Update" }).click();
  await expect(
    page.getByRole("heading", { name: "Your contact info has been" }),
  ).toBeVisible();
  await expect(page.getByText("Name: Bill J")).toBeVisible();
  await expect(page.getByText("Email: bill@j.com")).toBeVisible();
  await page.getByRole("link", { name: "Made a mistake?" }).click();
  await page.getByLabel("Name").click();
  await page.getByLabel("Name").fill("Bill K");
  await page.getByRole("button", { name: "Update" }).click();
  await expect(
    page.getByRole("heading", { name: "Your contact info has been" }),
  ).toBeVisible();
  await expect(page.getByText("Name: Bill K")).toBeVisible();
  await page.goto("http://localhost:8080/contacts");
  await page.goto("http://localhost:8080/contacts");
  await expect(page.getByRole("cell", { name: "Bill K" })).toBeVisible();
  await expect(page.getByRole("cell", { name: "bill@j.com" })).toBeVisible();
  // also check that unsubscribe link works
  if (!unsubscribeLink) {
    fail("unsubscribe link not found");
  }
  await page.goto(unsubscribeLink);
  await page.locator("#unsubscribe-box").check();
  await page.getByRole("button", { name: "Unsubscribe" }).click();
  await expect(
    page.getByRole("heading", { name: "Unsubscribed" }),
  ).toBeVisible();
  await page.goto("http://localhost:8080/");
  await page.getByRole("link", { name: "Contacts" }).click();
  await expect(page.getByRole("cell", { name: "Bill K" })).not.toBeVisible();
  await expect(
    page.getByRole("cell", { name: "bill@j.com" }),
  ).not.toBeVisible();
});

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
