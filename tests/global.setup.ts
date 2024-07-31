import { test as setup } from "@playwright/test";
import { execSync } from "node:child_process";

setup("create new database", async ({}) => {
  execSync("cabal run clean-db");
  await fetch(`${process.env.MAILPIT_DOMAIN}/api/v1/messages`, {
    method: "DELETE",
  });
});
