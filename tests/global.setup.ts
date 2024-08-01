import { test as setup } from "@playwright/test";
import { execSync } from "node:child_process";

setup("create new database", async ({}) => {
  execSync("cabal run clean-db");
  await fetch(`localhost:8025/api/v1/messages`, {
    method: "DELETE",
  });
});
