import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { FileStatus } from "@carddemo/vsam";
import { describe, expect, it } from "vitest";

import { createUserAdminApp } from "./api.js";
import { PgmContext } from "./commarea.js";
import type { UserListScreen } from "./cousr00.js";
import { openUserSecurityFile, writeUsrsecAscii } from "./usrsec-file.js";

const listen = async (): Promise<{ url: string; close: () => Promise<void> }> => {
  const path = writeUsrsecAscii(join(mkdtempSync(join(tmpdir(), "usrsec-")), "usrsec.txt"));
  const file = openUserSecurityFile(path);
  expect(file.openFile()).toBe(FileStatus.ok);
  const server = createUserAdminApp(file, { persist: false }).listen(0);
  await new Promise((resolve) => server.once("listening", resolve));
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("server did not bind a port");
  }
  return {
    url: `http://127.0.0.1:${String(address.port)}`,
    close: () =>
      new Promise<void>((resolve, reject) => {
        server.close((error) => {
          if (error) {
            reject(error);
          } else {
            resolve();
          }
        });
      }),
  };
};

describe("user administration API", () => {
  it("serves the first page of the user list", async () => {
    const { url, close } = await listen();
    try {
      const response = await fetch(`${url}/users/list`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ aid: "ENTER", commarea: { pgmContext: PgmContext.enter } }),
      });

      expect(response.status).toBe(200);
      const body = (await response.json()) as { screen: UserListScreen };
      expect(body.screen.rows[0]?.usrid).toBe("ADMIN001");
      expect(body.screen.pagenum).toBe(1);
    } finally {
      await close();
    }
  });
});
