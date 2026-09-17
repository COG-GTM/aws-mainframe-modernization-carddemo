import { copyFileSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { dataFiles } from "@carddemo/vsam";

import type { OnlineContext } from "./context.js";
import { createOnlineContext } from "./context.js";
import { InMemoryReportRequestQueue } from "./report-queue.js";

/**
 * A context over copies of the shipped ASCII data: `dailytran.txt` carries
 * the same 350 byte transaction layout as `TRANSACT`, and the copies keep
 * writes out of the repository.
 */
export function testContext(now = new Date("2024-05-17T10:20:30")): {
  context: OnlineContext;
  reports: InMemoryReportRequestQueue;
} {
  const dir = mkdtempSync(join(tmpdir(), "carddemo-online-"));
  const transactionFile = join(dir, "transact.txt");
  const accountFile = join(dir, "acctdata.txt");
  copyFileSync(dataFiles.dailytran, transactionFile);
  copyFileSync(dataFiles.acctdata, accountFile);

  const reports = new InMemoryReportRequestQueue();
  const context = createOnlineContext({
    transactionFile,
    accountFile,
    reports,
    now: () => now,
    persist: false,
  });
  return { context, reports };
}
