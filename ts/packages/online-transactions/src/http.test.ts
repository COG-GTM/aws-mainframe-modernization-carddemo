import type { AddressInfo } from "node:net";

import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { emptyCommarea } from "./commarea.js";
import { createApp } from "./http.js";
import { AidKey } from "./screen.js";
import { testContext } from "./test-support.js";
import { emptyTransactionListScreen } from "./transaction-list.js";

const { context } = testContext();
const server = createApp(context).listen(0);

const baseUrl = (): string => `http://127.0.0.1:${(server.address() as AddressInfo).port}`;

beforeAll(
  () =>
    new Promise<void>((resolve) => {
      if (server.listening) {
        resolve();
        return;
      }
      server.once("listening", () => {
        resolve();
      });
    }),
);

afterAll(
  () =>
    new Promise<void>((resolve) => {
      server.close(() => {
        resolve();
      });
    }),
);

describe("express transport", () => {
  it("returns the screen the handler produced", async () => {
    const response = await fetch(`${baseUrl()}/transactions/list`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        screen: emptyTransactionListScreen(),
        aid: AidKey.enter,
        commarea: { ...emptyCommarea(), pgmContext: 1 },
      }),
    });

    expect(response.status).toBe(200);
    const body = (await response.json()) as { screen: { pageNum: string } };
    expect(body.screen.pageNum).toBe("1");
  });

  it("rejects a request without a valid aid key", async () => {
    const response = await fetch(`${baseUrl()}/transactions/list`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ screen: emptyTransactionListScreen(), aid: "PF99" }),
    });
    expect(response.status).toBe(400);
  });
});
