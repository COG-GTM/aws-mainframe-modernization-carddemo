import type { Server } from "node:http";
import type { AddressInfo } from "node:net";

import { openCardFile } from "@carddemo/vsam";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { createCardsApp } from "./api.js";
import { emptyCardListInput, type CardListResult } from "./card-list.js";

const cardFile = openCardFile();
cardFile.openFile();

const app = createCardsApp({ cardFile });
let server: Server;

beforeAll(async () => {
  server = await new Promise<Server>((resolve) => {
    const listening = app.listen(0, () => resolve(listening));
  });
});

afterAll(() => {
  server.close();
});

describe("cards API", () => {
  it("serves the card list handler", async () => {
    const { port } = server.address() as AddressInfo;
    const response = await fetch(`http://127.0.0.1:${port}/cards/list`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ aid: "ENTER", screen: emptyCardListInput() }),
    });

    expect(response.status).toBe(200);
    const result = (await response.json()) as CardListResult;
    expect(result.screen?.pageNo).toBe("1");
    expect(result.screen?.rows).toHaveLength(7);
  });
});
