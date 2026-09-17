import request from "supertest";
import { describe, expect, it } from "vitest";

import { createCommarea, PgmContext, UserType } from "./commarea.js";
import { createApp } from "./http.js";
import { adminMenuProgram, mainMenuProgram, signonProgram } from "./programs.js";

const app = createApp({ now: () => new Date(2024, 4, 17, 9, 8, 7), applid: "CICSAPP1" });

describe("HTTP transport", () => {
  it("signs a user on", async () => {
    const response = await request(app)
      .post("/signon")
      .send({
        aid: "ENTER",
        commarea: createCommarea(),
        fields: { userid: "USER0001", passwd: "PASSWORD" },
      });
    expect(response.status).toBe(200);
    expect(response.body).toMatchObject({ kind: "transfer", program: mainMenuProgram });
  });

  it("carries the commarea into the admin menu", async () => {
    const signon = await request(app)
      .post("/signon")
      .send({
        aid: "ENTER",
        commarea: createCommarea(),
        fields: { userid: "ADMIN001", passwd: "PASSWORD" },
      });
    expect(signon.body).toMatchObject({ program: adminMenuProgram });

    const menu = await request(app)
      .post("/admin")
      .send({ aid: "ENTER", commarea: signon.body.commarea as unknown });
    expect(menu.body).toMatchObject({
      kind: "map",
      commarea: { userType: UserType.admin, pgmContext: PgmContext.reenter },
    });

    const routed = await request(app)
      .post("/admin")
      .send({ aid: "ENTER", commarea: menu.body.commarea as unknown, fields: { option: "1" } });
    expect(routed.body).toMatchObject({ kind: "transfer", program: "COUSR00C" });
  });

  it("sends an empty signon map when no commarea is posted", async () => {
    const response = await request(app).post("/signon").send({});
    expect(response.body).toMatchObject({ kind: "map", cursor: "userid" });
  });

  it("returns the main menu to signon on PF3", async () => {
    const response = await request(app)
      .post("/menu")
      .send({ aid: "pf3", commarea: createCommarea({ pgmContext: PgmContext.reenter }) });
    expect(response.body).toEqual({ kind: "transfer", program: signonProgram });
  });

  it("rejects an unknown aid key", async () => {
    const response = await request(app).post("/signon").send({ aid: "PF99" });
    expect(response.status).toBe(400);
    expect(response.body).toEqual({ error: "unknown aid key PF99" });
  });

  it("rejects a malformed commarea", async () => {
    const response = await request(app)
      .post("/menu")
      .send({ aid: "ENTER", commarea: { pgmContext: 7 } });
    expect(response.status).toBe(400);
  });
});
