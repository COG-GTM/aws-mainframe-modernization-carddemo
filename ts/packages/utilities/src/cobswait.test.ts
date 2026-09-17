import { describe, expect, it } from "vitest";

import { MAX_WAIT_CENTISECONDS, cobswait, parseWaitParm } from "./cobswait.js";

describe("parseWaitParm", () => {
  it("reads the SYSIN parm as centiseconds", () => {
    expect(parseWaitParm("00000150")).toEqual({
      centiseconds: 150,
      milliseconds: 1500,
      returnCode: 0,
    });
    expect(parseWaitParm("25      ").centiseconds).toBe(25);
  });

  it("treats a blank parm as no wait", () => {
    expect(parseWaitParm("        ").centiseconds).toBe(0);
  });

  it("rejects a non numeric parm", () => {
    expect(parseWaitParm("abc").returnCode).toBe(12);
  });

  it("stops at the width of MVSWAIT-TIME", () => {
    expect(parseWaitParm(String(MAX_WAIT_CENTISECONDS)).returnCode).toBe(0);
    expect(parseWaitParm("999999999").centiseconds).toBe(99_999_999);
  });
});

describe("cobswait", () => {
  it("waits for the requested time", async () => {
    const started = Date.now();
    const result = await cobswait("00000005");
    expect(result.returnCode).toBe(0);
    expect(Date.now() - started).toBeGreaterThanOrEqual(40);
  });
});
