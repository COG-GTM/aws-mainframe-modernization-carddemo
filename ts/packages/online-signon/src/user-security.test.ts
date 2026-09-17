import { describe, expect, it } from "vitest";

import { CicsResp, UserSecurityFile, loadUserSecurityRecords } from "./user-security.js";

describe("USRSEC", () => {
  const users = UserSecurityFile.open();

  it("decodes the shipped EBCDIC dataset", () => {
    const records = loadUserSecurityRecords();
    expect(records).toHaveLength(10);
    expect(records[0]).toEqual({
      secUsrId: "ADMIN001",
      secUsrFname: "MARGARET",
      secUsrLname: "GOLD",
      secUsrPwd: "PASSWORD",
      secUsrType: "A",
    });
  });

  it("reads a record by key", () => {
    const result = users.read("USER0001");
    expect(result.resp).toBe(CicsResp.normal);
    expect(result.record?.secUsrType).toBe("U");
  });

  it("reports NOTFND for an unknown key", () => {
    expect(users.read("NOSUCH").resp).toBe(CicsResp.notFound);
  });
});
