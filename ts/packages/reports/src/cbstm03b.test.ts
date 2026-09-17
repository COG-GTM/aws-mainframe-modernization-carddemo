import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { accountCodec, cardXrefCodec, customerCodec } from "@carddemo/domain";
import { FileStatus } from "@carddemo/vsam";
import { beforeAll, describe, expect, it } from "vitest";

import {
  M03B_DATA_LENGTH,
  M03bDdName,
  M03bOperation,
  newM03bArea,
  type Cbstm03b,
} from "./cbstm03b.js";
import { buildTrnxFile, loadPostedTransactions, openStatementIo } from "./datasets.js";
import { alphanumeric, displayDigits } from "./picture.js";
import { trnxCodec } from "./trnx.js";

let io: Cbstm03b;

beforeAll(() => {
  const trnxPath = join(mkdtempSync(join(tmpdir(), "carddemo-cbstm03b-")), "trnxfile.txt");
  buildTrnxFile(loadPostedTransactions(), trnxPath);
  io = openStatementIo(trnxPath);
});

describe("CBSTM03B", () => {
  it("reads TRNXFILE and XREFFILE sequentially until end of file", () => {
    const area = newM03bArea();
    for (const dd of [M03bDdName.trnxFile, M03bDdName.xrefFile]) {
      area.dd = dd;
      area.oper = M03bOperation.open;
      io.call(area);
      expect(area.rc).toBe(FileStatus.ok);

      let records = 0;
      area.oper = M03bOperation.read;
      for (;;) {
        io.call(area);
        if (area.rc === FileStatus.endOfFile) {
          break;
        }
        expect(area.rc).toBe(FileStatus.ok);
        expect(area.fldt).toHaveLength(M03B_DATA_LENGTH);
        records += 1;
      }
      expect(records).toBe(dd === M03bDdName.trnxFile ? 300 : 50);

      area.oper = M03bOperation.close;
      io.call(area);
      expect(area.rc).toBe(FileStatus.ok);
    }
  });

  it("reads CUSTFILE and ACCTFILE by the key length it is given", () => {
    const area = newM03bArea();
    area.dd = M03bDdName.xrefFile;
    area.oper = M03bOperation.open;
    io.call(area);
    area.oper = M03bOperation.read;
    io.call(area);
    const xref = cardXrefCodec.decode(area.fldt);

    area.dd = M03bDdName.custFile;
    area.oper = M03bOperation.open;
    io.call(area);
    area.oper = M03bOperation.readKeyed;
    area.key = alphanumeric(displayDigits(xref.xrefCustId, 9), 25);
    area.keyLength = 9;
    io.call(area);
    expect(area.rc).toBe(FileStatus.ok);
    expect(customerCodec.decode(area.fldt).custId).toBe(xref.xrefCustId);

    area.dd = M03bDdName.acctFile;
    area.oper = M03bOperation.open;
    io.call(area);
    area.oper = M03bOperation.readKeyed;
    area.key = alphanumeric(displayDigits(xref.xrefAcctId, 11), 25);
    area.keyLength = 11;
    io.call(area);
    expect(area.rc).toBe(FileStatus.ok);
    expect(accountCodec.decode(area.fldt).acctId).toBe(xref.xrefAcctId);
  });

  it("returns the file status of a missing key without throwing", () => {
    const area = newM03bArea();
    area.dd = M03bDdName.acctFile;
    area.oper = M03bOperation.open;
    io.call(area);
    area.oper = M03bOperation.readKeyed;
    area.key = alphanumeric("99999999999", 25);
    area.keyLength = 11;
    io.call(area);
    expect(area.rc).toBe(FileStatus.notFound);
  });

  it("leaves the area untouched for a DD name it does not know", () => {
    const area = newM03bArea();
    area.dd = "NOSUCHDD";
    area.oper = M03bOperation.open;
    area.rc = "77";
    io.call(area);
    expect(area.rc).toBe("77");
    expect(area.fldt).toBe("");
  });

  it("returns transaction records that decode through the TRNXFILE layout", () => {
    const area = newM03bArea();
    area.dd = M03bDdName.trnxFile;
    area.oper = M03bOperation.open;
    io.call(area);
    area.oper = M03bOperation.read;
    io.call(area);
    const record = trnxCodec.decode(area.fldt);
    expect(record.trnxCardNum).toMatch(/^\d{16}$/);
    expect(record.trnxId).toMatch(/^\d{16}$/);
    area.oper = M03bOperation.close;
    io.call(area);
  });
});
