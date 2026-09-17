/**
 * `USRSEC` — the user security KSDS the online programs read with
 * `EXEC CICS READ DATASET('USRSEC')`.
 *
 * The shipped copy of the file (`AWS.M2.CARDDEMO.USRSEC.PS`) is an unblocked
 * 80 byte EBCDIC dataset with no record separators, so the records are cut by
 * length and translated with the code page 037 table before the `CSUSR01Y`
 * codec decodes them.
 */

import { readFileSync } from "node:fs";
import { join } from "node:path";

import { userSecurityCodec, type UserSecurityRecord } from "@carddemo/domain";
import { repoRoot } from "@carddemo/vsam";

/** `EXEC CICS READ` response codes the signon and menu programs branch on. */
export const CicsResp = {
  normal: 0,
  fileNotFound: 12,
  notFound: 13,
  ioError: 17,
} as const;

export type CicsRespCode = (typeof CicsResp)[keyof typeof CicsResp];

export interface UserSecurityReadResult {
  readonly resp: CicsRespCode;
  readonly record?: UserSecurityRecord;
}

/** The read interface the handlers need; any store can stand in for tests. */
export interface UserSecurityStore {
  read(userId: string): UserSecurityReadResult;
}

/** `SEC-USER-DATA` record length. */
export const userSecurityRecordLength = 80;

/** `SEC-USR-ID` key length, used to pad the `RIDFLD`. */
export const userIdLength = 8;

export const userSecurityDataset = join(
  repoRoot,
  "app",
  "data",
  "EBCDIC",
  "AWS.M2.CARDDEMO.USRSEC.PS",
);

// prettier-ignore
const codePage037 =
  "\u0000\u0001\u0002\u0003\u009c\u0009\u0086\u007f\u0097\u008d\u008e\u000b\u000c\u000d\u000e\u000f" +
  "\u0010\u0011\u0012\u0013\u009d\u0085\u0008\u0087\u0018\u0019\u0092\u008f\u001c\u001d\u001e\u001f" +
  "\u0080\u0081\u0082\u0083\u0084\u000a\u0017\u001b\u0088\u0089\u008a\u008b\u008c\u0005\u0006\u0007" +
  "\u0090\u0091\u0016\u0093\u0094\u0095\u0096\u0004\u0098\u0099\u009a\u009b\u0014\u0015\u009e\u001a" +
  "\u0020\u00a0\u00e2\u00e4\u00e0\u00e1\u00e3\u00e5\u00e7\u00f1\u00a2\u002e\u003c\u0028\u002b\u007c" +
  "\u0026\u00e9\u00ea\u00eb\u00e8\u00ed\u00ee\u00ef\u00ec\u00df\u0021\u0024\u002a\u0029\u003b\u00ac" +
  "\u002d\u002f\u00c2\u00c4\u00c0\u00c1\u00c3\u00c5\u00c7\u00d1\u00a6\u002c\u0025\u005f\u003e\u003f" +
  "\u00f8\u00c9\u00ca\u00cb\u00c8\u00cd\u00ce\u00cf\u00cc\u0060\u003a\u0023\u0040\u0027\u003d\u0022" +
  "\u00d8\u0061\u0062\u0063\u0064\u0065\u0066\u0067\u0068\u0069\u00ab\u00bb\u00f0\u00fd\u00fe\u00b1" +
  "\u00b0\u006a\u006b\u006c\u006d\u006e\u006f\u0070\u0071\u0072\u00aa\u00ba\u00e6\u00b8\u00c6\u00a4" +
  "\u00b5\u007e\u0073\u0074\u0075\u0076\u0077\u0078\u0079\u007a\u00a1\u00bf\u00d0\u00dd\u00de\u00ae" +
  "\u005e\u00a3\u00a5\u00b7\u00a9\u00a7\u00b6\u00bc\u00bd\u00be\u005b\u005d\u00af\u00a8\u00b4\u00d7" +
  "\u007b\u0041\u0042\u0043\u0044\u0045\u0046\u0047\u0048\u0049\u00ad\u00f4\u00f6\u00f2\u00f3\u00f5" +
  "\u007d\u004a\u004b\u004c\u004d\u004e\u004f\u0050\u0051\u0052\u00b9\u00fb\u00fc\u00f9\u00fa\u00ff" +
  "\u005c\u00f7\u0053\u0054\u0055\u0056\u0057\u0058\u0059\u005a\u00b2\u00d4\u00d6\u00d2\u00d3\u00d5" +
  "\u0030\u0031\u0032\u0033\u0034\u0035\u0036\u0037\u0038\u0039\u00b3\u00db\u00dc\u00d9\u00da\u009f";

function fromEbcdic(bytes: Buffer): string {
  let text = "";
  for (const byte of bytes) {
    text += codePage037.charAt(byte);
  }
  return text;
}

/** True for the shipped EBCDIC dataset, false for an ASCII copy of it. */
function looksLikeEbcdic(bytes: Buffer): boolean {
  return bytes.some((byte) => byte > 0x7f);
}

/** Reads the dataset and decodes it with the `CSUSR01Y` layout. */
export function loadUserSecurityRecords(path = userSecurityDataset): UserSecurityRecord[] {
  const bytes = readFileSync(path);
  const text = looksLikeEbcdic(bytes) ? fromEbcdic(bytes) : bytes.toString("latin1");
  const records: UserSecurityRecord[] = [];
  for (let offset = 0; offset + userSecurityRecordLength <= text.length; ) {
    const line = text.slice(offset, offset + userSecurityRecordLength);
    offset += userSecurityRecordLength;
    if (text.charAt(offset) === "\n") {
      offset += 1;
    }
    if (line.trim().length > 0) {
      records.push(userSecurityCodec.decode(line));
    }
  }
  return records;
}

/** In-memory `USRSEC` file, keyed by `SEC-USR-ID` like the VSAM KSDS. */
export class UserSecurityFile implements UserSecurityStore {
  private readonly records = new Map<string, UserSecurityRecord>();

  constructor(records: readonly UserSecurityRecord[]) {
    for (const record of records) {
      this.records.set(record.secUsrId.padEnd(userIdLength, " "), record);
    }
  }

  static open(path = userSecurityDataset): UserSecurityFile {
    return new UserSecurityFile(loadUserSecurityRecords(path));
  }

  read(userId: string): UserSecurityReadResult {
    const record = this.records.get(userId.padEnd(userIdLength, " ").slice(0, userIdLength));
    return record === undefined ? { resp: CicsResp.notFound } : { resp: CicsResp.normal, record };
  }

  toArray(): UserSecurityRecord[] {
    return [...this.records.values()];
  }
}
