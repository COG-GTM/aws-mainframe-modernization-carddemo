/**
 * The `USRSEC` dataset the COUSR programs read, browse and maintain, plus the
 * browse cursor `STARTBR` / `READNEXT` / `READPREV` / `ENDBR` need.
 *
 * `@carddemo/vsam` browses forward only, so the backward leg of the
 * `COUSR00C` paging is served from a key-ordered snapshot taken at `STARTBR`,
 * which is what a CICS browse gives the program anyway.
 */

import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import {
  userSecurityCodec,
  userSecurityKey,
  type UserSecurityRecord,
} from "@carddemo/domain";
import {
  FileStatus,
  Ksds,
  asciiDataDir,
  failure,
  ok,
  repoRoot,
  type FileStatusCode,
  type IoResult,
} from "@carddemo/vsam";

import { decodeEbcdic } from "./ebcdic.js";

/** Where the ASCII `USRSEC` extract lives alongside the other sample files. */
export const usrsecDataPath = join(asciiDataDir, "usrsec.txt");

/** The shipped sign-on users; the repository has no ASCII counterpart. */
export const usrsecEbcdicPath = join(
  repoRoot,
  "app",
  "data",
  "EBCDIC",
  "AWS.M2.CARDDEMO.USRSEC.PS",
);

const USRSEC_RECORD_LENGTH = 80;

/**
 * Transcode the shipped EBCDIC `USRSEC` dataset into a writable ASCII copy and
 * return its path, so callers work on the real sign-on users.
 */
export function writeUsrsecAscii(target: string, source = usrsecEbcdicPath): string {
  const text = decodeEbcdic(readFileSync(source));
  const lines: string[] = [];
  for (let offset = 0; offset < text.length; offset += USRSEC_RECORD_LENGTH) {
    lines.push(text.slice(offset, offset + USRSEC_RECORD_LENGTH));
  }
  writeFileSync(target, `${lines.join("\n")}\n`, "ascii");
  return target;
}

/** `MOVE LOW-VALUES TO SEC-USR-ID` — browse from the first key. */
export const lowValues = "";

/** `MOVE HIGH-VALUES TO SEC-USR-ID` — browse past the last key. */
export const highValues = "\uFFFF";

export const openUserSecurityFile = (path = usrsecDataPath): Ksds<UserSecurityRecord> =>
  new Ksds({ name: "USRSEC", path, codec: userSecurityCodec, keyOf: userSecurityKey });

export class UserSecurityBrowse {
  private snapshot: readonly UserSecurityRecord[] = [];
  private cursor = 0;

  constructor(private readonly file: Ksds<UserSecurityRecord>) {}

  /** `EXEC CICS STARTBR`, which defaults to `GTEQ`. */
  startBrowse(key: string): FileStatusCode {
    this.snapshot = this.file.toArray();
    const index = this.snapshot.findIndex((record) => userSecurityKey(record) >= key);
    if (index < 0) {
      this.cursor = this.snapshot.length;
      return FileStatus.notFound;
    }
    this.cursor = index;
    return FileStatus.ok;
  }

  readNext(): IoResult<UserSecurityRecord> {
    const record = this.snapshot[this.cursor];
    if (record === undefined) {
      return failure(FileStatus.endOfFile);
    }
    this.cursor += 1;
    return ok(record);
  }

  readPrev(): IoResult<UserSecurityRecord> {
    const record = this.cursor < 0 ? undefined : this.snapshot[this.cursor];
    if (record === undefined) {
      return failure(FileStatus.endOfFile);
    }
    this.cursor -= 1;
    return ok(record);
  }

  endBrowse(): void {
    this.snapshot = [];
    this.cursor = 0;
  }
}
