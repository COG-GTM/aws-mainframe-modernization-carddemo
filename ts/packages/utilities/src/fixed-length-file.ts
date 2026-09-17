/**
 * `RECORDING MODE F` file access.
 *
 * The export file holds `COMP` fields, whose bytes can include the line feed
 * character, so it is read and written as fixed-length blocks rather than as
 * delimited lines like the `@carddemo/vsam` sequential helpers do.
 */

import { readFileSync, writeFileSync } from "node:fs";

const LATIN1 = "latin1";

export function readFixedLengthRecords(path: string, recordLength: number): string[] {
  const content = readFileSync(path, LATIN1);
  const records: string[] = [];
  for (let offset = 0; offset + recordLength <= content.length; offset += recordLength) {
    records.push(content.slice(offset, offset + recordLength));
  }
  return records;
}

export function writeFixedLengthRecords(
  path: string,
  records: readonly string[],
  recordLength: number,
): void {
  const content = records
    .map((record) => record.padEnd(recordLength, " ").slice(0, recordLength))
    .join("");
  writeFileSync(path, content, LATIN1);
}
