import { readFileSync, writeFileSync } from 'node:fs';

import { padRecord } from '../codec/fixedWidth.ts';

/**
 * Fixed-width dataset files. The repository's ASCII copies are newline
 * separated and sometimes omit trailing filler, so lines are space padded to
 * the record length on read. Single-byte (latin1) encoding is used so that
 * every byte offset in the copybooks maps to exactly one character.
 */
export function readRecordLines(path: string, recordLength: number): string[] {
  return readFileSync(path, 'latin1')
    .split('\n')
    .map((line) => line.replace(/\r$/, ''))
    .filter((line) => line.trim().length > 0)
    .map((line) => padRecord(line, recordLength));
}

export function writeRecordLines(path: string, lines: readonly string[]): void {
  const body = lines.length === 0 ? '' : `${lines.join('\n')}\n`;
  writeFileSync(path, body, 'latin1');
}
