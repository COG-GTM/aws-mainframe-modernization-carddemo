/**
 * Shared plumbing for the ported batch reader programs: the display sink each
 * COBOL `DISPLAY` writes to, and the abend raised by `CALL 'CEE3ABD'`.
 */

export type DisplayLine = (line: string) => void;

export interface ProgramOptions {
  /** Input dataset; defaults to the matching file in `app/data/ASCII`. */
  readonly path?: string;
  readonly display?: DisplayLine;
}

export interface ProgramResult {
  readonly recordsRead: number;
}

export const ABEND_CODE = 999;

export class AbendError extends Error {
  constructor(readonly abendCode: number = ABEND_CODE) {
    super(`program abended with code ${abendCode}`);
    this.name = "AbendError";
  }
}

export const consoleDisplay: DisplayLine = (line: string): void => {
  process.stdout.write(`${line}\n`);
};
