/**
 * COBOL file status codes returned by the VSAM-equivalent stores.
 */

export const FileStatus = {
  ok: "00",
  endOfFile: "10",
  duplicateKey: "22",
  notFound: "23",
  boundaryViolation: "24",
  fileNotOpen: "42",
  openFailed: "35",
} as const;

export type FileStatusCode = (typeof FileStatus)[keyof typeof FileStatus];

export interface IoResult<T> {
  readonly status: FileStatusCode;
  readonly record?: T;
}

export function ok<T>(record: T): IoResult<T> {
  return { status: FileStatus.ok, record };
}

export function failure<T>(status: FileStatusCode): IoResult<T> {
  return { status };
}

export class FileStatusError extends Error {
  constructor(
    readonly fileName: string,
    readonly status: FileStatusCode,
    operation: string,
  ) {
    super(`${operation} on ${fileName} failed with file status ${status}`);
    this.name = "FileStatusError";
  }
}
