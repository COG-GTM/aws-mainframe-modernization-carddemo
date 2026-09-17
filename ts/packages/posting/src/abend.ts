/**
 * Abend handling shared by the batch programs.
 *
 * `Z-DISPLAY-IO-STATUS` renders the two byte file status as four characters and
 * `Z-ABEND-PROGRAM` calls `CEE3ABD` with abend code 999; here the abend raises
 * an {@link AbendError} instead of terminating the process.
 */

import { FileStatus, type FileStatusCode } from "@carddemo/vsam";

export const ABEND_CODE = 999;

/** `IO-STATUS-04`: `00nn` for numeric statuses, `9nnn` for the `9x` family. */
export function formatIoStatus(status: string): string {
  const first = status.charAt(0);
  const second = status.charAt(1);
  if (!/^[0-9]{2}$/.test(status) || first === "9") {
    return `${first}${String(second.charCodeAt(0)).padStart(3, "0")}`;
  }
  return `00${status}`;
}

export class AbendError extends Error {
  readonly abcode = ABEND_CODE;

  constructor(
    readonly fileName: string,
    readonly status: FileStatusCode,
    readonly operation: string,
  ) {
    super(
      `${operation} ${fileName} failed, FILE STATUS IS: NNNN${formatIoStatus(status)}`,
    );
    this.name = "AbendError";
  }
}

export type FileStatusGuard = (
  fileName: string,
  operation: string,
  status: FileStatusCode,
  display: string,
  accepted?: readonly FileStatusCode[],
) => void;

/** Mirrors the `IF APPL-AOK ... ELSE DISPLAY / ABEND` blocks around every I/O verb. */
export function createFileStatusGuard(
  log: (line: string) => void,
): FileStatusGuard {
  return (
    fileName,
    operation,
    status,
    display,
    accepted = [FileStatus.ok],
  ): void => {
    if (accepted.includes(status)) {
      return;
    }
    log(display);
    log(`FILE STATUS IS: NNNN${formatIoStatus(status)}`);
    log("ABENDING PROGRAM");
    throw new AbendError(fileName, status, operation);
  };
}
