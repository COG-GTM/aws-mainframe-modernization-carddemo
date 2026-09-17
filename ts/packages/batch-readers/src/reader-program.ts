/**
 * The read loop the CBACT/CBCUS reader programs share: `OPEN INPUT`, a
 * `PERFORM UNTIL END-OF-FILE` driving `READ ... INTO`, then `CLOSE`, with the
 * FILE STATUS checks and abend paths of the COBOL originals.
 */

import { FileStatus, type Ksds } from "@carddemo/vsam";

import { formatIoStatus } from "./io-status.js";
import { AbendError, type DisplayLine, type ProgramResult } from "./program.js";

export interface ReaderProgramSpec<T> {
  readonly programName: string;
  readonly openErrorMessage: string;
  readonly readErrorMessage: string;
  readonly closeErrorMessage: string;
  readonly store: Ksds<T>;
  /** `DISPLAY` issued by the `...-GET-NEXT` paragraph itself. */
  readonly displayInGetNext?: (record: T, display: DisplayLine) => void;
  /** `DISPLAY` issued by the main `PERFORM UNTIL` loop. */
  readonly displayInMainLoop: (record: T, display: DisplayLine) => void;
}

function abend(display: DisplayLine): never {
  display("ABENDING PROGRAM");
  throw new AbendError();
}

export function runReaderProgram<T>(
  spec: ReaderProgramSpec<T>,
  display: DisplayLine,
): ProgramResult {
  display(`START OF EXECUTION OF PROGRAM ${spec.programName}`);

  const openStatus = spec.store.openFile();
  if (openStatus !== FileStatus.ok) {
    display(spec.openErrorMessage);
    display(formatIoStatus(openStatus));
    abend(display);
  }

  let recordsRead = 0;
  let endOfFile = false;
  while (!endOfFile) {
    const result = spec.store.readNext();
    if (result.status === FileStatus.ok) {
      const record = result.record as T;
      recordsRead += 1;
      spec.displayInGetNext?.(record, display);
      spec.displayInMainLoop(record, display);
    } else if (result.status === FileStatus.endOfFile) {
      endOfFile = true;
    } else {
      display(spec.readErrorMessage);
      display(formatIoStatus(result.status));
      abend(display);
    }
  }

  const closeStatus = spec.store.close();
  if (closeStatus !== FileStatus.ok) {
    display(spec.closeErrorMessage);
    display(formatIoStatus(closeStatus));
    abend(display);
  }

  display(`END OF EXECUTION OF PROGRAM ${spec.programName}`);
  return { recordsRead };
}
