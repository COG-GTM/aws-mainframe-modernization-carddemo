/**
 * `CBSTM03B` — the file handling subroutine `CBSTM03A` calls.
 *
 * The COBOL contract is a single linkage area carrying the DD name, a one
 * character operation code and a two character return code taken from the file
 * status of the addressed file; the record read is returned in the `X(1000)`
 * data area. That contract is preserved here: {@link Cbstm03b.call} mutates the
 * {@link M03bArea} it is given and never throws.
 */

import type {
  AccountRecord,
  CardXrefRecord,
  CustomerRecord,
  RecordCodec,
} from "@carddemo/domain";
import { FileStatus, type FileStatusCode, type Ksds } from "@carddemo/vsam";

import type { TrnxRecord } from "./trnx.js";

/** `LK-M03B-OPER` condition names. */
export const M03bOperation = {
  open: "O",
  close: "C",
  read: "R",
  readKeyed: "K",
  write: "W",
  rewrite: "Z",
} as const;

export type M03bOperationCode = (typeof M03bOperation)[keyof typeof M03bOperation];

/** DD names `CBSTM03B` dispatches on; any other name is a no-op (`9999-GOBACK`). */
export const M03bDdName = {
  trnxFile: "TRNXFILE",
  xrefFile: "XREFFILE",
  custFile: "CUSTFILE",
  acctFile: "ACCTFILE",
} as const;

export type M03bDdNameValue = (typeof M03bDdName)[keyof typeof M03bDdName];

/** `LK-M03B-FLDT PIC X(1000)`. */
export const M03B_DATA_LENGTH = 1000;

/** `LK-M03B-AREA`. */
export interface M03bArea {
  /** `LK-M03B-DD PIC X(08)`. */
  dd: string;
  /** `LK-M03B-OPER PIC X(01)`. */
  oper: M03bOperationCode;
  /** `LK-M03B-RC PIC X(02)`, set from the file status of the addressed file. */
  rc: string;
  /** `LK-M03B-KEY PIC X(25)`. */
  key: string;
  /** `LK-M03B-KEY-LN PIC S9(4)`. */
  keyLength: number;
  /** `LK-M03B-FLDT PIC X(1000)`. */
  fldt: string;
}

export function newM03bArea(): M03bArea {
  return { dd: "", oper: M03bOperation.read, rc: "00", key: "", keyLength: 0, fldt: "" };
}

/** A store plus the codec used to render its records into `LK-M03B-FLDT`. */
export interface Cbstm03bFile<T> {
  readonly store: Ksds<T>;
  readonly codec: RecordCodec<T>;
}

/** The four FDs declared by `CBSTM03B`. */
export interface Cbstm03bFiles {
  /** `TRNXFILE`, `ACCESS MODE IS SEQUENTIAL`. */
  readonly trnxFile: Cbstm03bFile<TrnxRecord>;
  /** `XREFFILE`, `ACCESS MODE IS SEQUENTIAL`. */
  readonly xrefFile: Cbstm03bFile<CardXrefRecord>;
  /** `CUSTFILE`, `ACCESS MODE IS RANDOM`, keyed on `FD-CUST-ID PIC X(09)`. */
  readonly custFile: Cbstm03bFile<CustomerRecord>;
  /** `ACCTFILE`, `ACCESS MODE IS RANDOM`, keyed on `FD-ACCT-ID PIC 9(11)`. */
  readonly acctFile: Cbstm03bFile<AccountRecord>;
}

interface ReadOutcome {
  readonly status: FileStatusCode;
  readonly data?: string;
}

interface FileHandle {
  readonly openFile: () => FileStatusCode;
  readonly close: () => FileStatusCode;
  readonly read: (() => ReadOutcome) | undefined;
  readonly readKeyed: ((key: string) => ReadOutcome) | undefined;
  status: FileStatusCode;
}

function encodeOutcome<T>(file: Cbstm03bFile<T>, status: FileStatusCode, record?: T): ReadOutcome {
  if (record === undefined) {
    return { status };
  }
  return { status, data: file.codec.encode(record).padEnd(M03B_DATA_LENGTH, " ") };
}

/** `ORGANIZATION IS INDEXED, ACCESS MODE IS SEQUENTIAL`: `OPEN` positions at the first record. */
function sequentialHandle<T>(file: Cbstm03bFile<T>): FileHandle {
  return {
    openFile: (): FileStatusCode => {
      const status = file.store.openFile();
      return status === FileStatus.ok ? file.store.startBrowse() : status;
    },
    close: (): FileStatusCode => file.store.close(),
    read: (): ReadOutcome => {
      const result = file.store.readNext();
      return encodeOutcome(file, result.status, result.record);
    },
    readKeyed: undefined,
    status: FileStatus.ok,
  };
}

/** `ACCESS MODE IS RANDOM`: only `READ ... KEY` is supported. */
function keyedHandle<T>(file: Cbstm03bFile<T>): FileHandle {
  return {
    openFile: (): FileStatusCode => file.store.openFile(),
    close: (): FileStatusCode => file.store.close(),
    read: undefined,
    readKeyed: (key: string): ReadOutcome => {
      const result = file.store.read(key);
      return encodeOutcome(file, result.status, result.record);
    },
    status: FileStatus.ok,
  };
}

export class Cbstm03b {
  private readonly handles: ReadonlyMap<string, FileHandle>;

  constructor(files: Cbstm03bFiles) {
    this.handles = new Map<string, FileHandle>([
      [M03bDdName.trnxFile, sequentialHandle(files.trnxFile)],
      [M03bDdName.xrefFile, sequentialHandle(files.xrefFile)],
      [M03bDdName.custFile, keyedHandle(files.custFile)],
      [M03bDdName.acctFile, keyedHandle(files.acctFile)],
    ]);
  }

  /**
   * `CALL 'CBSTM03B' USING WS-M03B-AREA`.
   *
   * Unknown DD names return without touching the area, and an operation the
   * addressed file does not support leaves the file status from the previous
   * request in `rc`, exactly as the COBOL fall-through to `xx00-EXIT` does.
   */
  call(area: M03bArea): void {
    const file = this.handles.get(area.dd.trim());
    if (file === undefined) {
      return;
    }

    switch (area.oper) {
      case M03bOperation.open:
        file.status = file.openFile();
        break;
      case M03bOperation.close:
        file.status = file.close();
        break;
      case M03bOperation.read:
        if (file.read !== undefined) {
          this.apply(area, file, file.read());
        }
        break;
      case M03bOperation.readKeyed:
        if (file.readKeyed !== undefined) {
          const key = area.key.slice(0, Math.max(area.keyLength, 0));
          this.apply(area, file, file.readKeyed(key));
        }
        break;
      case M03bOperation.write:
      case M03bOperation.rewrite:
        break;
    }

    area.rc = file.status;
  }

  private apply(area: M03bArea, file: FileHandle, outcome: ReadOutcome): void {
    file.status = outcome.status;
    if (outcome.data !== undefined) {
      area.fldt = outcome.data;
    }
  }
}
