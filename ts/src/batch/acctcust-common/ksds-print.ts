/**
 * The shared shape of CBACT02C / CBACT03C / CBCUS01C: OPEN INPUT a KSDS with
 * ACCESS SEQUENTIAL, READ ... INTO the copybook record until status '10',
 * DISPLAY each record, CLOSE. Any other status DISPLAYs an error, the file
 * status (9910) and abends (9999).
 */
import { IndexedFile } from '../../common/vsam/cobol-file.js';
import { abendProgram, displayIoStatus, resolveCatalog, runProgram, type BatchRunOptions, type BatchRunResult, type Sysout } from './support.js';

export interface KsdsPrintSpec {
  program: string;
  /** SELECT ... ASSIGN TO (DD name). */
  assign: string;
  recordLength: number;
  /** 1000-...-GET-NEXT also DISPLAYs the record (CBACT03C, CBCUS01C), so each record is shown twice. */
  displayInGetNext: boolean;
  openError: string;
  readError: string;
  closeError: string;
}

export interface KsdsPrintResult extends BatchRunResult {
  /** Records read (copies of the INTO area), in key order. */
  records: Buffer[];
}

export function runKsdsPrint(spec: KsdsPrintSpec, opts: BatchRunOptions = {}): KsdsPrintResult {
  const records: Buffer[] = [];
  return runProgram(
    opts,
    (out) => {
      const file = new IndexedFile(resolveCatalog(opts), spec.assign, { access: 'SEQUENTIAL' });
      // The INTO area is WORKING-STORAGE: it keeps its last content after a failed READ.
      let area: Buffer = Buffer.alloc(spec.recordLength, 0x20);
      out.display(`START OF EXECUTION OF PROGRAM ${spec.program}`);
      checkOk(out, file.open('INPUT').status, spec.openError);
      let endOfFile = false;
      while (!endOfFile) {
        const res = file.readNext();
        if (res.status === '00') {
          area = fit(res.record!, spec.recordLength);
          records.push(Buffer.from(area));
          if (spec.displayInGetNext) out.display(area);
        } else if (res.status === '10') {
          endOfFile = true;
        } else {
          out.display(spec.readError);
          displayIoStatus(out, res.status);
          abendProgram(out);
        }
        if (!endOfFile) out.display(area);
      }
      checkOk(out, file.close().status, spec.closeError);
      out.display(`END OF EXECUTION OF PROGRAM ${spec.program}`);
      return { records };
    },
    () => ({ records }),
  );
}

function checkOk(out: Sysout, status: string, message: string): void {
  if (status === '00') return;
  out.display(message);
  displayIoStatus(out, status);
  abendProgram(out);
}

/** READ INTO: MOVE of the record to the INTO item (truncate / space pad). */
export function fit(record: Uint8Array, length: number): Buffer {
  const b = Buffer.alloc(length, 0x20);
  Buffer.from(record).copy(b, 0, 0, Math.min(length, record.length));
  return b;
}
