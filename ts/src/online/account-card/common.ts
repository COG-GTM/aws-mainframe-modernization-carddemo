/**
 * Helpers shared by the Session 7 account/card programs (COACTVWC, COACTUPC,
 * COCRDLIC, COCRDSLC, COCRDUPC). Local to this area so S8/S9 are unaffected;
 * candidates for upstreaming into framework/helpers.ts.
 */
import { CarddemoCommareaLayout } from '../../common/copybooks/index.js';
import { LATIN1_TO_CP037 } from '../../common/codecs/ebcdic.js';
import { AbendDataLayout } from '../../common/copybooks/index.js';
import { CarddemoCommarea, type CicsContext } from '../framework/index.js';

/** 01 WS-COMMAREA PIC X(2000): the RETURN TRANSID commarea of every program here. */
export const WS_COMMAREA_LEN = 2000;

/** INITIALIZE CARDDEMO-COMMAREA (alphanumerics to spaces, numerics to zeros). */
export function initializedCommarea(): CarddemoCommarea {
  return new CarddemoCommarea(CarddemoCommareaLayout.serialize({}));
}

/** MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1 : len) TO WS-THIS-PROGCOMMAREA. */
export function programCommarea(commarea: Buffer | undefined, len: number): Buffer {
  const out = Buffer.alloc(len, 0x20);
  const start = CarddemoCommarea.LENGTH;
  if (commarea && commarea.length > start) commarea.copy(out, 0, start, Math.min(commarea.length, start + len));
  return out;
}

/** COMMON-RETURN: WS-COMMAREA (INITIALIZEd = spaces) <- CARDDEMO-COMMAREA + WS-THIS-PROGCOMMAREA. */
export function buildWsCommarea(cdemo: CarddemoCommarea, prog: Buffer): Buffer {
  const out = Buffer.alloc(WS_COMMAREA_LEN, 0x20);
  cdemo.buffer.copy(out, 0);
  prog.copy(out, CarddemoCommarea.LENGTH, 0, Math.min(prog.length, WS_COMMAREA_LEN - CarddemoCommarea.LENGTH));
  return out;
}

/** MOVE S9(9) COMP TO PIC X(10): unsigned 9-digit DISPLAY, left-justified. */
export const respText = (n: number): string => String(Math.abs(n) % 1e9).padStart(9, '0') + ' ';

/** WS-FILE-ERROR-MESSAGE ('File Error: ' op ' on ' file ' returned RESP ' resp ',RESP2 ' resp2). */
export function fileErrorMessage(op: string, file: string, resp: number, resp2: number): string {
  const pad = (s: string, n: number): string => s.padEnd(n, ' ').slice(0, n);
  return `File Error: ${pad(op, 8)} on ${pad(file, 9)} returned RESP ${respText(resp)},RESP2 ${respText(resp2)}     `;
}

/**
 * Value of a zoned DISPLAY item holding arbitrary bytes, as z/OS PACK sees it:
 * each byte contributes its CP037 low nibble (LOW-VALUES and SPACES give 0).
 * Used for numeric comparisons such as `CC-ACCT-ID-N = ZEROS` on unedited input.
 */
export function zonedValue(text: string): bigint {
  let v = 0n;
  for (const ch of text) {
    const code = ch.charCodeAt(0) & 0xff;
    const nib = (LATIN1_TO_CP037[code] ?? 0) & 0x0f;
    v = v * 10n + BigInt(nib > 9 ? nib % 10 : nib);
  }
  return v;
}

/** `x IS NUMERIC` for an unsigned PIC X / PIC 9 DISPLAY item. */
export const isNumeric = (s: string): boolean => /^[0-9]+$/.test(s);

/** ABEND-ROUTINE: SEND ABEND-DATA, HANDLE ABEND CANCEL, ABEND ABCODE('9999'). */
export function abendRoutine(ctx: CicsContext, program: string, abendCode = '', abendMsg = ''): never {
  const msg = abendMsg === '' || /^\0*$/.test(abendMsg) ? 'UNEXPECTED ABEND OCCURRED.' : abendMsg;
  const data = AbendDataLayout.serialize({ abendCode, abendCulprit: program, abendReason: '', abendMsg: msg });
  ctx.abendHandler = undefined;
  ctx.sendText(data.toString('latin1'));
  return ctx.abend('9999');
}

/** EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE). */
export function installAbendRoutine(ctx: CicsContext, program: string): void {
  ctx.abendHandler = () => abendRoutine(ctx, program);
}

/** WHEN OTHER -> SEND-PLAIN-TEXT 'UNEXPECTED DATA SCENARIO' + EXEC CICS RETURN. */
export function sendPlainText(ctx: CicsContext, msg: string): never {
  ctx.sendText(msg.padEnd(75, ' ').slice(0, 75));
  return ctx.return();
}
