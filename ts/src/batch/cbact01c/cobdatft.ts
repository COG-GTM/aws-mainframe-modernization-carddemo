/**
 * COBDATFT (app/asm/COBDATFT.asm): date re-formatting routine CALLed by
 * CBACT01C with the 80-byte CODATECN-REC (copybook CODATECN, DSECT COCDATFT).
 *
 *   COINTYPE '1' (YYYYMMDD in):   error if COINPDT+4 = '-' or COOUTYPE = '2';
 *                                 else COOUTDT(1:10) = YYYY-MM-DD.
 *   COINTYPE '2' (YYYY-MM-DD in): error if COOUTYPE = '1';
 *                                 else COOUTDT(1:8) = YYYYMMDD.
 *   anything else:                error.
 *   error:                        MVC COERMSG,=C'INVALID INPUT'.
 *
 * Only the bytes the assembler moves are changed; the rest of the output
 * date keeps whatever the caller had there.
 */
export const CODATECN_LENGTH = 80;
const IN_TYPE = 0;
const IN_DATE = 1;
const OUT_TYPE = 21;
const OUT_DATE = 22;
const ERR_MSG = 42;
const ERR_LEN = 38;

export function cobdatft(rec: Buffer): void {
  if (rec.length < CODATECN_LENGTH) throw new RangeError(`CODATECN-REC must be ${CODATECN_LENGTH} bytes`);
  const inType = String.fromCharCode(rec[IN_TYPE]!);
  const outType = String.fromCharCode(rec[OUT_TYPE]!);
  const copy = (to: number, from: number, len: number) => rec.copy(rec, OUT_DATE + to, IN_DATE + from, IN_DATE + from + len);
  if (inType === '1') {
    if (rec[IN_DATE + 4] === 0x2d || outType === '2') return gotoErr(rec);
    copy(0, 0, 4);
    rec[OUT_DATE + 4] = 0x2d;
    copy(5, 4, 2);
    rec[OUT_DATE + 7] = 0x2d;
    copy(8, 6, 2);
    return;
  }
  if (inType === '2') {
    if (outType === '1') return gotoErr(rec);
    copy(0, 0, 4);
    copy(4, 5, 2);
    copy(6, 8, 2);
    return;
  }
  gotoErr(rec);
}

function gotoErr(rec: Buffer): void {
  // MVC uses the 38-byte length of COERMSG; the 25 bytes after the 13-byte
  // literal come from the literal pool / storage beyond it (see LIMITATIONS).
  Buffer.from('INVALID INPUT'.padEnd(ERR_LEN, ' '), 'latin1').copy(rec, ERR_MSG);
}
