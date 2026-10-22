/** CBACT03C: Read and print the card/account cross-reference file (CARDXREF KSDS). Port of app/cbl/CBACT03C.cbl. */
import { runKsdsPrint, type KsdsPrintResult } from '../acctcust-common/ksds-print.js';
import type { BatchRunOptions } from '../acctcust-common/support.js';

export const CBACT03C_SPEC = {
  program: 'CBACT03C',
  assign: 'XREFFILE',
  recordLength: 50,
  displayInGetNext: true,
  openError: 'ERROR OPENING XREFFILE',
  readError: 'ERROR READING XREFFILE',
  closeError: 'ERROR CLOSING XREFFILE',
} as const;

export function runCbact03c(opts: BatchRunOptions = {}): KsdsPrintResult {
  return runKsdsPrint(CBACT03C_SPEC, opts);
}
