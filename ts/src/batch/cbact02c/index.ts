/** CBACT02C: Read and print the card data file (CARDDATA KSDS). Port of app/cbl/CBACT02C.cbl. */
import { runKsdsPrint, type KsdsPrintResult } from '../acctcust-common/ksds-print.js';
import type { BatchRunOptions } from '../acctcust-common/support.js';

export const CBACT02C_SPEC = {
  program: 'CBACT02C',
  assign: 'CARDFILE',
  recordLength: 150,
  displayInGetNext: false,
  openError: 'ERROR OPENING CARDFILE',
  readError: 'ERROR READING CARDFILE',
  closeError: 'ERROR CLOSING CARDFILE',
} as const;

export function runCbact02c(opts: BatchRunOptions = {}): KsdsPrintResult {
  return runKsdsPrint(CBACT02C_SPEC, opts);
}
