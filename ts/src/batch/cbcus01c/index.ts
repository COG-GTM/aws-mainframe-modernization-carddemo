/** CBCUS01C: Read and print the customer data file (CUSTDATA KSDS). Port of app/cbl/CBCUS01C.cbl. */
import { runKsdsPrint, type KsdsPrintResult } from '../acctcust-common/ksds-print.js';
import type { BatchRunOptions } from '../acctcust-common/support.js';

export const CBCUS01C_SPEC = {
  program: 'CBCUS01C',
  assign: 'CUSTFILE',
  recordLength: 500,
  displayInGetNext: true,
  openError: 'ERROR OPENING CUSTFILE',
  readError: 'ERROR READING CUSTOMER FILE',
  closeError: 'ERROR CLOSING CUSTOMER FILE',
} as const;

export function runCbcus01c(opts: BatchRunOptions = {}): KsdsPrintResult {
  return runKsdsPrint(CBCUS01C_SPEC, opts);
}
