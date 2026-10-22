import type { OnlineProgram } from '../framework/index.js';
import { COBIL00C, CORPT00C, COTRN00C, COTRN01C, COTRN02C } from '../transaction/index.js';

/**
 * Session 8 - transaction, bill-pay and report programs (COTRN00C, COTRN01C, COTRN02C, COBIL00C, CORPT00C).
 * Register each ported program with one line, e.g. `COxxxxxC,` after importing it
 * from its module under src/online/. Unregistered programs behave as not installed.
 */
export const TRANSACTION_PROGRAMS: readonly OnlineProgram[] = [COTRN00C, COTRN01C, COTRN02C, COBIL00C, CORPT00C];
