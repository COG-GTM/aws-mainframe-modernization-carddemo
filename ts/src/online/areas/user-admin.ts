import type { OnlineProgram } from '../framework/index.js';

/**
 * Session 9 - user administration programs (COUSR00C, COUSR01C, COUSR02C, COUSR03C).
 * Register each ported program with one line, e.g. `COxxxxxC,` after importing it
 * from its module under src/online/. Unregistered programs behave as not installed.
 */
export const USER_ADMIN_PROGRAMS: readonly OnlineProgram[] = [];
