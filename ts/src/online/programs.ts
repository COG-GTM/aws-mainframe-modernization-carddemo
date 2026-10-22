import { ACCOUNT_CARD_PROGRAMS } from './areas/account-card.js';
import { TRANSACTION_PROGRAMS } from './areas/transaction.js';
import { USER_ADMIN_PROGRAMS } from './areas/user-admin.js';
import { OnlineRuntime, type OnlineProgram, type RuntimeOptions } from './framework/index.js';
import { COADM01C } from './menu/coadm01c.js';
import { COMEN01C } from './menu/comen01c.js';
import { COSGN00C } from './signon/cosgn00c.js';

/** Session 6: signon and menus. */
export const CORE_PROGRAMS: readonly OnlineProgram[] = [COSGN00C, COMEN01C, COADM01C];

/** Every registered CardDemo online program. */
export const ONLINE_PROGRAMS: readonly OnlineProgram[] = [
  ...CORE_PROGRAMS,
  ...ACCOUNT_CARD_PROGRAMS,
  ...TRANSACTION_PROGRAMS,
  ...USER_ADMIN_PROGRAMS,
];

/** A runtime with every registered program (seed data unless `catalog` is given). */
export function createCardDemoRuntime(opts: Omit<RuntimeOptions, 'programs' | 'registry'> = {}): OnlineRuntime {
  return new OnlineRuntime({ ...opts, programs: ONLINE_PROGRAMS });
}
