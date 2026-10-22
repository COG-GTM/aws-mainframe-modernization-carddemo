import type { OnlineProgram } from '../framework/index.js';
import { COACTUPC } from '../account-card/coactupc.js';
import { COACTVWC } from '../account-card/coactvwc.js';
import { COCRDLIC } from '../account-card/cocrdlic.js';
import { COCRDSLC } from '../account-card/cocrdslc.js';
import { COCRDUPC } from '../account-card/cocrdupc.js';

/**
 * Session 7 - account and card programs (COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC).
 * Register each ported program with one line, e.g. `COxxxxxC,` after importing it
 * from its module under src/online/. Unregistered programs behave as not installed.
 */
export const ACCOUNT_CARD_PROGRAMS: readonly OnlineProgram[] = [
  COACTVWC,
  COACTUPC,
  COCRDLIC,
  COCRDSLC,
  COCRDUPC,
];
