import { CCARD_AID } from '../../common/copybooks/index.js';

/** Attention identifiers (DFHAID). */
export const AID_KEYS = [
  'ENTER', 'CLEAR', 'PA1', 'PA2', 'PA3',
  'PF1', 'PF2', 'PF3', 'PF4', 'PF5', 'PF6', 'PF7', 'PF8', 'PF9', 'PF10', 'PF11', 'PF12',
  'PF13', 'PF14', 'PF15', 'PF16', 'PF17', 'PF18', 'PF19', 'PF20', 'PF21', 'PF22', 'PF23', 'PF24',
] as const;
export type AidKey = (typeof AID_KEYS)[number];

/** DFHAID symbolic names, compared against `ctx.eib.eibaid`. */
export const DFHAID = {
  DFHENTER: 'ENTER', DFHCLEAR: 'CLEAR', DFHPA1: 'PA1', DFHPA2: 'PA2', DFHPA3: 'PA3',
  DFHPF1: 'PF1', DFHPF2: 'PF2', DFHPF3: 'PF3', DFHPF4: 'PF4', DFHPF5: 'PF5', DFHPF6: 'PF6',
  DFHPF7: 'PF7', DFHPF8: 'PF8', DFHPF9: 'PF9', DFHPF10: 'PF10', DFHPF11: 'PF11', DFHPF12: 'PF12',
  DFHPF13: 'PF13', DFHPF14: 'PF14', DFHPF15: 'PF15', DFHPF16: 'PF16', DFHPF17: 'PF17', DFHPF18: 'PF18',
  DFHPF19: 'PF19', DFHPF20: 'PF20', DFHPF21: 'PF21', DFHPF22: 'PF22', DFHPF23: 'PF23', DFHPF24: 'PF24',
} as const satisfies Record<string, AidKey>;

/** Short-read keys: no field data is transmitted (RECEIVE MAP gets MAPFAIL). */
export const isShortReadAid = (aid: AidKey): boolean => aid === 'CLEAR' || aid.startsWith('PA');

/** Accepts 'ENTER', 'PF3', 'PF03', 'F3', 'pf12', 'PA1', 'CLEAR'. */
export function parseAid(text: string | undefined): AidKey {
  const t = (text ?? 'ENTER').trim().toUpperCase();
  const m = /^P?F0?(\d{1,2})$/.exec(t);
  const key = m ? `PF${Number(m[1])}` : t;
  if ((AID_KEYS as readonly string[]).includes(key)) return key as AidKey;
  throw new Error(`unknown AID key '${text}'`);
}

/**
 * CSSTRPFY YYYY-STORE-PFKEY: maps EIBAID to CCARD-AID; PF13-PF24 fold onto PFK01-PFK12.
 * The EVALUATE has no WHEN OTHER, so PA3 leaves CCARD-AID unchanged (`current`).
 */
export function storePfKey(aid: AidKey, current: string = CCARD_AID.ENTER): string {
  switch (aid) {
    case 'ENTER': return CCARD_AID.ENTER;
    case 'CLEAR': return CCARD_AID.CLEAR;
    case 'PA1': return CCARD_AID.PA1;
    case 'PA2': return CCARD_AID.PA2;
    case 'PA3': return current;
    default: {
      const n = Number(aid.slice(2));
      const k = ((n - 1) % 12) + 1;
      return CCARD_AID[`PFK${String(k).padStart(2, '0')}` as keyof typeof CCARD_AID];
    }
  }
}
