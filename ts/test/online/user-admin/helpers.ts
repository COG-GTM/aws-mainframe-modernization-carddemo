import type { TransactionResult } from '../../../src/online/index.js';
import { runtime, signOn } from '../helpers.js';

export type Rt = ReturnType<typeof runtime>;

/** Signs on as ADMIN001 and picks an admin-menu option (1 list, 2 add, 3 update, 4 delete). */
export function adminOption(option: string, rt: Rt = runtime()): { rt: Rt; r: TransactionResult } {
  signOn(rt, 'ADMIN001', 'PASSWORD');
  const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: option } });
  return { rt, r };
}

/** USRSEC record (latin1) or undefined. */
export const usrsec = (rt: Rt, id: string): string | undefined => rt.catalog.require('USRSEC').read(id.padEnd(8))?.toString('latin1');

/** Adds a user straight into the USRSEC cluster (test fixture). */
export function seedUser(rt: Rt, id: string, fname: string, lname: string, type = 'U', pwd = 'PASSWORD'): void {
  const rec = id.padEnd(8) + fname.padEnd(20) + lname.padEnd(20) + pwd.padEnd(8) + type + ' '.repeat(23);
  rt.catalog.require('USRSEC').insert(Buffer.from(rec, 'latin1'));
}

/** USRIDnn values of the list rows (trimmed). */
export const rows = (r: TransactionResult): string[] =>
  Array.from({ length: 10 }, (_, i) => r.screen.fields.find((f) => f.name === `USRID${String(i + 1).padStart(2, '0')}`)!.value.trimEnd());
