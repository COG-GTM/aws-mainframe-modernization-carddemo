import { describe, expect, it } from 'vitest';
import { PROGRAMS } from '../programs';

// Every input-capable field of the symbolic map (app/cpy-bms/<MAP>.CPY, the "...I" fields) has a place on its page.
// Header, message and PF-key lines are rendered by components/Screen.tsx for every page.
const COPYBOOKS = import.meta.glob<string>('../../../../app/cpy-bms/*.CPY', { query: '?raw', import: 'default', eager: true });
const PAGES = import.meta.glob<string>('../pages/*.tsx', { query: '?raw', import: 'default', eager: true });
// CRDSTPn only carries the protect attribute of an empty list row's selection field (no data of its own).
const ATTRIBUTE_ONLY = /^CRDSTP\d$/;
const SCREEN_FIELDS = new Set(['TRNNAME', 'TITLE01', 'CURDATE', 'PGMNAME', 'TITLE02', 'CURTIME', 'APPLID', 'SYSID', 'ERRMSG', 'INFOMSG', 'FKEYS', 'FKEYSC', 'FKEY05', 'FKEY12']);
const PAGE_FILE: Record<string, string> = {
  COSGN00: 'SignonPage', COMEN01: 'MenuPage', COADM01: 'MenuPage', COACTVW: 'AccountViewPage', COACTUP: 'AccountUpdatePage',
  COCRDLI: 'CardListPage', COCRDSL: 'CardDetailPage', COCRDUP: 'CardUpdatePage', COTRN00: 'TransactionListPage',
  COTRN01: 'TransactionViewPage', COTRN02: 'TransactionAddPage', COBIL00: 'BillPaymentPage', CORPT00: 'ReportPage',
  COUSR00: 'UserListPage', COUSR01: 'UserAddPage', COUSR02: 'UserUpdatePage', COUSR03: 'UserDeletePage',
};

function symbolicFields(mapset: string): string[] {
  const cpy = COPYBOOKS[`../../../../app/cpy-bms/${mapset}.CPY`];
  return [...cpy.matchAll(/^\s+\d+\s+([A-Z0-9]+)I\s+PIC/gm)].map((m) => m[1]);
}

describe('pages carry the fields of their symbolic BMS map', () => {
  it.each(PROGRAMS.map((p) => p.mapset))('%s', (mapset) => {
    const source = PAGES[`../pages/${PAGE_FILE[mapset]}.tsx`];
    expect(symbolicFields(mapset).length).toBeGreaterThan(6);
    const missing = symbolicFields(mapset)
      .filter((f) => !SCREEN_FIELDS.has(f) && !ATTRIBUTE_ONLY.test(f))
      .filter((f) => {
        if (source.includes(`"${f}"`) || source.includes(`'${f}'`)) return false;
        // composed names: date('openDate', 'OPN', ...) with bms={`${bms}YEAR`} -> OPNYEAR
        const suffixes = [...source.matchAll(/`\$\{bms\}([A-Z]+)`/g)].map((m) => m[1]);
        if (suffixes.some((sfx) => f.endsWith(sfx) && source.includes(`'${f.slice(0, -sfx.length)}'`))) return false;
        // repeated rows / option lines: SEL0001..SEL0010 -> `SEL00${n}`, OPTN001 -> `OPTN${n}`
        const stem = f.replace(/\d+$/, '').replace(/0+$/, '');
        return !new RegExp(`\`${stem}0*\\$\\{`).test(source);
      });
    expect(missing).toEqual([]);
  });
});
