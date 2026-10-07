import { cleanup, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';
import { PROGRAMS } from '../programs';
import { mockApi, renderAt, signedIn } from './harness';

// Every input-capable field of the symbolic map (app/cpy-bms/<MAP>.CPY, the "...I" fields) is rendered on its page
// as an element carrying data-bms="<FIELD>" (pages are rendered with no data, so empty rows/options must still appear).
// Header, message and PF-key lines are rendered by components/Screen.tsx for every page.
const COPYBOOKS = import.meta.glob<string>('../../../../app/cpy-bms/*.CPY', { query: '?raw', import: 'default', eager: true });
// CRDSTPn only carries the protect attribute of an empty list row's selection field (no data of its own).
const ATTRIBUTE_ONLY = /^CRDSTP\d$/;
const SCREEN_FIELDS = new Set(['TRNNAME', 'TITLE01', 'CURDATE', 'PGMNAME', 'TITLE02', 'CURTIME', 'APPLID', 'SYSID', 'ERRMSG', 'INFOMSG', 'FKEYS', 'FKEYSC', 'FKEY05', 'FKEY12']);
function symbolicFields(mapset: string): string[] {
  const cpy = COPYBOOKS[`../../../../app/cpy-bms/${mapset}.CPY`];
  return [...cpy.matchAll(/^\s+\d+\s+([A-Z0-9]+)I\s+PIC/gm)].map((m) => m[1]);
}

afterEach(() => {
  cleanup();
  sessionStorage.clear();
});

describe('pages render the fields of their symbolic BMS map', () => {
  it.each(PROGRAMS.map((p) => [p.mapset, p] as const))('%s', async (mapset, page) => {
    if (page.program !== 'COSGN00C') signedIn(page.adminOnly ? 'ADMIN' : 'USER');
    mockApi({});
    renderAt(page.route);
    await screen.findByTestId('program');
    const rendered = new Set(Array.from(document.querySelectorAll('[data-bms]'), (el) => el.getAttribute('data-bms')));
    const expected = symbolicFields(mapset).filter((f) => !SCREEN_FIELDS.has(f) && !ATTRIBUTE_ONLY.test(f));
    expect(expected.length).toBeGreaterThan(0);
    expect(expected.filter((f) => !rendered.has(f))).toEqual([]);
  });
});
