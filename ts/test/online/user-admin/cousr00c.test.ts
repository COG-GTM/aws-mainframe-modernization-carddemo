import { describe, expect, it } from 'vitest';
import { errmsg, field, runtime, value } from '../helpers.js';
import { adminOption, rows, seedUser, type Rt } from './helpers.js';

const SEED = ['ADMIN001', 'ADMIN002', 'ADMIN003', 'ADMIN004', 'ADMIN005', 'USER0001', 'USER0002', 'USER0003', 'USER0004', 'USER0005'];

/** Seed + 15 extra users (USER0006..USER0020) = 25 users, 3 pages. */
function bigRuntime(): Rt {
  const rt = runtime();
  for (let i = 6; i <= 20; i++) seedUser(rt, `USER${String(i).padStart(4, '0')}`, `FIRST${i}`, `LAST${i}`);
  return rt;
}

describe('COUSR00C user list', () => {
  it('admin menu option 1 lists the first 10 users in key order (page 1)', () => {
    const { r } = adminOption('1');
    expect(r.programs).toEqual(['COADM01C', 'COUSR00C']);
    expect(r.screen.map).toBe('COUSR0A');
    expect(r.nextTransid).toBe('CU00');
    expect(value(r, 'TRNNAME')).toBe('CU00');
    expect(value(r, 'PGMNAME')).toBe('COUSR00C');
    expect(rows(r)).toEqual(SEED);
    expect(value(r, 'FNAME01')).toBe('MARGARET');
    expect(value(r, 'LNAME01')).toBe('GOLD');
    expect(value(r, 'UTYPE01')).toBe('A');
    expect(value(r, 'UTYPE06')).toBe('U');
    expect(value(r, 'PAGENUM')).toBe('00000001');
    // Exactly 10 users: the look-ahead READNEXT hits ENDFILE.
    expect(errmsg(r)).toBe('You have reached the bottom of the page...');
  });

  it('PF8 at the last page / PF7 at the first page', () => {
    const { rt } = adminOption('1');
    let r = rt.sendInput('T1', { aid: 'PF8' });
    expect(errmsg(r)).toBe('You are already at the bottom of the page...');
    expect(rows(r)).toEqual(SEED);
    r = rt.sendInput('T1', { aid: 'PF7' });
    expect(errmsg(r)).toBe('You are already at the top of the page...');
    expect(value(r, 'PAGENUM')).toBe('00000001');
  });

  it('pages forward with PF8 and backward with PF7', () => {
    const { rt, r: p1 } = adminOption('1', bigRuntime());
    expect(errmsg(p1)).toBe('');
    expect(rows(p1)).toEqual(SEED);
    const p2 = rt.sendInput('T1', { aid: 'PF8' });
    expect(value(p2, 'PAGENUM')).toBe('00000002');
    expect(rows(p2)).toEqual(Array.from({ length: 10 }, (_, i) => `USER${String(i + 6).padStart(4, '0')}`));
    expect(value(p2, 'FNAME01')).toBe('FIRST6');
    const p3 = rt.sendInput('T1', { aid: 'PF8' });
    expect(value(p3, 'PAGENUM')).toBe('00000003');
    expect(rows(p3)).toEqual(['USER0016', 'USER0017', 'USER0018', 'USER0019', 'USER0020', '', '', '', '', '']);
    expect(errmsg(p3)).toBe('You have reached the bottom of the page...');
    expect(rt.sendInput('T1', { aid: 'PF8' }).screen.fields.find((f) => f.name === 'ERRMSG')!.value.trimEnd()).toBe(
      'You are already at the bottom of the page...',
    );
    const b2 = rt.sendInput('T1', { aid: 'PF7' });
    expect(value(b2, 'PAGENUM')).toBe('00000002');
    expect(rows(b2)).toEqual(rows(p2));
    const b1 = rt.sendInput('T1', { aid: 'PF7' });
    expect(value(b1, 'PAGENUM')).toBe('00000001');
    expect(rows(b1)).toEqual(SEED);
    // READPREV before ADMIN001 hits the top of the file.
    expect(errmsg(b1)).toBe('You have reached the top of the page...');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF7' }))).toBe('You are already at the top of the page...');
  });

  it('ENTER with a search User ID starts the list at that key (GTEQ)', () => {
    const { rt } = adminOption('1');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0003' } });
    expect(rows(r)).toEqual(['USER0003', 'USER0004', 'USER0005', '', '', '', '', '', '', '']);
    expect(value(r, 'USRIDIN')).toBe('');
    expect(value(r, 'PAGENUM')).toBe('00000001');
    const partial = rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'B' } });
    expect(rows(partial)[0]).toBe('USER0001');
  });

  it('search key past the last user: STARTBR NOTFND, then ENDBR without a browse abends (bug-for-bug)', () => {
    const { rt } = adminOption('1');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'ZZZZZZZZ' } });
    expect(r.abend?.code).toBe('AEIP');
  });

  it('row selection U -> COUSR02C, D -> COUSR03C with the selected user in the commarea', () => {
    const { rt } = adminOption('1');
    const u = rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0003: 'U' } });
    expect(u.programs).toEqual(['COUSR00C', 'COUSR02C']);
    expect(value(u, 'USRIDIN')).toBe('ADMIN003');
    expect(value(u, 'FNAME')).toBe('RAYMOND');
    expect(errmsg(u)).toBe('Press PF5 key to save your updates ...');

    const { rt: rt2 } = adminOption('1');
    const d = rt2.sendInput('T1', { aid: 'ENTER', fields: { SEL0007: 'd' } });
    expect(d.programs).toEqual(['COUSR00C', 'COUSR03C']);
    expect(value(d, 'USRIDIN')).toBe('USER0002');
    expect(errmsg(d)).toBe('Press PF5 key to delete this user ...');
  });

  it('invalid selection value', () => {
    const { rt } = adminOption('1', bigRuntime());
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0001: 'X' } });
    expect(r.programs).toEqual(['COUSR00C']);
    expect(errmsg(r)).toBe('Invalid selection. Valid values are U and D');
    expect(rows(r)).toEqual(SEED);
    // Bug-for-bug: the list is re-read after the message is set, so an ENDFILE on the
    // look-ahead READNEXT (exactly 10 users) overwrites it.
    const { rt: rt2 } = adminOption('1');
    expect(errmsg(rt2.sendInput('T1', { aid: 'ENTER', fields: { SEL0001: 'X' } }))).toBe('You have reached the bottom of the page...');
  });

  it('PF3 returns to the admin menu; other keys are invalid', () => {
    const { rt } = adminOption('1');
    const bad = rt.sendInput('T1', { aid: 'PF5' });
    expect(errmsg(bad)).toBe('Invalid key pressed. Please see below...');
    const back = rt.sendInput('T1', { aid: 'PF3' });
    expect(back.programs).toEqual(['COUSR00C', 'COADM01C']);
    expect(back.screen.map).toBe('COADM1A');
  });

  it('EIBCALEN = 0 (CU00 typed on a clear screen) goes to sign-on', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    const r = rt.startTransaction('T1', 'CU00');
    expect(r.programs).toEqual(['COUSR00C', 'COSGN00C']);
    expect(field(r, 'USERID')).toBeDefined();
  });
});
