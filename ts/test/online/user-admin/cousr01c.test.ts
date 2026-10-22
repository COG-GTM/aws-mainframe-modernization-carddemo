import { describe, expect, it } from 'vitest';
import { errmsg, field, signOn, value } from '../helpers.js';
import { adminOption, usrsec } from './helpers.js';

const FULL = { FNAME: 'Jane', LNAME: 'Doe', USERID: 'JDOE0001', PASSWD: 'SECRET01', USRTYPE: 'U' };

describe('COUSR01C add user', () => {
  it('admin option 2 shows an empty add screen with the cursor on First Name', () => {
    const { r } = adminOption('2');
    expect(r.programs).toEqual(['COADM01C', 'COUSR01C']);
    expect(r.screen.map).toBe('COUSR1A');
    expect(r.nextTransid).toBe('CU01');
    expect(errmsg(r)).toBe('');
    const f = field(r, 'FNAME');
    expect(r.screen.cursor).toEqual({ row: f.row, col: f.col });
  });

  it('validates required fields in screen order', () => {
    const cases: [Record<string, string>, string, string][] = [
      [{}, 'First Name can NOT be empty...', 'FNAME'],
      [{ FNAME: 'A' }, 'Last Name can NOT be empty...', 'LNAME'],
      [{ FNAME: 'A', LNAME: 'B' }, 'User ID can NOT be empty...', 'USERID'],
      [{ FNAME: 'A', LNAME: 'B', USERID: 'X1' }, 'Password can NOT be empty...', 'PASSWD'],
      [{ FNAME: 'A', LNAME: 'B', USERID: 'X1', PASSWD: '   ' }, 'Password can NOT be empty...', 'PASSWD'],
      [{ FNAME: 'A', LNAME: 'B', USERID: 'X1', PASSWD: 'P' }, 'User Type can NOT be empty...', 'USRTYPE'],
    ];
    for (const [fields, msg, cursor] of cases) {
      const { rt } = adminOption('2');
      const r = rt.sendInput('T1', { aid: 'ENTER', fields });
      expect(errmsg(r)).toBe(msg);
      const f = field(r, cursor);
      expect(r.screen.cursor).toEqual({ row: f.row, col: f.col });
      expect(usrsec(rt, 'X1')).toBeUndefined();
    }
  });

  it('WRITE NORMAL: record added, fields cleared, green confirmation; new user can sign on', () => {
    const { rt } = adminOption('2');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: FULL });
    expect(errmsg(r)).toBe('User JDOE0001 has been added ...');
    expect(field(r, 'ERRMSG').color).toBe('GREEN');
    expect(value(r, 'USERID')).toBe('');
    expect(value(r, 'FNAME')).toBe('');
    expect(usrsec(rt, 'JDOE0001')).toBe('JDOE0001Jane                Doe                 SECRET01U' + '\0'.repeat(23));
    expect(r.nextTransid).toBe('CU01');
    rt.openTerminal('T2');
    const s = signOn(rt, 'JDOE0001', 'SECRET01', 'T2');
    expect(s.programs).toContain('COMEN01C');
  });

  it('message uses the User ID up to its first space', () => {
    const { rt } = adminOption('2');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ...FULL, USERID: 'AB CD' } });
    expect(errmsg(r)).toBe('User AB has been added ...');
    expect(usrsec(rt, 'AB CD')).toBeDefined();
  });

  it('duplicate User ID -> DUPREC -> "User ID already exist..." and nothing changes', () => {
    const { rt } = adminOption('2');
    const before = usrsec(rt, 'USER0001');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { ...FULL, USERID: 'USER0001' } });
    expect(errmsg(r)).toBe('User ID already exist...');
    // Bug-for-bug: FNAMEL was already -1 (validation WHEN OTHER), and SEND MAP CURSOR
    // places the cursor on the first field with L = -1, so USERIDL = -1 has no effect.
    const f = field(r, 'FNAME');
    expect(r.screen.cursor).toEqual({ row: f.row, col: f.col });
    expect(value(r, 'FNAME')).toBe('Jane');
    expect(usrsec(rt, 'USER0001')).toBe(before);
  });

  it('PF4 clears, PF3 returns to the admin menu, other keys are invalid', () => {
    const { rt } = adminOption('2');
    const bad = rt.sendInput('T1', { aid: 'PF7', fields: { FNAME: 'Jane' } });
    expect(errmsg(bad)).toBe('Invalid key pressed. Please see below...');
    expect(value(bad, 'FNAME')).toBe('Jane');
    const clr = rt.sendInput('T1', { aid: 'PF4', fields: { LNAME: 'Doe' } });
    expect(value(clr, 'FNAME')).toBe('');
    expect(value(clr, 'LNAME')).toBe('');
    expect(errmsg(clr)).toBe('');
    const back = rt.sendInput('T1', { aid: 'PF3' });
    expect(back.programs).toEqual(['COUSR01C', 'COADM01C']);
  });
});
