import { describe, expect, it } from 'vitest';
import { errmsg, field, value } from '../helpers.js';
import { adminOption, usrsec } from './helpers.js';

describe('COUSR02C update user', () => {
  it('admin option 3 shows an empty update screen', () => {
    const { r } = adminOption('3');
    expect(r.programs).toEqual(['COADM01C', 'COUSR02C']);
    expect(r.screen.map).toBe('COUSR2A');
    expect(errmsg(r)).toBe('');
    expect(value(r, 'USRIDIN')).toBe('');
  });

  it('ENTER fetches the user (READ UPDATE NORMAL)', () => {
    const { rt } = adminOption('3');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0002' } });
    expect(value(r, 'FNAME')).toBe('AJITH');
    expect(value(r, 'LNAME')).toBe('KUMAR');
    expect(value(r, 'PASSWD')).toBe('PASSWORD');
    expect(value(r, 'USRTYPE')).toBe('U');
    expect(errmsg(r)).toBe('Press PF5 key to save your updates ...');
    expect(field(r, 'ERRMSG').color).toBe('NEUTRAL');
  });

  it('ENTER: empty User ID / NOTFND', () => {
    const { rt } = adminOption('3');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: '  ' } }))).toBe('User ID can NOT be empty...');
    const nf = rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'NOBODY' } });
    expect(errmsg(nf)).toBe('User ID NOT found...');
    expect(value(nf, 'FNAME')).toBe('');
  });

  it('PF5 with a change rewrites the record (green confirmation)', () => {
    const { rt } = adminOption('3');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0002' } });
    const r = rt.sendInput('T1', { aid: 'PF5', fields: { LNAME: 'SHARMA', USRTYPE: 'A' } });
    expect(errmsg(r)).toBe('User USER0002 has been updated ...');
    expect(field(r, 'ERRMSG').color).toBe('GREEN');
    expect(value(r, 'LNAME')).toBe('SHARMA');
    expect(usrsec(rt, 'USER0002')).toBe('USER0002AJITH               SHARMA              PASSWORDA' + ' '.repeat(23));
  });

  it('PF5 without a change -> "Please modify to update ..." (red), no rewrite', () => {
    const { rt } = adminOption('3');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0002' } });
    const before = usrsec(rt, 'USER0002');
    const r = rt.sendInput('T1', { aid: 'PF5' });
    expect(errmsg(r)).toBe('Please modify to update ...');
    expect(field(r, 'ERRMSG').color).toBe('RED');
    expect(usrsec(rt, 'USER0002')).toBe(before);
  });

  it('PF5 validations', () => {
    const { rt } = adminOption('3');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0002' } });
    expect(errmsg(rt.sendInput('T1', { aid: 'PF5', fields: { FNAME: ' ' } }))).toBe('First Name can NOT be empty...');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF5', fields: { FNAME: 'A', PASSWD: ' ' } }))).toBe('Password can NOT be empty...');
  });

  it('PF5 for an unknown User ID: READ NOTFND, then REWRITE without a lock -> "Unable to Update User..." (bug-for-bug)', () => {
    const { rt } = adminOption('3');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0002' } });
    const r = rt.sendInput('T1', { aid: 'PF5', fields: { USRIDIN: 'NOBODY' } });
    expect(errmsg(r)).toBe('Unable to Update User...');
    expect(usrsec(rt, 'NOBODY')).toBeUndefined();
  });

  it('PF3 saves the change and returns to the calling program', () => {
    const { rt } = adminOption('3');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0003' } });
    const r = rt.sendInput('T1', { aid: 'PF3', fields: { FNAME: 'LARS' } });
    expect(r.programs).toEqual(['COUSR02C', 'COADM01C']);
    expect(usrsec(rt, 'USER0003')!.slice(8, 28).trimEnd()).toBe('LARS');
  });

  it('from the user list: PF3 returns to COUSR00C showing the updated row; PF12 goes to the admin menu', () => {
    const { rt } = adminOption('1');
    rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0006: 'U' } });
    const r = rt.sendInput('T1', { aid: 'PF3', fields: { FNAME: 'LARRY' } });
    expect(r.programs).toEqual(['COUSR02C', 'COUSR00C']);
    expect(value(r, 'FNAME06')).toBe('LARRY');
    rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0001: 'u' } });
    expect(rt.sendInput('T1', { aid: 'PF12' }).programs).toEqual(['COUSR02C', 'COADM01C']);
  });

  it('PF4 clears; other keys are invalid', () => {
    const { rt } = adminOption('3');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0002' } });
    const clr = rt.sendInput('T1', { aid: 'PF4' });
    expect(value(clr, 'USRIDIN')).toBe('');
    expect(value(clr, 'FNAME')).toBe('');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF8' }))).toBe('Invalid key pressed. Please see below...');
  });
});
