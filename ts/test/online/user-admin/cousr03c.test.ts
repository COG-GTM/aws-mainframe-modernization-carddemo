import { describe, expect, it } from 'vitest';
import { errmsg, field, value } from '../helpers.js';
import { adminOption, rows, usrsec } from './helpers.js';

describe('COUSR03C delete user', () => {
  it('admin option 4 shows an empty delete screen; ENTER fetches the user', () => {
    const { rt, r } = adminOption('4');
    expect(r.programs).toEqual(['COADM01C', 'COUSR03C']);
    expect(r.screen.map).toBe('COUSR3A');
    const f = rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'ADMIN005' } });
    expect(value(f, 'FNAME')).toBe('GRANVILLE');
    expect(value(f, 'LNAME')).toBe('LACHAPELLE');
    expect(value(f, 'USRTYPE')).toBe('A');
    expect(errmsg(f)).toBe('Press PF5 key to delete this user ...');
    expect(field(f, 'ERRMSG').color).toBe('NEUTRAL');
  });

  it('ENTER: empty User ID / NOTFND', () => {
    const { rt } = adminOption('4');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: ' ' } }))).toBe('User ID can NOT be empty...');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'NOBODY' } }))).toBe('User ID NOT found...');
  });

  it('PF5 deletes the record, clears the screen, green confirmation', () => {
    const { rt } = adminOption('4');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0004' } });
    const r = rt.sendInput('T1', { aid: 'PF5' });
    expect(errmsg(r)).toBe('User USER0004 has been deleted ...');
    expect(field(r, 'ERRMSG').color).toBe('GREEN');
    expect(value(r, 'USRIDIN')).toBe('');
    expect(value(r, 'FNAME')).toBe('');
    expect(usrsec(rt, 'USER0004')).toBeUndefined();
  });

  it('PF5 for an unknown User ID: READ NOTFND then DELETE without a lock -> "Unable to Update User..." (bug-for-bug)', () => {
    const { rt } = adminOption('4');
    const r = rt.sendInput('T1', { aid: 'PF5', fields: { USRIDIN: 'NOBODY' } });
    expect(errmsg(r)).toBe('Unable to Update User...');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF5', fields: { USRIDIN: ' ' } }))).toBe('User ID can NOT be empty...');
  });

  it('from the user list: D selection, PF5 delete, PF3 back to the list without the user', () => {
    const { rt } = adminOption('1');
    const d = rt.sendInput('T1', { aid: 'ENTER', fields: { SEL0010: 'D' } });
    expect(value(d, 'USRIDIN')).toBe('USER0005');
    rt.sendInput('T1', { aid: 'PF5' });
    expect(usrsec(rt, 'USER0005')).toBeUndefined();
    const back = rt.sendInput('T1', { aid: 'PF3' });
    expect(back.programs).toEqual(['COUSR03C', 'COUSR00C']);
    expect(rows(back)[9]).toBe('');
    expect(rows(back)[8]).toBe('USER0004');
  });

  it('PF4 clears, PF12 goes to the admin menu, other keys are invalid', () => {
    const { rt } = adminOption('4');
    rt.sendInput('T1', { aid: 'ENTER', fields: { USRIDIN: 'USER0001' } });
    expect(value(rt.sendInput('T1', { aid: 'PF4' }), 'USRIDIN')).toBe('');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF2' }))).toBe('Invalid key pressed. Please see below...');
    expect(rt.sendInput('T1', { aid: 'PF12' }).programs).toEqual(['COUSR03C', 'COADM01C']);
    expect(usrsec(rt, 'USER0001')).toBeDefined();
  });
});
