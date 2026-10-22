import { describe, expect, it } from 'vitest';
import { CarddemoCommarea } from '../../src/online/index.js';
import { errmsg, runtime, signOn, value } from './helpers.js';

describe('COSGN00C signon', () => {
  it('first entry (EIBCALEN=0) sends the signon map with cursor on USERID and RETURN TRANSID CC00', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    const r = rt.startTransaction('T1', 'CC00');
    expect(r.programs).toEqual(['COSGN00C']);
    expect(r.screen.map).toBe('COSGN0A');
    expect(r.nextTransid).toBe('CC00');
    expect(r.screen.cursor).toEqual({ row: 19, col: 43 });
    expect(value(r, 'TRNNAME')).toBe('CC00');
    expect(value(r, 'PGMNAME')).toBe('COSGN00C');
    expect(value(r, 'CURDATE')).toBe('10/22/26');
    expect(value(r, 'CURTIME')).toBe('14:05:09');
    expect(value(r, 'APPLID')).toBe('CARDDEMO');
    expect(value(r, 'SYSID')).toBe('CDEM');
    expect(value(r, 'TITLE01')).toBe('      AWS Mainframe Modernization');
    expect(errmsg(r)).toBe('');
    expect(r.commarea).toHaveLength(160);
  });

  it('valid admin credentials XCTL to COADM01C with the commarea populated', () => {
    const rt = runtime();
    const r = signOn(rt, 'admin001', 'password');
    expect(r.abend).toBeNull();
    expect(r.programs).toEqual(['COSGN00C', 'COADM01C']);
    expect(r.screen.map).toBe('COADM1A');
    expect(r.nextTransid).toBe('CA00');
    const ca = new CarddemoCommarea(Buffer.from(r.commarea!, 'latin1'));
    expect(ca.userId).toBe('ADMIN001');
    expect(ca.userType).toBe('A');
    expect(ca.fromProgram).toBe('COSGN00C');
    expect(ca.fromTranid).toBe('CC00');
    expect(ca.isPgmReenter).toBe(true);
  });

  it('valid regular-user credentials (case-insensitive) XCTL to COMEN01C', () => {
    const rt = runtime();
    const r = signOn(rt, 'User0001', 'PassWord');
    expect(r.programs).toEqual(['COSGN00C', 'COMEN01C']);
    expect(r.screen.map).toBe('COMEN1A');
    expect(r.nextTransid).toBe('CM00');
    const ca = new CarddemoCommarea(Buffer.from(r.commarea!, 'latin1'));
    expect(ca.userId).toBe('USER0001');
    expect(ca.userType).toBe('U');
  });

  it('wrong password re-sends signon with the message and cursor on PASSWD', () => {
    const r = signOn(runtime(), 'USER0001', 'WRONG');
    expect(r.screen.map).toBe('COSGN0A');
    expect(errmsg(r)).toBe('Wrong Password. Try again ...');
    expect(r.screen.cursor).toEqual({ row: 20, col: 43 });
    expect(value(r, 'USERID')).toBe('USER0001');
    expect(r.nextTransid).toBe('CC00');
  });

  it('unknown user (RESP NOTFND) -> User not found, cursor on USERID', () => {
    const r = signOn(runtime(), 'NOBODY', 'PASSWORD');
    expect(errmsg(r)).toBe('User not found. Try again ...');
    expect(r.screen.cursor).toEqual({ row: 19, col: 43 });
  });

  it('blank user id / blank password', () => {
    expect(errmsg(signOn(runtime(), '        ', 'PASSWORD'))).toBe('Please enter User ID ...');
    const r = signOn(runtime(), 'USER0001', '');
    expect(errmsg(r)).toBe('Please enter Password ...');
    expect(r.screen.cursor).toEqual({ row: 20, col: 43 });
  });

  it('ENTER with nothing typed (MAPFAIL) -> Please enter User ID', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    rt.startTransaction('T1', 'CC00');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER' }))).toBe('Please enter User ID ...');
  });

  it('other READ failures -> Unable to verify the User', () => {
    const rt = runtime();
    rt.catalog.delete('USRSEC');
    expect(errmsg(signOn(rt, 'USER0001', 'PASSWORD'))).toBe('Unable to verify the User ...');
  });

  it('PF3 sends the thank-you text and ends the conversation', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    rt.startTransaction('T1', 'CC00');
    const r = rt.sendInput('T1', { aid: 'PF3' });
    expect(r.screen.map).toBeNull();
    expect(r.screen.lines[0]!.trimEnd()).toBe('Thank you for using CardDemo application...');
    expect(r.nextTransid).toBeNull();
    expect(() => rt.sendInput('T1', { aid: 'ENTER' })).toThrow(/no transaction is pending/);
  });

  it.each(['PF5', 'PF12', 'CLEAR', 'PA1'])('%s is an invalid key on signon', (aid) => {
    const rt = runtime();
    rt.openTerminal('T1');
    rt.startTransaction('T1', 'CC00');
    const r = rt.sendInput('T1', { aid });
    expect(r.screen.map).toBe('COSGN0A');
    expect(errmsg(r)).toBe('Invalid key pressed. Please see below...');
    expect(r.nextTransid).toBe('CC00');
  });
});
