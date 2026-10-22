import { describe, expect, it } from 'vitest';
import { CarddemoCommarea, MAIN_MENU_OPTIONS } from '../../src/online/index.js';
import { errmsg, field, runtime, signOn, stub, value } from './helpers.js';

describe('COMEN01C main menu (regular user)', () => {
  it('lists the COMEN02Y options', () => {
    const r = signOn(runtime(), 'USER0001', 'PASSWORD');
    expect(value(r, 'OPTN001')).toBe('01. Account View');
    expect(value(r, 'OPTN011')).toBe('11. Pending Authorization View');
    expect(value(r, 'OPTN012')).toBe('');
    expect(value(r, 'TRNNAME')).toBe('CM00');
  });

  it('routes a selected option to its program with the commarea (XCTL)', () => {
    const seen: Buffer[] = [];
    const rt = runtime([stub('COTRN00C', seen)]);
    signOn(rt, 'USER0001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '6' } });
    expect(r.programs).toEqual(['COMEN01C', 'COTRN00C']);
    const ca = new CarddemoCommarea(seen[0]);
    expect(ca.fromTranid).toBe('CM00');
    expect(ca.fromProgram).toBe('COMEN01C');
    expect(ca.userId).toBe('USER0001');
    expect(ca.isPgmEnter).toBe(true);
  });

  it('option of a not-yet-ported program shows "not installed" (red) and stays on the menu', () => {
    const rt = runtime();
    signOn(rt, 'USER0001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '6' } });
    expect(r.abend).toBeNull();
    expect(errmsg(r)).toBe('This option Transaction List is not installed...');
    expect(field(r, 'ERRMSG').color).toBe('RED');
    expect(value(r, 'OPTION')).toBe('06');
    expect(r.nextTransid).toBe('CM00');
  });

  it('COPAUS0C (pending authorizations) uses INQUIRE PROGRAM -> not installed', () => {
    const rt = runtime();
    signOn(rt, 'USER0001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '11' } });
    expect(errmsg(r)).toBe('This option Pending Authorization View is not installed...');
  });

  it.each(['0', '12', '99', '  ', 'a'])('invalid option %j', (opt) => {
    const rt = runtime();
    signOn(rt, 'USER0001', 'PASSWORD');
    const fields: Record<string, string> = opt === 'a' ? {} : { OPTION: opt };
    const r = rt.sendInput('T1', { aid: 'ENTER', fields });
    expect(errmsg(r)).toBe('Please enter a valid option number...');
  });

  it('COMEN02Y has no admin-only options, so a regular user is never denied', () => {
    expect(MAIN_MENU_OPTIONS.every((o) => o.usrType === 'U')).toBe(true);
  });

  it('PF3 returns to signon (XCTL without commarea)', () => {
    const rt = runtime();
    signOn(rt, 'USER0001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'PF3' });
    expect(r.programs).toEqual(['COMEN01C', 'COSGN00C']);
    expect(r.screen.map).toBe('COSGN0A');
    expect(r.nextTransid).toBe('CC00');
  });

  it.each(['PF4', 'PF5', 'PF7', 'PF8', 'PF12', 'CLEAR'])('%s is an invalid key on the menu', (aid) => {
    const rt = runtime();
    signOn(rt, 'USER0001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid });
    expect(r.screen.map).toBe('COMEN1A');
    expect(errmsg(r)).toBe('Invalid key pressed. Please see below...');
  });

  it('started directly (EIBCALEN=0) goes back to signon', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    const r = rt.startTransaction('T1', 'CM00');
    expect(r.programs).toEqual(['COMEN01C', 'COSGN00C']);
    expect(r.screen.map).toBe('COSGN0A');
  });
});

describe('COADM01C admin menu', () => {
  it('lists the COADM02Y options', () => {
    const r = signOn(runtime(), 'ADMIN001', 'PASSWORD');
    expect(value(r, 'OPTN001')).toBe('01. User List (Security)');
    expect(value(r, 'OPTN006')).toBe('06. Transaction Type Maintenance (Db2)');
    expect(value(r, 'OPTN007')).toBe('');
  });

  it('routes to a registered admin program', () => {
    const seen: Buffer[] = [];
    const rt = runtime([stub('COUSR00C', seen)]);
    signOn(rt, 'ADMIN001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '1' } });
    expect(r.programs).toEqual(['COADM01C', 'COUSR00C']);
    const ca = new CarddemoCommarea(seen[0]);
    expect(ca.fromProgram).toBe('COADM01C');
    expect(ca.fromTranid).toBe('CA00');
    expect(ca.userType).toBe('A');
  });

  it('PGMIDERR (program not installed) -> green "not installed" message', () => {
    const rt = runtime();
    signOn(rt, 'ADMIN001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '5' } });
    expect(r.abend).toBeNull();
    expect(errmsg(r)).toBe('This option Transaction Type List/Update (Db2) is not installed ...');
    expect(field(r, 'ERRMSG').color).toBe('GREEN');
    expect(r.nextTransid).toBe('CA00');
    // Bug-for-bug: the commarea was set to CDEMO-PGM-ENTER before the failed XCTL, so the
    // next input just re-displays a fresh menu.
    const again = rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '1' } });
    expect(errmsg(again)).toBe('');
    expect(value(again, 'OPTION')).toBe('');
  });

  it('invalid option and invalid key', () => {
    const rt = runtime();
    signOn(rt, 'ADMIN001', 'PASSWORD');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { OPTION: '7' } }))).toBe('Please enter a valid option number...');
    expect(errmsg(rt.sendInput('T1', { aid: 'PF8' }))).toBe('Invalid key pressed. Please see below...');
  });

  it('PF3 returns to signon', () => {
    const rt = runtime();
    signOn(rt, 'ADMIN001', 'PASSWORD');
    const r = rt.sendInput('T1', { aid: 'PF3' });
    expect(r.programs).toEqual(['COADM01C', 'COSGN00C']);
    expect(r.screen.map).toBe('COSGN0A');
  });
});
