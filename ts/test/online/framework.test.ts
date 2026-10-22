import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { AddressInfo } from 'node:net';
import { describe, expect, it } from 'vitest';
import { generate } from '../../tools/gen-bms-maps.js';
import { createOnlineServer } from '../../src/online/server/http.js';
import {
  CicsAbend,
  SymbolicMap,
  TaskFiles,
  applyInput,
  cobolString,
  emptyScreen,
  formatTime,
  getMapDefinition,
  numval,
  numvalC,
  parseAid,
  receiveMap,
  sendMap,
  storePfKey,
  testNumval,
  testNumvalC,
  type OnlineProgram,
} from '../../src/online/index.js';
import { loadSeedCatalog, CARDDEMO_CICS_FILES } from '../../src/common/seed/index.js';
import { NOW, runtime } from './helpers.js';

describe('BMS maps', () => {
  it('maps.generated.ts is up to date with app/bms + app/cpy-bms', () => {
    const ts = resolve(import.meta.dirname, '../..');
    const committed = readFileSync(resolve(ts, 'src/online/framework/bms/maps.generated.ts'), 'utf8');
    expect(generate(resolve(ts, '../app'))).toBe(committed);
  });

  it('carries BMS attributes', () => {
    const def = getMapDefinition('COSGN0A', 'COSGN00');
    expect(def.byName.get('PASSWD')).toMatchObject({ row: 20, col: 43, length: 8, attrb: ['DRK', 'FSET', 'UNPROT'] });
    expect(def.byName.get('ERRMSG')?.length).toBe(78);
    expect(getMapDefinition('COMEN1A').byName.get('OPTION')?.justify).toEqual(['RIGHT', 'ZERO']);
    expect(() => getMapDefinition('COSGN0A', 'COMEN01')).toThrow();
  });

  it('RECEIVE MAP applies JUSTIFY=(RIGHT,ZERO), sets L, nulls unmodified fields', () => {
    const out = new SymbolicMap('COMEN1A');
    const screen = applyInput(sendMap(emptyScreen(), out, { erase: true }), 'ENTER', { OPTION: '1' });
    const into = new SymbolicMap('COMEN1A');
    expect(receiveMap(screen, into, 'ENTER')).toBe(0);
    expect(into.get('OPTION')).toBe('01');
    expect(into.getLength('OPTION')).toBe(1);
    expect(into.get('ERRMSG')).toBe('\0'.repeat(78));
    expect(receiveMap(screen, into, 'CLEAR')).toBe(36);
  });

  it('rejects input into protected fields and letters into NUM fields', () => {
    const screen = sendMap(emptyScreen(), new SymbolicMap('COMEN1A'), { erase: true });
    expect(() => applyInput(screen, 'ENTER', { ERRMSG: 'x' })).toThrow(/protected/);
    expect(() => applyInput(screen, 'ENTER', { OPTION: 'ab' })).toThrow(/numeric/);
  });
});

describe('AID handling', () => {
  it('parses key names and folds PF13-24 (CSSTRPFY)', () => {
    expect(parseAid('pf03')).toBe('PF3');
    expect(parseAid('F12')).toBe('PF12');
    expect(parseAid(undefined)).toBe('ENTER');
    expect(() => parseAid('PF25')).toThrow();
    expect(storePfKey('PF15')).toBe('PFK03');
    expect(storePfKey('ENTER')).toBe('ENTER');
    expect(storePfKey('PA3', 'PFK07')).toBe('PFK07');
  });
});

describe('helpers', () => {
  it('NUMVAL / NUMVAL-C / TEST-NUMVAL', () => {
    expect(numval('  -12.50 ').toString()).toBe('-12.5');
    expect(numval('12.5-').toString()).toBe('-12.5');
    expect(numval('7 CR').toString()).toBe('-7');
    expect(numval('abc').toString()).toBe('0');
    expect(numvalC('$1,234.56').toString()).toBe('1234.56');
    expect(numvalC('-$1,000').toString()).toBe('-1000');
    expect(testNumval('123')).toBe(0);
    expect(testNumval('12x')).toBe(3);
    expect(testNumval('   ')).toBe(4);
    expect(testNumvalC('$1,000.00')).toBe(0);
  });

  it('STRING DELIMITED BY and FORMATTIME', () => {
    expect(cobolString(['A ', { value: 'Bill Payment   x', delimitedBy: '  ' }, '!'], ' '.repeat(20))).toBe('A Bill Payment!     ');
    expect(formatTime(NOW, { datesep: '/', timesep: ':' })).toMatchObject({ mmddyy: '10/22/26', yyyymmdd: '2026/10/22', time: '14:05:09' });
  });
});

describe('TaskFiles SYNCPOINT ROLLBACK', () => {
  const rec = (id: string): Buffer => Buffer.from(id.padEnd(80, ' '), 'latin1');
  it('backs out recoverable files only (CSD files are RECOVERY(NONE))', () => {
    const { catalog } = loadSeedCatalog();
    const plain = new TaskFiles(catalog, CARDDEMO_CICS_FILES);
    expect(plain.write({ file: 'USRSEC', ridfld: 'NEWUSER1', from: rec('NEWUSER1') }).resp).toBe(0);
    plain.rollback();
    expect(catalog.require('USRSEC').size).toBe(11);
    const rec2 = new TaskFiles(catalog, CARDDEMO_CICS_FILES, new Set(['USRSEC']));
    rec2.write({ file: 'USRSEC', ridfld: 'NEWUSER2', from: rec('NEWUSER2') });
    rec2.delete({ file: 'USRSEC', ridfld: 'ADMIN001' });
    rec2.rollback();
    expect(catalog.require('USRSEC').size).toBe(11);
    expect(rec2.read({ file: 'USRSEC', ridfld: 'ADMIN001' }).resp).toBe(0);
    expect(rec2.read({ file: 'USRSEC', ridfld: 'NEWUSER2' }).resp).toBe(13);
  });
});

describe('runtime', () => {
  it('unknown TRANSID and transaction whose program is not ported', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    expect(rt.startTransaction('T1', 'ZZZZ').screen.lines[0]).toMatch(/DFHAC2001 .* Transaction 'ZZZZ' is not recognized/);
    const r = rt.startTransaction('T1', 'CAVW');
    expect(r.abend?.code).toBe('APCT');
    expect(r.nextTransid).toBeNull();
  });

  it('abends: program check -> ASRA, HANDLE ABEND handler, LINK', () => {
    const boom: OnlineProgram = { program: 'BOOM', transid: 'BOOM', run: () => { throw new Error('S0C7'); } };
    const handled: OnlineProgram = {
      program: 'HNDL',
      transid: 'HNDL',
      run(ctx) {
        ctx.abendHandler = (a: CicsAbend) => { ctx.sendText(`handled ${a.code}`); ctx.return(); };
        ctx.abend('9999');
      },
    };
    const callee: OnlineProgram = { program: 'CALLEE', run(ctx) { ctx.commarea!.write('OK', 0, 'latin1'); ctx.return(); } };
    const caller: OnlineProgram = {
      program: 'CALLER',
      transid: 'CALR',
      run(ctx) {
        const r = ctx.link('CALLEE', Buffer.from('..', 'latin1'));
        ctx.sendText(`got ${r.commarea!.toString('latin1')}`);
        ctx.return();
      },
    };
    const rt = runtime([boom, handled, callee, caller]);
    rt.openTerminal('T1');
    const b = rt.startTransaction('T1', 'BOOM');
    expect(b.abend?.code).toBe('ASRA');
    expect(b.screen.lines[0]).toMatch(/DFHAC2206 14:05:09 CARDDEMO Transaction BOOM failed with abend ASRA/);
    expect(rt.startTransaction('T1', 'HNDL').screen.lines[0]!.trimEnd()).toBe('handled 9999');
    expect(rt.startTransaction('T1', 'CALR').screen.lines[0]!.trimEnd()).toBe('got OK');
  });
});

describe('HTTP API', () => {
  it('POST /api/transaction/CC00 then /api/terminal/{id} signs on', async () => {
    const server = createOnlineServer(runtime());
    await new Promise<void>((r) => server.listen(0, r));
    const base = `http://127.0.0.1:${(server.address() as AddressInfo).port}`;
    try {
      const post = async (path: string, body: unknown) => {
        const res = await fetch(base + path, { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(body) });
        return { status: res.status, json: (await res.json()) as { terminalId: string; screen: { map: string }; nextTransid: string; programs: string[] } };
      };
      const a = await post('/api/transaction/CC00', {});
      expect(a.status).toBe(200);
      expect(a.json.screen.map).toBe('COSGN0A');
      const b = await post(`/api/terminal/${a.json.terminalId}`, { aid: 'ENTER', fields: { USERID: 'ADMIN001', PASSWD: 'PASSWORD' } });
      expect(b.json.programs).toEqual(['COSGN00C', 'COADM01C']);
      expect(b.json.nextTransid).toBe('CA00');
      expect((await post(`/api/terminal/${a.json.terminalId}`, { aid: 'PF99' })).status).toBe(400);
      expect((await fetch(base + '/')).headers.get('content-type')).toMatch(/text\/html/);
    } finally {
      server.close();
    }
  });
});

describe('review fixes', () => {
  it('rollback keeps non-unique AIX duplicates in their arrival order', () => {
    const { catalog } = loadSeedCatalog();
    const ds = catalog.require(CARDDEMO_CICS_FILES['CARDDAT']!.dataset);
    const first = ds.records()[0]!;
    const extra = Buffer.from(first);
    extra.write('0000000000000001', 0, 'latin1');
    expect(ds.insert(extra).ok).toBe(true);
    const order = () => [...ds.alternateIndexes.values()].map((idx) => idx.entries().map((e) => `${e.key}/${e.primary}`));
    const before = order();
    const files = new TaskFiles(catalog, CARDDEMO_CICS_FILES, new Set(['CARDDAT']));
    const another = Buffer.from(first);
    another.write('0000000000000002', 0, 'latin1');
    expect(files.write({ file: 'CARDDAT', ridfld: '0000000000000002', from: another }).resp).toBe(0);
    files.rollback();
    expect(order()).toEqual(before);
  });

  it('startTransaction with an invalid AID leaves the pending conversation intact', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    rt.startTransaction('T1', 'CC00');
    expect(() => rt.startTransaction('T1', 'CC00', { aid: 'PF99' })).toThrow();
    expect(rt.snapshot('T1').nextTransid).toBe('CC00');
  });

  it('DRK fields are blanked in snapshots but still retransmitted (FSET)', () => {
    const rt = runtime();
    rt.openTerminal('T1');
    rt.startTransaction('T1', 'CC00');
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { USERID: 'USER0001', PASSWD: 'SECRET' } });
    const pwd = r.screen.fields.find((f) => f.name === 'PASSWD')!;
    expect(pwd.value).toBe(' '.repeat(8));
    expect(JSON.stringify(r)).not.toContain('SECRET');
    const again = rt.sendInput('T1', { aid: 'ENTER', fields: { USERID: 'USER0001' } });
    expect(again.screen.fields.find((f) => f.name === 'ERRMSG')!.value.trimEnd()).toBe('Wrong Password. Try again ...');
  });

  it('HTTP rejects oversized bodies with 413', async () => {
    const server = createOnlineServer(runtime());
    await new Promise<void>((r) => server.listen(0, r));
    const base = `http://127.0.0.1:${(server.address() as AddressInfo).port}`;
    try {
      const res = await fetch(`${base}/api/transaction/CC00`, { method: 'POST', body: JSON.stringify({ fields: { X: 'x'.repeat(100_000) } }) });
      expect(res.status).toBe(413);
    } finally {
      server.close();
    }
  });
});
