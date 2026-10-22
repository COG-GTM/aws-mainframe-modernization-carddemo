import { describe, expect, it } from 'vitest';
import { ceedays, CEEDAYS_FC, cobswait, csutldtc, parseWaitParm, toLillian } from '../src/common/utils/index.js';

describe('CEEDAYS', () => {
  it('computes Lillian day numbers', () => {
    expect(toLillian(1582, 10, 15)).toBe(1);
    expect(ceedays('1582-10-15', 'YYYY-MM-DD').lillian).toBe(1);
    expect(ceedays('2000-01-01', 'YYYY-MM-DD').lillian).toBe(152385);
    expect(ceedays('20000101', 'YYYYMMDD').lillian).toBe(152385);
    expect(ceedays('2000001', 'YYYYDDD').lillian).toBe(152385);
  });
  it('reports feedback codes', () => {
    expect(ceedays('2023-02-29', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.BAD_DATE_VALUE);
    expect(ceedays('2024-02-29', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.OK);
    expect(ceedays('2024-13-01', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.INVALID_MONTH);
    expect(ceedays('2024-1A-01', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.NON_NUMERIC_DATA);
    expect(ceedays('1500-01-01', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.UNSUPP_RANGE);
    expect(ceedays('1582-10-14', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.UNSUPP_RANGE);
    expect(ceedays('          ', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.INSUFFICIENT_DATA);
    expect(ceedays('2024-01-01', 'QQQQ-MM-DD').feedback).toBe(CEEDAYS_FC.BAD_PIC_STRING);
  });
});

describe('CEEDAYS separators', () => {
  it('rejects separators that do not match the picture string', () => {
    expect(ceedays('2024/02/29', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.BAD_DATE_VALUE);
    expect(ceedays('2024-02', 'YYYY-MM-DD').feedback).toBe(CEEDAYS_FC.INSUFFICIENT_DATA);
    expect(csutldtc('2024/02/29', 'YYYY-MM-DD').msgNo).toBe('2508');
  });
});

describe('CSUTLDTC', () => {
  it('valid date: severity 0000, 80-byte message, RETURN-CODE 0', () => {
    const r = csutldtc('2024-02-29', 'YYYY-MM-DD');
    expect(r.result.length).toBe(80);
    expect(r.returnCode).toBe(0);
    expect(r.result.slice(0, 4)).toBe('0000');
    expect(r.result.slice(4, 15)).toBe('Mesg Code: ');
    expect(r.result.slice(15, 19)).toBe('0000');
    expect(r.result.slice(20, 35)).toBe('Date is valid  ');
    expect(r.result.slice(36, 45)).toBe('TstDate: ');
    // WS-DATE receives the VSTRING group: binary length X'000A' + first 8 chars.
    expect(r.result.slice(45, 55)).toBe('\x00\x0a2024-02-');
    expect(r.result.slice(56, 66)).toBe('Mask used:');
    expect(r.result.slice(66, 76)).toBe('YYYY-MM-DD');
  });
  it.each([
    ['2023-02-29', '0003', '2508', 'Datevalue error'],
    ['2023-13-01', '0003', '2517', 'Invalid month  '],
    ['20X3-01-01', '0003', '2520', 'Nonnumeric data'],
    ['1066-10-14', '0003', '2513', 'Unsupp. Range  '],
    ['          ', '0003', '2507', 'Insufficient   '],
  ])('%s -> sev %s msg %s %s', (date, sev, msg, text) => {
    const r = csutldtc(date, 'YYYY-MM-DD');
    expect([r.severity, r.msgNo, r.message, r.returnCode]).toEqual([sev, msg, text, 3]);
    expect(r.result.slice(0, 4) + r.result.slice(15, 19)).toBe(sev + msg);
  });
  it('unknown feedback -> Date is invalid', () => {
    const r = csutldtc('2024-01-01', 'YYYY-MM-DD', () => ({ lillian: 0, feedback: { severity: 2, msgNo: 9999, symbol: 'X' } }));
    expect([r.message, r.returnCode]).toEqual(['Date is invalid', 2]);
  });
});

describe('COBSWAIT', () => {
  it('parses SYSIN centiseconds like MOVE X(8) TO 9(8) COMP', () => {
    expect(parseWaitParm('00003600 VALUE IN CENTISECONDS')).toBe(3600);
    expect(parseWaitParm('12')).toBe(12000000);
    expect(parseWaitParm('')).toBe(0);
  });
  it('waits centiseconds * 10 ms', async () => {
    const waits: number[] = [];
    expect(await cobswait('00000250', async (ms) => void waits.push(ms))).toBe(250);
    expect(waits).toEqual([2500]);
    const t = Date.now();
    await cobswait('00000002');
    expect(Date.now() - t).toBeGreaterThanOrEqual(15);
  });
});
