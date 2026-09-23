import { describe, expect, it } from 'vitest';

import { normalizeParmDate, runCbact04c } from '../src/cbact04c.ts';
import { AbendError } from '../src/errors.ts';
import { parseTran } from '../src/records.ts';
import { db2FormatTimestamp } from '../src/timestamp.ts';
import {
  accountRecord,
  balanceRecord,
  discgrpRecord,
  fixedClock,
  harness,
  xrefRecord,
} from './helpers.ts';

const CARD = '4111111111111111';

describe('interest rate lookup', () => {
  it('uses the account-group row when it exists', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '100.00')],
      [xrefRecord('1', CARD)],
      [discgrpRecord('A000000000', '01', '0001', '12.00'), discgrpRecord('DEFAULT', '01', '0001', '24.00')],
    );
    runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [balanceRecord('1', '01', '0001', '1000.00')],
      clock: fixedClock,
      ...h,
    });
    expect(parseTran(h.transactions.records[0] ?? '').tranAmt.toFixed(2)).toBe('10.00');
  });

  it('falls back to the DEFAULT group on record-not-found', () => {
    const displayed: string[] = [];
    const h = harness(
      [accountRecord('1', 'NOSUCHGRP', '100.00')],
      [xrefRecord('1', CARD)],
      [discgrpRecord('DEFAULT', '01', '0001', '24.00')],
    );
    runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [balanceRecord('1', '01', '0001', '1000.00')],
      clock: fixedClock,
      display: (line) => displayed.push(line),
      ...h,
    });
    expect(displayed).toContain('DISCLOSURE GROUP RECORD MISSING');
    expect(displayed).toContain('TRY WITH DEFAULT GROUP CODE');
    expect(parseTran(h.transactions.records[0] ?? '').tranAmt.toFixed(2)).toBe('20.00');
  });

  it('abends when the DEFAULT row is missing too', () => {
    const h = harness(
      [accountRecord('1', 'NOSUCHGRP', '100.00')],
      [xrefRecord('1', CARD)],
      [discgrpRecord('DEFAULT', '02', '0001', '24.00')],
    );
    expect(() =>
      runCbact04c({
        parmDate: '2022071800',
        tranCatBalRecords: [balanceRecord('1', '01', '0001', '1000.00')],
        clock: fixedClock,
        ...h,
      }),
    ).toThrowError(new AbendError('ERROR READING DEFAULT DISCLOSURE GROUP'));
  });

  it('never reuses a stale rate from a previous lookup (defect D4)', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '100.00')],
      [xrefRecord('1', CARD)],
      [discgrpRecord('A000000000', '01', '0001', '12.00')],
    );
    expect(() =>
      runCbact04c({
        parmDate: '2022071800',
        tranCatBalRecords: [
          balanceRecord('1', '01', '0001', '1000.00'),
          balanceRecord('1', '01', '0002', '1000.00'),
        ],
        clock: fixedClock,
        ...h,
      }),
    ).toThrowError(AbendError);
    expect(h.transactions.records).toHaveLength(1);
  });
});

describe('zero rate', () => {
  it('writes no transaction and accrues nothing', () => {
    const h = harness(
      [accountRecord('1', 'ZEROAPR', '100.00'), accountRecord('2', 'ZEROAPR', '50.00')],
      [xrefRecord('1', CARD), xrefRecord('2', CARD)],
      [discgrpRecord('ZEROAPR', '01', '0001', '0.00')],
    );
    const result = runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [
        balanceRecord('1', '01', '0001', '1000.00'),
        balanceRecord('2', '01', '0001', '1000.00'),
      ],
      clock: fixedClock,
      ...h,
    });
    expect(h.transactions.records).toHaveLength(0);
    expect(result.accountsUpdated).toBe(1);
    expect(h.accounts.read('1').currBal.toFixed(2)).toBe('100.00');
  });
});

describe('transaction attributes', () => {
  it('composes TRAN-ID from the PARM and a run-sequential counter', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '0.00')],
      [xrefRecord('1', CARD)],
      [
        discgrpRecord('A000000000', '01', '0001', '12.00'),
        discgrpRecord('A000000000', '01', '0002', '12.00'),
      ],
    );
    runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [
        balanceRecord('1', '01', '0001', '1000.00'),
        balanceRecord('1', '01', '0002', '1000.00'),
      ],
      clock: fixedClock,
      ...h,
    });
    const ids = h.transactions.records.map((image) => parseTran(image).tranId);
    expect(ids).toEqual(['2022071800000001', '2022071800000002']);
  });

  it('sets the remaining attributes exactly as 1300-B-WRITE-TX does', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '0.00')],
      [xrefRecord('1', CARD)],
      [discgrpRecord('A000000000', '01', '0001', '12.00')],
    );
    runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [balanceRecord('1', '01', '0001', '1000.00')],
      clock: fixedClock,
      ...h,
    });
    const image = h.transactions.records[0] ?? '';
    expect(image).toHaveLength(350);
    const tran = parseTran(image);
    expect(tran).toMatchObject({
      tranTypeCd: '01',
      tranCatCd: '0005',
      tranSource: 'System    ',
      merchantId: '000000000',
      merchantName: ' '.repeat(50),
      merchantCity: ' '.repeat(50),
      merchantZip: ' '.repeat(10),
      cardNum: CARD,
    });
    expect(tran.tranDesc).toBe('Int. for a/c 00000000001'.padEnd(100, ' '));
    expect(tran.origTs).toBe(db2FormatTimestamp(fixedClock()));
    expect(tran.procTs).toBe(tran.origTs);
    expect(tran.tranAmt.toFixed(2)).toBe('10.00');
  });

  it('accepts any PARM without validation (defect D5)', () => {
    expect(normalizeParmDate('2022071800')).toBe('2022071800');
    expect(normalizeParmDate('not-a-date')).toBe('not-a-date');
    expect(normalizeParmDate('99')).toBe('99        ');
    expect(normalizeParmDate('20220718001234')).toBe('2022071800');
  });
});

describe('account break accumulation', () => {
  it('adds the sum of per-category interest and zeroes the cycle buckets', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '100.00'), accountRecord('2', 'A000000000', '500.00')],
      [xrefRecord('1', CARD), xrefRecord('2', '4222222222222222')],
      [
        discgrpRecord('A000000000', '01', '0001', '1.50'),
        discgrpRecord('A000000000', '01', '0002', '1.50'),
        discgrpRecord('A000000000', '01', '0003', '1.50'),
      ],
    );
    const result = runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [
        balanceRecord('1', '01', '0001', '100.00'),
        balanceRecord('1', '01', '0002', '100.00'),
        balanceRecord('1', '01', '0003', '100.00'),
        balanceRecord('2', '01', '0001', '100.00'),
      ],
      clock: fixedClock,
      ...h,
    });

    // 3 x truncate(100.00 * 1.50 / 1200) = 3 x 0.12 = 0.36
    const updated = h.accounts.read('1');
    expect(updated.currBal.toFixed(2)).toBe('100.36');
    expect(updated.currCycCredit.toFixed(2)).toBe('0.00');
    expect(updated.currCycDebit.toFixed(2)).toBe('0.00');
    expect(result.transactionsWritten).toBe(4);
    expect(result.accountsUpdated).toBe(1);
  });

  it('abends when the account or cross-reference record is missing', () => {
    const noAccount = harness([], [xrefRecord('1', CARD)], [discgrpRecord('DEFAULT', '01', '0001', '1.00')]);
    expect(() =>
      runCbact04c({
        parmDate: '2022071800',
        tranCatBalRecords: [balanceRecord('1', '01', '0001', '100.00')],
        clock: fixedClock,
        ...noAccount,
      }),
    ).toThrowError(AbendError);

    const noXref = harness(
      [accountRecord('1', 'A000000000', '0.00')],
      [],
      [discgrpRecord('A000000000', '01', '0001', '1.00')],
    );
    expect(() =>
      runCbact04c({
        parmDate: '2022071800',
        tranCatBalRecords: [balanceRecord('1', '01', '0001', '100.00')],
        clock: fixedClock,
        ...noXref,
      }),
    ).toThrowError(AbendError);
  });
});

describe('defect D1 — last account of a run is never updated', () => {
  it('writes the interest transactions but leaves the balance and cycle buckets untouched', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '100.00'), accountRecord('2', 'A000000000', '500.00')],
      [xrefRecord('1', CARD), xrefRecord('2', '4222222222222222')],
      [discgrpRecord('A000000000', '01', '0001', '12.00')],
    );
    const result = runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [
        balanceRecord('1', '01', '0001', '1200.00'),
        balanceRecord('2', '01', '0001', '1200.00'),
      ],
      clock: fixedClock,
      ...h,
    });

    expect(h.accounts.read('1').currBal.toFixed(2)).toBe('112.00');

    const last = h.accounts.read('2');
    expect(last.currBal.toFixed(2)).toBe('500.00');
    expect(last.currCycCredit.toFixed(2)).toBe('11.11');
    expect(last.currCycDebit.toFixed(2)).toBe('22.22');

    expect(result.transactionsWritten).toBe(2);
    expect(result.accountsUpdated).toBe(1);
    expect(result.lastAccountNotUpdated).toBe('00000000002');
    expect(result.totalInterestNotApplied.toFixed(2)).toBe('12.00');
  });

  it('updates nothing at all when the run has a single account', () => {
    const h = harness(
      [accountRecord('1', 'A000000000', '100.00')],
      [xrefRecord('1', CARD)],
      [discgrpRecord('A000000000', '01', '0001', '12.00')],
    );
    const result = runCbact04c({
      parmDate: '2022071800',
      tranCatBalRecords: [balanceRecord('1', '01', '0001', '1200.00')],
      clock: fixedClock,
      ...h,
    });
    expect(h.accounts.read('1').currBal.toFixed(2)).toBe('100.00');
    expect(result.accountsUpdated).toBe(0);
  });
});
