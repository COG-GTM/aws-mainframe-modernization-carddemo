import { Decimal, ZERO } from './decimal.ts';
import { AbendError } from './errors.ts';
import { computeMonthlyInterest } from './interest.ts';
import type { AccountStore, DiscgrpStore, TransactionWriter, XrefStore } from './io.ts';
import {
  formatTran,
  formatTranCatBal,
  unsignedDisplay,
  type AccountRecord,
  type CardXrefRecord,
  type DisGroupRecord,
  type TranCatBalRecord,
} from './records.ts';
import { db2FormatTimestamp, systemClock, type Clock } from './timestamp.ts';

export const PARM_DATE_LENGTH = 10;
export const DEFAULT_GROUP_ID = 'DEFAULT';

/**
 * `PARM-DATE PIC X(10)` (`cbl:175-180`).
 *
 * CBACT04C never inspects `PARM-LENGTH` and never validates, converts or
 * range-checks `PARM-DATE`: any ten characters are accepted and the value is
 * used only as the first ten bytes of the generated transaction id. That lack
 * of validation is reproduced deliberately (defect D5) — this function only
 * makes the fixed-width handling explicit instead of accidental: a shorter PARM
 * is space padded (rather than leaving undefined storage) and a longer one is
 * truncated to the field width.
 */
export function normalizeParmDate(raw: string): string {
  return raw.padEnd(PARM_DATE_LENGTH, ' ').slice(0, PARM_DATE_LENGTH);
}

export interface Cbact04cOptions {
  /** Raw PARM string from the JCL `EXEC` card, e.g. `2022071800`. */
  parmDate: string;
  tranCatBalRecords: readonly TranCatBalRecord[];
  accounts: AccountStore;
  xref: XrefStore;
  discgrp: DiscgrpStore;
  transactions: TransactionWriter;
  clock?: Clock | undefined;
  /** Receives the program's `DISPLAY` output (`SYSOUT`). */
  display?: ((line: string) => void) | undefined;
}

export interface Cbact04cResult {
  /** `WS-RECORD-COUNT` — computed but never reported by the COBOL (defect D11). */
  recordCount: number;
  transactionsWritten: number;
  accountsUpdated: number;
  /** Account whose accumulated interest was never applied because of defect D1. */
  lastAccountNotUpdated: string | undefined;
  totalInterestNotApplied: Decimal;
}

interface AccountContext {
  account: AccountRecord;
  xref: CardXrefRecord;
}

/**
 * `1400-COMPUTE-FEES` (`cbl:518-520`) is an empty paragraph in the COBOL — the
 * job advertises "interest and fees" but no fee is ever calculated, accumulated
 * or posted (defect D2). Kept as an explicit, documented extension point; no
 * fee logic is invented here.
 */
function computeFees(): void {
  // Intentionally empty: "To be implemented" in CBACT04C.
}

/** Faithful port of `CBACT04C` — see `docs/specs/INTCALC-CBACT04C-business-spec.md`. */
export function runCbact04c(options: Cbact04cOptions): Cbact04cResult {
  const clock = options.clock ?? systemClock;
  const display = options.display ?? ((): void => {});
  const parmDate = normalizeParmDate(options.parmDate);

  display('START OF EXECUTION OF PROGRAM CBACT04C');

  let recordCount = 0;
  let tranIdSuffix = 0;
  let accountsUpdated = 0;
  let lastAcctNum = '';
  let firstTime = true;
  let totalInterest = ZERO;
  let context: AccountContext | undefined;

  const updateAccount = (ctx: AccountContext, accumulated: Decimal): void => {
    // 1050-UPDATE-ACCOUNT (cbl:350-370)
    const updated: AccountRecord = {
      ...ctx.account,
      currBal: ctx.account.currBal.plus(accumulated),
      currCycCredit: ZERO,
      currCycDebit: ZERO,
    };
    options.accounts.rewrite(updated);
    ctx.account = updated;
    accountsUpdated += 1;
  };

  const getInterestRate = (account: AccountRecord, balance: TranCatBalRecord): DisGroupRecord => {
    // 1200-GET-INTEREST-RATE (cbl:415-440)
    const key = {
      acctGroupId: account.groupId,
      tranTypeCd: balance.tranTypeCd,
      tranCatCd: balance.tranCatCd,
    };
    const record = options.discgrp.read(key);
    if (record !== undefined) {
      return record;
    }
    display('DISCLOSURE GROUP RECORD MISSING');
    display('TRY WITH DEFAULT GROUP CODE');

    // 1200-A-GET-DEFAULT-INT-RATE (cbl:443-460). The COBOL leaves the previous
    // record in the buffer on a miss (defect D4); a stale rate is never reused
    // here — a missing DEFAULT row abends, as it does on the mainframe (D3).
    const fallback = options.discgrp.read({ ...key, acctGroupId: DEFAULT_GROUP_ID });
    if (fallback === undefined) {
      throw new AbendError('ERROR READING DEFAULT DISCLOSURE GROUP');
    }
    return fallback;
  };

  const writeTransaction = (ctx: AccountContext, monthlyInterest: Decimal): void => {
    // 1300-B-WRITE-TX (cbl:473-515)
    tranIdSuffix += 1;
    const acctId = unsignedDisplay(ctx.account.acctId, 11);
    const timestamp = db2FormatTimestamp(clock());
    options.transactions.write(
      formatTran({
        tranId: `${parmDate}${String(tranIdSuffix % 1_000_000).padStart(6, '0')}`,
        tranTypeCd: '01',
        tranCatCd: '05',
        tranSource: 'System',
        tranDesc: `Int. for a/c ${acctId}`,
        tranAmt: monthlyInterest,
        merchantId: '0',
        merchantName: '',
        merchantCity: '',
        merchantZip: '',
        cardNum: ctx.xref.cardNum,
        origTs: timestamp,
        procTs: timestamp,
        filler: '',
      }),
    );
  };

  for (const balance of options.tranCatBalRecords) {
    // 1000-TCATBALF-GET-NEXT + main loop body (cbl:188-222)
    recordCount += 1;
    display(formatTranCatBal(balance));

    if (balance.acctId !== lastAcctNum) {
      if (!firstTime && context !== undefined) {
        updateAccount(context, totalInterest);
      } else {
        firstTime = false;
      }
      totalInterest = ZERO;
      lastAcctNum = balance.acctId;
      const account = options.accounts.read(balance.acctId);
      const xref = options.xref.read(balance.acctId);
      context = { account, xref };
    }

    if (context === undefined) {
      throw new AbendError('ERROR READING ACCOUNT FILE');
    }

    const rateRecord = getInterestRate(context.account, balance);
    if (!rateRecord.intRate.isZero()) {
      // Zero rate short-circuits interest, the transaction and the fee stub
      // (cbl:214-217, defect D8).
      const monthlyInterest = computeMonthlyInterest(balance.balance, rateRecord.intRate);
      totalInterest = totalInterest.plus(monthlyInterest);
      writeTransaction(context, monthlyInterest);
      computeFees();
    }
  }

  // Defect D1 (cbl:219-220): the `ELSE PERFORM 1050-UPDATE-ACCOUNT` branch is
  // unreachable — it can only run once END-OF-FILE = 'Y', a state in which the
  // PERFORM UNTIL has already terminated. The last account of every run
  // therefore keeps its old balance and its un-zeroed cycle buckets even though
  // its interest transactions were written. Reproduced on purpose; pinned by
  // test/d1-last-account.test.ts. Awaiting a business decision before it is
  // "fixed" (spec §11.1).

  display('END OF EXECUTION OF PROGRAM CBACT04C');

  return {
    recordCount,
    transactionsWritten: tranIdSuffix,
    accountsUpdated,
    lastAccountNotUpdated: lastAcctNum === '' ? undefined : lastAcctNum,
    totalInterestNotApplied: totalInterest,
  };
}
