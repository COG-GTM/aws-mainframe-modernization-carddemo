import type { Account, AccountUpdate } from '../api/types';
import { blank, daysInMonth, isAlpha, isDigits, isLeapYear, type ValidationError } from './common';
import lookups from './lookups.json';

/** Screen image of map CACTUPA (COACTUP.bms); every input is a string exactly as typed. */
export interface AccountForm {
  acctId: string;
  activeStatus: string;
  openYear: string;
  openMon: string;
  openDay: string;
  creditLimit: string;
  expYear: string;
  expMon: string;
  expDay: string;
  cashCreditLimit: string;
  risYear: string;
  risMon: string;
  risDay: string;
  currBal: string;
  currCycCredit: string;
  currCycDebit: string;
  groupId: string;
  custId: string;
  ssn1: string;
  ssn2: string;
  ssn3: string;
  dobYear: string;
  dobMon: string;
  dobDay: string;
  fico: string;
  firstName: string;
  middleName: string;
  lastName: string;
  addrLine1: string;
  addrStateCd: string;
  addrLine2: string;
  addrZip: string;
  addrLine3: string;
  addrCountryCd: string;
  ph1a: string;
  ph1b: string;
  ph1c: string;
  ph2a: string;
  ph2b: string;
  ph2c: string;
  govtIssuedId: string;
  eftAccountId: string;
  priCardHolderInd: string;
}

export type AccountField = keyof AccountForm;
export type AccountError = ValidationError<AccountField>;

export const NO_CHANGE = 'No change detected with respect to values fetched.';


const splitDate = (iso: string) => {
  const [y = '', m = '', d = ''] = (iso ?? '').slice(0, 10).split('-');
  return [y, m, d] as const;
};

const splitPhone = (phone: string) => {
  const m = /^\((\d{3})\)(\d{3})-(\d{4})/.exec((phone ?? '').trim());
  if (m) return [m[1], m[2], m[3]] as const;
  const d = (phone ?? '').replace(/\D/g, '');
  return [d.slice(0, 3), d.slice(3, 6), d.slice(6, 10)] as const;
};

export function toForm(a: Account): AccountForm {
  const c = a.customer;
  const [openYear, openMon, openDay] = splitDate(a.openDate);
  const [expYear, expMon, expDay] = splitDate(a.expirationDate);
  const [risYear, risMon, risDay] = splitDate(a.reissueDate);
  const [dobYear, dobMon, dobDay] = splitDate(c.dob);
  const [ph1a, ph1b, ph1c] = splitPhone(c.phoneNum1);
  const [ph2a, ph2b, ph2c] = splitPhone(c.phoneNum2);
  const ssn = (c.ssn ?? '').replace(/\D/g, '').padStart(9, '0');
  return {
    acctId: String(a.acctId).padStart(11, '0'),
    activeStatus: a.activeStatus,
    openYear,
    openMon,
    openDay,
    creditLimit: a.creditLimit,
    expYear,
    expMon,
    expDay,
    cashCreditLimit: a.cashCreditLimit,
    risYear,
    risMon,
    risDay,
    currBal: a.currBal,
    currCycCredit: a.currCycCredit,
    currCycDebit: a.currCycDebit,
    groupId: a.groupId ?? '',
    custId: String(c.custId).padStart(9, '0'),
    ssn1: ssn.slice(0, 3),
    ssn2: ssn.slice(3, 5),
    ssn3: ssn.slice(5, 9),
    dobYear,
    dobMon,
    dobDay,
    fico: String(c.ficoCreditScore ?? ''),
    firstName: c.firstName,
    middleName: c.middleName ?? '',
    lastName: c.lastName,
    addrLine1: c.addrLine1,
    addrStateCd: c.addrStateCd,
    addrLine2: c.addrLine2 ?? '',
    addrZip: c.addrZip.slice(0, 5),
    addrLine3: c.addrLine3 ?? '',
    addrCountryCd: c.addrCountryCd,
    ph1a,
    ph1b,
    ph1c,
    ph2a,
    ph2b,
    ph2c,
    govtIssuedId: c.govtIssuedId ?? '',
    eftAccountId: c.eftAccountId,
    priCardHolderInd: c.priCardHolderInd,
  };
}

const iso = (y: string, m: string, d: string) => `${y.trim()}-${m.trim().padStart(2, '0')}-${d.trim().padStart(2, '0')}`;
const phone = (a: string, b: string, c: string) => (blank(a) && blank(b) && blank(c) ? '' : `(${a.trim()})${b.trim()}-${c.trim()}`);
/** NUMVAL-C semantics for the formats accepted by `signed9v2`: trailing `+`/`-`/`CR`/`DB` sign. */
export const decimal = (v: string): string => {
  const s = v.replace(/[,$\s]/g, '').toUpperCase();
  const sign = /(CR|DB|[+-])$/.exec(s)?.[0];
  const digits = sign ? s.slice(0, -sign.length) : s;
  const negative = sign === '-' || sign === 'CR' || sign === 'DB';
  const n = Number(negative ? `-${digits.replace(/^[+-]/, '')}` : digits);
  if (!Number.isFinite(n)) throw new Error(`Not a valid amount: ${v}`);
  return n.toFixed(2);
};

export function fromForm(f: AccountForm, original: Account): AccountUpdate {
  return {
    activeStatus: f.activeStatus.trim().toUpperCase(),
    currBal: decimal(f.currBal),
    creditLimit: decimal(f.creditLimit),
    cashCreditLimit: decimal(f.cashCreditLimit),
    openDate: iso(f.openYear, f.openMon, f.openDay),
    expirationDate: iso(f.expYear, f.expMon, f.expDay),
    reissueDate: iso(f.risYear, f.risMon, f.risDay),
    currCycCredit: decimal(f.currCycCredit),
    currCycDebit: decimal(f.currCycDebit),
    groupId: f.groupId.trim(),
    version: original.version,
    customer: {
      firstName: f.firstName.trim(),
      middleName: f.middleName.trim(),
      lastName: f.lastName.trim(),
      addrLine1: f.addrLine1.trim(),
      addrLine2: f.addrLine2.trim(),
      addrLine3: f.addrLine3.trim(),
      addrStateCd: f.addrStateCd.trim().toUpperCase(),
      addrCountryCd: f.addrCountryCd.trim().toUpperCase(),
      // the map shows 5 characters; keep a stored ZIP+4 unless the visible part was edited
      addrZip: f.addrZip.trim() === original.customer.addrZip.slice(0, 5) ? original.customer.addrZip : f.addrZip.trim(),
      phoneNum1: phone(f.ph1a, f.ph1b, f.ph1c),
      phoneNum2: phone(f.ph2a, f.ph2b, f.ph2c),
      ssn: `${f.ssn1}${f.ssn2}${f.ssn3}`,
      govtIssuedId: f.govtIssuedId.trim(),
      dob: iso(f.dobYear, f.dobMon, f.dobDay),
      eftAccountId: f.eftAccountId.trim(),
      priCardHolderInd: f.priCardHolderInd.trim().toUpperCase(),
      ficoCreditScore: Number(f.fico),
      version: original.customer.version,
    },
  };
}

/** 1205-COMPARE-OLD-NEW: case-insensitive, trimmed comparison of every editable field. */
export function hasChanges(current: AccountForm, original: AccountForm): boolean {
  return (Object.keys(original) as AccountField[]).some(
    (k) => current[k].trim().toUpperCase() !== original[k].trim().toUpperCase(),
  );
}

class Edits {
  error: AccountError | null = null;

  private fail(field: AccountField, message: string): false {
    if (!this.error) this.error = { field, message };
    return false;
  }

  /** 1215-EDIT-MANDATORY */
  mandatory(field: AccountField, name: string, v: string): boolean {
    return blank(v) ? this.fail(field, `${name} must be supplied.`) : true;
  }

  /** 1220-EDIT-YESNO */
  yesNo(field: AccountField, name: string, v: string): boolean {
    if (blank(v) || v.trim() === '0') return this.fail(field, `${name} must be supplied.`);
    return ['Y', 'N'].includes(v.trim().toUpperCase()) ? true : this.fail(field, `${name} must be Y or N.`);
  }

  /** 1225-EDIT-ALPHA-REQD */
  alphaReqd(field: AccountField, name: string, v: string): boolean {
    if (blank(v)) return this.fail(field, `${name} must be supplied.`);
    return isAlpha(v) ? true : this.fail(field, `${name} can have alphabets only.`);
  }

  /** 1235-EDIT-ALPHA-OPT */
  alphaOpt(field: AccountField, name: string, v: string): boolean {
    if (blank(v)) return true;
    return isAlpha(v) ? true : this.fail(field, `${name} can have alphabets only.`);
  }

  /** 1245-EDIT-NUM-REQD */
  numReqd(field: AccountField, name: string, v: string): boolean {
    if (blank(v)) return this.fail(field, `${name} must be supplied.`);
    if (!isDigits(v)) return this.fail(field, `${name} must be all numeric.`);
    return Number(v) === 0 ? this.fail(field, `${name} must not be zero.`) : true;
  }

  /** 1250-EDIT-SIGNED-9V2 (FUNCTION TEST-NUMVAL-C) */
  signed9v2(field: AccountField, name: string, v: string): boolean {
    if (blank(v)) return this.fail(field, `${name} must be supplied.`);
    const s = v.replace(/[,$\s]/g, '');
    const ok = /^[+-]?(\d+(\.\d*)?|\.\d+)$/.test(s) || /^(\d+(\.\d*)?|\.\d+)(CR|DB|[+-])$/i.test(s);
    return ok ? true : this.fail(field, `${name} is not valid`);
  }

  /** EDIT-DATE-CCYYMMDD (CSUTLDPY) + EDIT-DATE-OF-BIRTH */
  date(fields: [AccountField, AccountField, AccountField], name: string, y: string, m: string, d: string, dob = false): boolean {
    const [fy, fm, fd] = fields;
    if (blank(y)) return this.fail(fy, `${name} : Year must be supplied.`);
    if (!isDigits(y, 4)) return this.fail(fy, `${name} must be 4 digit number.`);
    if (!['19', '20'].includes(y.slice(0, 2))) return this.fail(fy, `${name} : Century is not valid.`);
    if (blank(m)) return this.fail(fm, `${name} : Month must be supplied.`);
    const mm = Number(m);
    if (!isDigits(m) || mm < 1 || mm > 12) return this.fail(fm, `${name}: Month must be a number between 1 and 12.`);
    if (blank(d)) return this.fail(fd, `${name} : Day must be supplied.`);
    const dd = Number(d);
    if (!isDigits(d) || dd < 1 || dd > 31) return this.fail(fd, `${name}:day must be a number between 1 and 31.`);
    const yy = Number(y);
    if (dd === 31 && daysInMonth(yy, mm) < 31) return this.fail(fd, `${name}:Cannot have 31 days in this month.`);
    if (mm === 2 && dd === 30) return this.fail(fd, `${name}:Cannot have 30 days in this month.`);
    if (mm === 2 && dd === 29 && !isLeapYear(yy)) {
      return this.fail(fd, `${name}:Not a leap year.Cannot have 29 days in this month.`);
    }
    if (dob) {
      const today = new Date();
      const value = new Date(yy, mm - 1, dd);
      if (value.getTime() > today.getTime()) return this.fail(fy, `${name}:cannot be in the future `);
    }
    return true;
  }

  /** 1260-EDIT-US-PHONE-NUM (optional; all three parts blank is valid) */
  phone(fields: [AccountField, AccountField, AccountField], name: string, a: string, b: string, c: string): boolean {
    if (blank(a) && blank(b) && blank(c)) return true;
    const parts: [AccountField, string, string, number][] = [
      [fields[0], a, 'Area code', 3],
      [fields[1], b, 'Prefix code', 3],
      [fields[2], c, 'Line number code', 4],
    ];
    for (const [field, v, label, len] of parts) {
      if (blank(v)) return this.fail(field, `${name}: ${label} must be supplied.`);
      if (!isDigits(v, len)) return this.fail(field, `${name}: ${label} must be A ${len} digit number.`);
      if (Number(v) === 0) return this.fail(field, `${name}: ${label} cannot be zero`);
      if (label === 'Area code' && !lookups.generalPurposeAreaCodes.includes(v)) {
        return this.fail(field, `${name}: Not valid North America general purpose area code`);
      }
    }
    return true;
  }
}

/**
 * Field edits of COACTUPC 1200-EDIT-MAP-INPUTS, in the legacy order. Like the COBOL program only the
 * first failing edit's message is reported (WS-RETURN-MSG is set once).
 */
export function validateAccountForm(f: AccountForm, original: AccountForm): AccountError | null {
  if (!hasChanges(f, original)) return { field: 'activeStatus', message: NO_CHANGE };

  const e = new Edits();
  e.yesNo('activeStatus', 'Account Status', f.activeStatus);
  e.date(['openYear', 'openMon', 'openDay'], 'Open Date', f.openYear, f.openMon, f.openDay);
  e.signed9v2('creditLimit', 'Credit Limit', f.creditLimit);
  e.date(['expYear', 'expMon', 'expDay'], 'Expiry Date', f.expYear, f.expMon, f.expDay);
  e.signed9v2('cashCreditLimit', 'Cash Credit Limit', f.cashCreditLimit);
  e.date(['risYear', 'risMon', 'risDay'], 'Reissue Date', f.risYear, f.risMon, f.risDay);
  e.signed9v2('currBal', 'Current Balance', f.currBal);
  e.signed9v2('currCycCredit', 'Current Cycle Credit Limit', f.currCycCredit);
  e.signed9v2('currCycDebit', 'Current Cycle Debit Limit', f.currCycDebit);

  // 1265-EDIT-US-SSN
  if (e.numReqd('ssn1', 'SSN: First 3 chars', f.ssn1)) {
    const p1 = Number(f.ssn1);
    if (p1 === 666 || p1 >= 900) {
      e.error ??= { field: 'ssn1', message: 'SSN: First 3 chars: should not be 000, 666, or between 900 and 999' };
    }
  }
  e.numReqd('ssn2', 'SSN 4th & 5th chars', f.ssn2);
  e.numReqd('ssn3', 'SSN Last 4 chars', f.ssn3);

  e.date(['dobYear', 'dobMon', 'dobDay'], 'Date of Birth', f.dobYear, f.dobMon, f.dobDay, true);
  if (e.numReqd('fico', 'FICO Score', f.fico)) {
    const fico = Number(f.fico);
    if (fico < 300 || fico > 850) {
      e.error ??= { field: 'fico', message: 'FICO Score: should be between 300 and 850' };
    }
  }
  e.alphaReqd('firstName', 'First Name', f.firstName);
  e.alphaOpt('middleName', 'Middle Name', f.middleName);
  e.alphaReqd('lastName', 'Last Name', f.lastName);
  e.mandatory('addrLine1', 'Address Line 1', f.addrLine1);
  if (e.alphaReqd('addrStateCd', 'State', f.addrStateCd) && !lookups.usStateCodes.includes(f.addrStateCd.trim().toUpperCase())) {
    e.error ??= { field: 'addrStateCd', message: 'State: is not a valid state code' };
  }
  e.numReqd('addrZip', 'Zip', f.addrZip);
  e.alphaReqd('addrLine3', 'City', f.addrLine3);
  e.alphaReqd('addrCountryCd', 'Country', f.addrCountryCd);
  e.phone(['ph1a', 'ph1b', 'ph1c'], 'Phone Number 1', f.ph1a, f.ph1b, f.ph1c);
  e.phone(['ph2a', 'ph2b', 'ph2c'], 'Phone Number 2', f.ph2a, f.ph2b, f.ph2c);
  e.numReqd('eftAccountId', 'EFT Account Id', f.eftAccountId);
  e.yesNo('priCardHolderInd', 'Primary Card Holder', f.priCardHolderInd);

  // 1280-EDIT-US-STATE-ZIP-CD
  if (!e.error) {
    const combo = `${f.addrStateCd.trim().toUpperCase()}${f.addrZip.trim().slice(0, 2)}`;
    if (!lookups.usStateZip2Combos.includes(combo)) e.error = { field: 'addrZip', message: 'Invalid zip code for state' };
  }
  return e.error;
}
