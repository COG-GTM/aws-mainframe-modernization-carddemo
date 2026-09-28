import { blank, isAlpha, isDigits } from './common';

export type CardField = 'acctId' | 'cardNum' | 'embossedName' | 'activeStatus' | 'expMon' | 'expYear';

export interface CardKeyError {
  field: 'acctId' | 'cardNum';
  message: string;
}

/** COCRDSLC/COCRDUPC 2210-EDIT-ACCOUNT + 2220-EDIT-CARD. */
export function validateCardKeys(acctId: string, cardNum: string): CardKeyError | null {
  if (blank(acctId) && blank(cardNum)) return { field: 'acctId', message: 'No input received' };
  if (blank(acctId)) return { field: 'acctId', message: 'Account number not provided' };
  if (!isDigits(acctId, 11) || Number(acctId) === 0) {
    return { field: 'acctId', message: 'Account number must be a non zero 11 digit number' };
  }
  if (blank(cardNum)) return { field: 'cardNum', message: 'Card number not provided' };
  if (!isDigits(cardNum, 16) || Number(cardNum) === 0) {
    return { field: 'cardNum', message: 'Card number if supplied must be a 16 digit number' };
  }
  return null;
}

export interface CardEditForm {
  embossedName: string;
  activeStatus: string;
  expMon: string;
  expYear: string;
}

/** COCRDUPC 1230-EDIT-NAME, 1240-EDIT-CARDSTATUS, 1250-EDIT-EXPIRY-MON, 1260-EDIT-EXPIRY-YEAR. */
export function validateCardEdit(f: CardEditForm): { field: CardField; message: string } | null {
  if (blank(f.embossedName)) return { field: 'embossedName', message: 'Card name not provided' };
  if (!isAlpha(f.embossedName)) return { field: 'embossedName', message: 'Card name can only contain alphabets and spaces' };
  if (!['Y', 'N'].includes(f.activeStatus.trim().toUpperCase())) {
    return { field: 'activeStatus', message: 'Card Active Status must be Y or N' };
  }
  const mon = Number(f.expMon);
  if (!isDigits(f.expMon) || mon < 1 || mon > 12) {
    return { field: 'expMon', message: 'Card expiry month must be between 1 and 12' };
  }
  const year = Number(f.expYear);
  if (!isDigits(f.expYear, 4) || year < 1950 || year > 2099) return { field: 'expYear', message: 'Invalid card expiry year' };
  return null;
}

/** COCRDLIC 2210-EDIT-ACCOUNT / 2220-EDIT-CARD filters (optional). */
export function validateCardFilters(acctId: string, cardNum: string): { field: 'acctId' | 'cardNum'; message: string } | null {
  if (!blank(acctId) && (!isDigits(acctId, 11) || Number(acctId) === 0)) {
    return { field: 'acctId', message: 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER' };
  }
  if (!blank(cardNum) && (!isDigits(cardNum, 16) || Number(cardNum) === 0)) {
    return { field: 'cardNum', message: 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER' };
  }
  return null;
}
