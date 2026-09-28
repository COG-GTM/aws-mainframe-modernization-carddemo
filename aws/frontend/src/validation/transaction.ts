import type { NewTransaction } from '../api/types';
import { blank, isDigits, isValidIsoDate, type ValidationError } from './common';

export interface TransactionAddForm {
  acctId: string;
  cardNum: string;
  typeCd: string;
  catCd: string;
  source: string;
  description: string;
  amt: string;
  origDate: string;
  procDate: string;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  confirm: string;
}

export type TransactionAddField = keyof TransactionAddForm;

export const EMPTY_TRANSACTION_FORM: TransactionAddForm = {
  acctId: '',
  cardNum: '',
  typeCd: '',
  catCd: '',
  source: '',
  description: '',
  amt: '',
  origDate: '',
  procDate: '',
  merchantId: '',
  merchantName: '',
  merchantCity: '',
  merchantZip: '',
  confirm: '',
};

export const CONFIRM_ADD = 'Confirm to add this transaction...';
export const INVALID_YN = 'Invalid value. Valid values are (Y/N)...';

/** Amount as keyed on COTRN2A: `-99999999.99`; the sign and leading zeros may be omitted. */
const AMOUNT = /^[+-]?\d{1,8}\.\d{2}$/;
const ISO = /^\d{4}-\d{2}-\d{2}$/;

/** COTRN02C VALIDATE-INPUT-KEY-FIELDS + VALIDATE-INPUT-DATA-FIELDS (first error wins). */
export function validateTransactionAdd(f: TransactionAddForm): ValidationError<TransactionAddField> | null {
  if (!blank(f.acctId)) {
    if (!isDigits(f.acctId)) return { field: 'acctId', message: 'Account ID must be Numeric...' };
  } else if (!blank(f.cardNum)) {
    if (!isDigits(f.cardNum)) return { field: 'cardNum', message: 'Card Number must be Numeric...' };
  } else {
    return { field: 'acctId', message: 'Account or Card Number must be entered...' };
  }

  const required: [TransactionAddField, string][] = [
    ['typeCd', 'Type CD'],
    ['catCd', 'Category CD'],
    ['source', 'Source'],
    ['description', 'Description'],
    ['amt', 'Amount'],
    ['origDate', 'Orig Date'],
    ['procDate', 'Proc Date'],
    ['merchantId', 'Merchant ID'],
    ['merchantName', 'Merchant Name'],
    ['merchantCity', 'Merchant City'],
    ['merchantZip', 'Merchant Zip'],
  ];
  for (const [field, label] of required) {
    if (blank(f[field])) return { field, message: `${label} can NOT be empty...` };
  }
  if (!isDigits(f.typeCd)) return { field: 'typeCd', message: 'Type CD must be Numeric...' };
  if (!isDigits(f.catCd)) return { field: 'catCd', message: 'Category CD must be Numeric...' };
  if (!AMOUNT.test(f.amt.trim())) return { field: 'amt', message: 'Amount should be in format -99999999.99' };
  if (!ISO.test(f.origDate.trim())) return { field: 'origDate', message: 'Orig Date should be in format YYYY-MM-DD' };
  if (!ISO.test(f.procDate.trim())) return { field: 'procDate', message: 'Proc Date should be in format YYYY-MM-DD' };
  if (!isValidIsoDate(f.origDate)) return { field: 'origDate', message: 'Orig Date - Not a valid date...' };
  if (!isValidIsoDate(f.procDate)) return { field: 'procDate', message: 'Proc Date - Not a valid date...' };
  if (!isDigits(f.merchantId)) return { field: 'merchantId', message: 'Merchant ID must be Numeric...' };
  return null;
}

export function toNewTransaction(f: TransactionAddForm): NewTransaction {
  const amt = Number(f.amt.trim()).toFixed(2);
  return {
    acctId: blank(f.acctId) ? null : Number(f.acctId),
    cardNum: blank(f.acctId) ? f.cardNum.trim() : null,
    typeCd: f.typeCd.trim().padStart(2, '0'),
    catCd: Number(f.catCd),
    source: f.source.trim(),
    description: f.description.trim(),
    amt,
    origDate: f.origDate.trim(),
    procDate: f.procDate.trim(),
    merchantId: Number(f.merchantId),
    merchantName: f.merchantName.trim(),
    merchantCity: f.merchantCity.trim(),
    merchantZip: f.merchantZip.trim(),
  };
}
