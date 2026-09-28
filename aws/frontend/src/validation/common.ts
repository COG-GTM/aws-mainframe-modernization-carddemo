export interface ValidationError<F extends string = string> {
  field: F;
  message: string;
}

export const blank = (v: string | undefined | null) => v === undefined || v === null || v.trim() === '';
export const isDigits = (v: string, len?: number) => (len ? new RegExp(`^\\d{${len}}$`) : /^\d+$/).test(v.trim());
export const isAlpha = (v: string) => /^[A-Za-z ]+$/.test(v.trim());

export function isLeapYear(y: number): boolean {
  return (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0;
}

export function daysInMonth(y: number, m: number): number {
  return [31, isLeapYear(y) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1];
}

/** Calendar validity of yyyy-MM-dd (the LE CEEDAYS check done by CSUTLDTC). */
export function isValidIsoDate(value: string): boolean {
  const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value.trim());
  if (!m) return false;
  const [y, mo, d] = [Number(m[1]), Number(m[2]), Number(m[3])];
  return y >= 1 && mo >= 1 && mo <= 12 && d >= 1 && d <= daysInMonth(y, mo);
}
