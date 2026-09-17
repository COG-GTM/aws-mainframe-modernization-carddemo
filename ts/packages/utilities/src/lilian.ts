/**
 * Lilian day numbering, as used by the Language Environment date services.
 *
 * Lilian day 1 is 15 October 1582, the first day of the Gregorian calendar;
 * 31 December 9999 is Lilian day 3 074 324, the highest value LE supports.
 */

const MS_PER_DAY = 86_400_000;

/** Lilian day number of 15 October 1582, the first representable date. */
export const FIRST_LILIAN_DAY = 1;

/** Lilian day number of 31 December 9999, the last representable date. */
export const LAST_LILIAN_DAY = 3_074_324;

const EPOCH_UTC = Date.UTC(1582, 9, 14);

export interface CalendarDate {
  readonly year: number;
  readonly month: number;
  readonly day: number;
}

export function isLeapYear(year: number): boolean {
  return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
}

export function daysInMonth(year: number, month: number): number {
  if (month === 2) {
    return isLeapYear(year) ? 29 : 28;
  }
  return [4, 6, 9, 11].includes(month) ? 30 : 31;
}

/** Days elapsed since 14 October 1582; the LE Lilian day number. */
export function lilianDay(date: CalendarDate): number {
  const utc = Date.UTC(date.year, date.month - 1, date.day);
  // Date.UTC maps years 0-99 into the 20th century; undo that shift.
  const adjusted = new Date(utc);
  if (date.year >= 0 && date.year <= 99) {
    adjusted.setUTCFullYear(date.year);
  }
  return Math.round((adjusted.getTime() - EPOCH_UTC) / MS_PER_DAY);
}

export function isWithinLilianRange(date: CalendarDate): boolean {
  const day = lilianDay(date);
  return day >= FIRST_LILIAN_DAY && day <= LAST_LILIAN_DAY;
}
