/**
 * Equivalent of the Language Environment callable service CEEDAYS: converts a
 * date string described by a picture string to a Lillian day number (days
 * since 14 Oct 1582, so 15 Oct 1582 = 1) and reports errors via a condition
 * token (severity + message number, facility CEE).
 *
 * Supported picture tokens: YYYY, YY, MM, DD, DDD (day of year), MMM / Mmm /
 * MMMMMMMMMM (month names), plus any separator characters (copied literally).
 * Unsupported tokens (eras, ZD, ...) yield CEE2EE "Bad Pic String".
 */

export interface CeedaysFeedback {
  /** Condition severity (0 = success, 3 = error). */
  severity: number;
  /** LE message number (e.g. 2508 for CEE2EC). */
  msgNo: number;
  /** Symbolic feedback code, e.g. 'CEE000', 'CEE2EC'. */
  symbol: string;
}

export interface CeedaysResult {
  /** Lillian day number; 0 when the date was not valid. */
  lillian: number;
  feedback: CeedaysFeedback;
}

const fb = (msgNo: number, symbol: string, severity = 3): CeedaysFeedback => ({ severity, msgNo, symbol });

/** Feedback codes CEEDAYS can return (those tested by CSUTLDTC). */
export const CEEDAYS_FC = {
  OK: fb(0, 'CEE000', 0),
  INSUFFICIENT_DATA: fb(2507, 'CEE2EB'),
  BAD_DATE_VALUE: fb(2508, 'CEE2EC'),
  INVALID_ERA: fb(2509, 'CEE2ED'),
  UNSUPP_RANGE: fb(2513, 'CEE2EH'),
  INVALID_MONTH: fb(2517, 'CEE2EL'),
  BAD_PIC_STRING: fb(2518, 'CEE2EM'),
  NON_NUMERIC_DATA: fb(2520, 'CEE2EO'),
  YEAR_IN_ERA_ZERO: fb(2521, 'CEE2EP'),
} as const;

const MONTHS = ['JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER'];

type Token = { kind: 'YYYY' | 'YY' | 'MM' | 'DD' | 'DDD'; width: number } | { kind: 'MON'; width: number } | { kind: 'lit'; text: string };

function tokenize(pic: string): Token[] | undefined {
  const out: Token[] = [];
  let i = 0;
  while (i < pic.length) {
    const rest = pic.slice(i);
    const upper = rest.toUpperCase();
    const run = (ch: string) => {
      let n = 0;
      while (n < upper.length && upper[n] === ch) n++;
      return n;
    };
    const c = upper[0]!;
    if (c === 'Y') {
      const n = run('Y');
      if (n === 4) out.push({ kind: 'YYYY', width: 4 });
      else if (n === 2) out.push({ kind: 'YY', width: 2 });
      else return undefined;
      i += n;
    } else if (c === 'M') {
      const n = run('M');
      if (n === 2) out.push({ kind: 'MM', width: 2 });
      else if (n === 3 || n === 10) out.push({ kind: 'MON', width: n });
      else return undefined;
      i += n;
    } else if (c === 'D') {
      const n = run('D');
      if (n === 2) out.push({ kind: 'DD', width: 2 });
      else if (n === 3) out.push({ kind: 'DDD', width: 3 });
      else return undefined;
      i += n;
    } else if (/[A-Z0-9]/.test(c)) {
      // Era (<JJJJ>, GGGG), ZD, ZM, ... are not supported by this port.
      return undefined;
    } else {
      out.push({ kind: 'lit', text: rest[0]! });
      i += 1;
    }
  }
  return out;
}

export const isLeapYear = (y: number): boolean => (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0;

export function daysInMonth(year: number, month: number): number {
  return [31, isLeapYear(year) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][month - 1]!;
}

/** Lillian day number of a Gregorian date (15 Oct 1582 = 1). */
export function toLillian(year: number, month: number, day: number): number {
  // Days from civil (Howard Hinnant), epoch 1970-01-01 = 0.
  const y = month <= 2 ? year - 1 : year;
  const era = Math.floor(y / 400);
  const yoe = y - era * 400;
  const mp = (month + 9) % 12;
  const doy = Math.floor((153 * mp + 2) / 5) + day - 1;
  const doe = yoe * 365 + Math.floor(yoe / 4) - Math.floor(yoe / 100) + doy;
  const epochDays = era * 146097 + doe - 719468;
  return epochDays + 141428; // 1970-01-01 is Lillian 141428
}

/** CEEDAYS. `century` is the 100-year window start for YY (LE default: current year - 80). */
export function ceedays(date: string, picture: string, century?: number): CeedaysResult {
  const fail = (f: CeedaysFeedback): CeedaysResult => ({ lillian: 0, feedback: f });
  const pic = picture.replace(/\s+$/, '');
  const tokens = tokenize(pic);
  if (!tokens || pic.length === 0) return fail(CEEDAYS_FC.BAD_PIC_STRING);
  const hasYear = tokens.some((t) => t.kind === 'YYYY' || t.kind === 'YY');
  const hasDdd = tokens.some((t) => t.kind === 'DDD');
  const hasMonth = tokens.some((t) => t.kind === 'MM' || t.kind === 'MON');
  const hasDay = tokens.some((t) => t.kind === 'DD');
  if (!hasYear || (!hasDdd && !(hasMonth && hasDay))) return fail(CEEDAYS_FC.BAD_PIC_STRING);

  const value = date.replace(/\s+$/, '');
  if (value.length === 0) return fail(CEEDAYS_FC.INSUFFICIENT_DATA);

  let pos = 0;
  let year = 0;
  let month = 0;
  let day = 0;
  let ddd = 0;
  for (const t of tokens) {
    if (t.kind === 'lit') {
      // Separators must match the picture string (assumption; see README LIMITATIONS).
      if (pos >= value.length) return fail(CEEDAYS_FC.INSUFFICIENT_DATA);
      if (value[pos] !== t.text) return fail(CEEDAYS_FC.BAD_DATE_VALUE);
      pos += 1;
      continue;
    }
    if (t.kind === 'MON') {
      const text = value.slice(pos, pos + t.width).trim().toUpperCase();
      if (text.length < 3) return fail(CEEDAYS_FC.INSUFFICIENT_DATA);
      const m = MONTHS.findIndex((name) => name.startsWith(text) || (t.width === 3 && name.slice(0, 3) === text));
      if (m < 0) return fail(CEEDAYS_FC.INVALID_MONTH);
      month = m + 1;
      pos += t.width;
      continue;
    }
    const field = value.slice(pos, pos + t.width);
    if (field.length < t.width) return fail(CEEDAYS_FC.INSUFFICIENT_DATA);
    if (!/^\d+$/.test(field)) return fail(CEEDAYS_FC.NON_NUMERIC_DATA);
    const n = Number(field);
    switch (t.kind) {
      case 'YYYY':
        year = n;
        break;
      case 'YY': {
        const start = century ?? new Date().getFullYear() - 80;
        year = Math.floor(start / 100) * 100 + n;
        if (year < start) year += 100;
        break;
      }
      case 'MM':
        month = n;
        break;
      case 'DD':
        day = n;
        break;
      case 'DDD':
        ddd = n;
        break;
    }
    pos += t.width;
  }

  if (year < 1582 || year > 9999) return fail(CEEDAYS_FC.UNSUPP_RANGE);
  if (hasDdd) {
    const max = isLeapYear(year) ? 366 : 365;
    if (ddd < 1 || ddd > max) return fail(CEEDAYS_FC.BAD_DATE_VALUE);
    const lillian = toLillian(year, 1, 1) + ddd - 1;
    if (lillian < 1) return fail(CEEDAYS_FC.UNSUPP_RANGE);
    return { lillian, feedback: CEEDAYS_FC.OK };
  }
  if (month < 1 || month > 12) return fail(CEEDAYS_FC.INVALID_MONTH);
  if (day < 1 || day > daysInMonth(year, month)) return fail(CEEDAYS_FC.BAD_DATE_VALUE);
  const lillian = toLillian(year, month, day);
  if (lillian < 1) return fail(CEEDAYS_FC.UNSUPP_RANGE);
  return { lillian, feedback: CEEDAYS_FC.OK };
}
