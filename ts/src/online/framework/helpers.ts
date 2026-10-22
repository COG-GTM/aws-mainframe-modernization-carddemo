/**
 * Shared online helpers (CSDAT01Y dates, COTTL01Y titles, CSMSG01Y/CSMSG02Y
 * messages, NUMVAL family, numeric-edited formatting, CSUTLDTC and CSLKPCDY
 * re-exports, COBOL string handling) for every online program port.
 */
import { CcdaScreenTitleLayout } from '../../common/copybooks/index.js';
import { Decimal } from '../../common/codecs/decimal.js';
import type { SymbolicMap } from './bms/map.js';

export { CCDA_MSG_INVALID_KEY, CCDA_MSG_THANK_YOU, AbendDataLayout, type AbendData } from '../../common/copybooks/index.js';
export { ADMIN_MENU_OPTIONS, ADMIN_MENU_OPT_COUNT, MAIN_MENU_OPTIONS, MAIN_MENU_OPT_COUNT, type MenuOption } from '../../common/copybooks/index.js';
export * from '../../common/copybooks/lookup-codes.js';
export { csutldtc, type CsutldtcResult } from '../../common/utils/csutldtc.js';
export { formatEdited, deEdit } from '../../common/codecs/edited.js';

const titles = CcdaScreenTitleLayout.initial();
/** COTTL01Y CCDA-TITLE01 / CCDA-TITLE02 / CCDA-THANK-YOU. */
export const CCDA_TITLE01: string = titles.ccdaTitle01;
export const CCDA_TITLE02: string = titles.ccdaTitle02;
export const CCDA_THANK_YOU: string = titles.ccdaThankYou;

// ---------------------------------------------------------------- dates (CSDAT01Y)
const p2 = (n: number): string => String(n).padStart(2, '0');

/** FUNCTION CURRENT-DATE: YYYYMMDDHHMMSSss+hhmm (local time). */
export function functionCurrentDate(now: Date): string {
  const off = -now.getTimezoneOffset();
  const sign = off >= 0 ? '+' : '-';
  return (
    String(now.getFullYear()).padStart(4, '0') + p2(now.getMonth() + 1) + p2(now.getDate()) +
    p2(now.getHours()) + p2(now.getMinutes()) + p2(now.getSeconds()) + p2(Math.floor(now.getMilliseconds() / 10)) +
    sign + p2(Math.floor(Math.abs(off) / 60)) + p2(Math.abs(off) % 60)
  );
}

/** WS-CURDATE-MM-DD-YY ("mm/dd/yy"). */
export const curdateMmDdYy = (now: Date): string => `${p2(now.getMonth() + 1)}/${p2(now.getDate())}/${p2(now.getFullYear() % 100)}`;
/** WS-CURTIME-HH-MM-SS ("hh:mm:ss"). */
export const curtimeHhMmSs = (now: Date): string => `${p2(now.getHours())}:${p2(now.getMinutes())}:${p2(now.getSeconds())}`;
/** WS-TIMESTAMP ("YYYY-MM-DD HH:MM:SS.ffffff"). */
export const wsTimestamp = (now: Date): string =>
  `${now.getFullYear()}-${p2(now.getMonth() + 1)}-${p2(now.getDate())} ${curtimeHhMmSs(now)}.${String(now.getMilliseconds() * 1000).padStart(6, '0')}`;

/** EXEC CICS FORMATTIME ABSTIME(...) with the options CardDemo uses. */
export function formatTime(abstime: Date, opts: { datesep?: string; timesep?: string } = {}): {
  yyyymmdd: string; mmddyy: string; yymmdd: string; ddmmyy: string; time: string;
} {
  const ds = opts.datesep ?? '';
  const ts = opts.timesep ?? '';
  const y4 = String(abstime.getFullYear()).padStart(4, '0');
  const y2 = p2(abstime.getFullYear() % 100);
  const m = p2(abstime.getMonth() + 1);
  const d = p2(abstime.getDate());
  return {
    yyyymmdd: [y4, m, d].join(ds),
    mmddyy: [m, d, y2].join(ds),
    yymmdd: [y2, m, d].join(ds),
    ddmmyy: [d, m, y2].join(ds),
    time: [p2(abstime.getHours()), p2(abstime.getMinutes()), p2(abstime.getSeconds())].join(ts),
  };
}

/** POPULATE-HEADER-INFO common to every CardDemo screen (TITLE01/02, TRNNAME, PGMNAME, CURDATE, CURTIME). */
export function populateHeaderInfo(map: SymbolicMap, info: { tranid: string; pgmname: string; now: Date }): void {
  const names = new Set(map.fieldNames());
  const put = (n: string, v: string): void => {
    if (names.has(n)) map.set(n, v);
  };
  put('TITLE01', CCDA_TITLE01);
  put('TITLE02', CCDA_TITLE02);
  put('TRNNAME', info.tranid);
  put('PGMNAME', info.pgmname);
  put('CURDATE', curdateMmDdYy(info.now));
  put('CURTIME', curtimeHhMmSs(info.now));
}

// ---------------------------------------------------------------- COBOL strings
/** `x = SPACES` */
export const isSpaces = (s: string): boolean => /^ *$/.test(s);
/** `x = LOW-VALUES` */
export const isLowValues = (s: string): boolean => /^\0*$/.test(s);
/** `x = SPACES OR LOW-VALUES` (all spaces, or all low-values). */
export const isSpacesOrLowValues = (s: string): boolean => isSpaces(s) || isLowValues(s);
/** Alphanumeric MOVE into a PIC X(n) item. */
export const moveAlnum = (value: string, length: number): string => (value.length >= length ? value.slice(0, length) : value.padEnd(length, ' '));
/** FUNCTION UPPER-CASE / LOWER-CASE (ASCII letters only, like EBCDIC CP037 a-z). */
export const upperCase = (s: string): string => s.replace(/[a-z]/g, (c) => c.toUpperCase());
export const lowerCase = (s: string): string => s.replace(/[A-Z]/g, (c) => c.toLowerCase());
/** FUNCTION TRIM (leading and trailing spaces). */
export const trimCobol = (s: string): string => s.replace(/^ +| +$/g, '');
/** `x IS NUMERIC` for an unsigned DISPLAY / alphanumeric item. */
export const isNumericText = (s: string): boolean => s.length > 0 && /^[0-9]+$/.test(s);

/** One STRING source: value with `DELIMITED BY SIZE` (default) or by a delimiter literal. */
export type StringPart = string | { value: string; delimitedBy: string };

/** STRING ... DELIMITED BY ... INTO target (target pre-filled; stops at its length). */
export function cobolString(parts: readonly StringPart[], into: string): string {
  let out = '';
  for (const p of parts) {
    if (typeof p === 'string') out += p;
    else {
      const i = p.value.indexOf(p.delimitedBy);
      out += i < 0 ? p.value : p.value.slice(0, i);
    }
  }
  return out.length >= into.length ? out.slice(0, into.length) : out + into.slice(out.length);
}

// ---------------------------------------------------------------- NUMVAL family
interface NumParse { ok: boolean; badPos: number; value: Decimal }

function parseNumval(src: string, currency: string | undefined): NumParse {
  const s = src;
  let i = 0;
  let neg = false;
  let sign = false;
  const bad = (pos: number): NumParse => ({ ok: false, badPos: pos + 1, value: new Decimal(0) });
  const skip = (): void => { while (s[i] === ' ') i++; };
  skip();
  const takeLeading = (): void => {
    if (!sign && (s[i] === '+' || s[i] === '-')) { sign = true; neg = s[i] === '-'; i++; skip(); }
  };
  takeLeading();
  if (currency && s.startsWith(currency, i)) { i += currency.length; skip(); takeLeading(); }
  let digits = '';
  let frac = '';
  let seenDot = false;
  let any = false;
  for (; i < s.length; i++) {
    const c = s[i]!;
    if (c >= '0' && c <= '9') { if (seenDot) frac += c; else digits += c; any = true; }
    else if (c === '.' && !seenDot) seenDot = true;
    else if (c === ',' && currency !== undefined && !seenDot) continue;
    else break;
  }
  if (!any) return bad(i >= s.length ? s.length : i);
  skip();
  if (i < s.length) {
    const rest = s.slice(i).toUpperCase();
    if (!sign && (s[i] === '+' || s[i] === '-')) { sign = true; neg = s[i] === '-'; i++; }
    else if (!sign && (rest.startsWith('CR') || rest.startsWith('DB'))) { sign = true; neg = true; i += 2; }
    else return bad(i);
    skip();
    if (i < s.length) return bad(i);
  }
  const v = new Decimal(`${digits || '0'}.${frac || '0'}`);
  return { ok: true, badPos: 0, value: neg ? v.neg() : v };
}

/** FUNCTION NUMVAL(s): invalid input yields 0 (GnuCOBOL / LE behaviour for bad data). */
export const numval = (s: string): Decimal => parseNumval(s, undefined).value;
/** FUNCTION NUMVAL-C(s [, currency]) (default currency sign '$', commas allowed). */
export const numvalC = (s: string, currency = '$'): Decimal => parseNumval(s, currency).value;
/** FUNCTION TEST-NUMVAL(s): 0 if valid, else 1-based position of the first bad character. */
export const testNumval = (s: string): number => parseNumval(s, undefined).badPos;
/** FUNCTION TEST-NUMVAL-C(s [, currency]). */
export const testNumvalC = (s: string, currency = '$'): number => parseNumval(s, currency).badPos;
