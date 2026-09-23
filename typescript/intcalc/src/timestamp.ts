/**
 * `Z-GET-DB2-FORMAT-TIMESTAMP` (`CBACT04C.cbl:613-626`).
 *
 * Builds the 26-character DB2 timestamp `YYYY-MM-DD-HH.MM.SS.mm0000` from the
 * wall clock — hundredths of a second only, with the literal trailing `0000`.
 * The PARM run-date is deliberately not used here, exactly as in the COBOL.
 */
export type Clock = () => Date;

export const systemClock: Clock = () => new Date();

function pad(value: number, width: number): string {
  return String(value).padStart(width, '0');
}

export function db2FormatTimestamp(now: Date): string {
  const yyyy = pad(now.getFullYear(), 4);
  const mm = pad(now.getMonth() + 1, 2);
  const dd = pad(now.getDate(), 2);
  const hh = pad(now.getHours(), 2);
  const min = pad(now.getMinutes(), 2);
  const ss = pad(now.getSeconds(), 2);
  const hundredths = pad(Math.floor(now.getMilliseconds() / 10), 2);
  return `${yyyy}-${mm}-${dd}-${hh}.${min}.${ss}.${hundredths}0000`;
}
