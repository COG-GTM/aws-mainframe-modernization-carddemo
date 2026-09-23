/**
 * `Z-GET-DB2-FORMAT-TIMESTAMP` (CBTRN02C l.692-705): `FUNCTION CURRENT-DATE`
 * rendered as the 26-character DB2 format `YYYY-MM-DD-HH.MM.SS.hh0000` —
 * hundredths of a second followed by a literal `0000`.
 */
export type Clock = () => Date;

export const systemClock: Clock = () => new Date();

function pad(value: number, length: number): string {
  return value.toString().padStart(length, '0');
}

/**
 * Formats local server time, matching `FUNCTION CURRENT-DATE`. The time zone
 * is therefore whatever the host is set to (spec §8.14).
 */
export function db2FormatTimestamp(now: Date): string {
  const hundredths = Math.floor(now.getMilliseconds() / 10);
  return [
    pad(now.getFullYear(), 4),
    '-',
    pad(now.getMonth() + 1, 2),
    '-',
    pad(now.getDate(), 2),
    '-',
    pad(now.getHours(), 2),
    '.',
    pad(now.getMinutes(), 2),
    '.',
    pad(now.getSeconds(), 2),
    '.',
    pad(hundredths, 2),
    '0000',
  ].join('');
}
