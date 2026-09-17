/**
 * `Z-GET-DB2-FORMAT-TIMESTAMP`: `FUNCTION CURRENT-DATE` reformatted as the DB2
 * timestamp `YYYY-MM-DD-HH.MM.SS.hh0000` stored in `TRAN-PROC-TS`.
 */

const pad = (value: number, width: number): string =>
  String(value).padStart(width, "0");

export function db2FormatTimestamp(now: Date): string {
  const hundredths = Math.trunc(now.getMilliseconds() / 10);
  return [
    pad(now.getFullYear(), 4),
    "-",
    pad(now.getMonth() + 1, 2),
    "-",
    pad(now.getDate(), 2),
    "-",
    pad(now.getHours(), 2),
    ".",
    pad(now.getMinutes(), 2),
    ".",
    pad(now.getSeconds(), 2),
    ".",
    pad(hundredths, 2),
    "0000",
  ].join("");
}
