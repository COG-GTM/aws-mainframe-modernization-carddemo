/**
 * `Z-GET-DB2-FORMAT-TIMESTAMP`: `FUNCTION CURRENT-DATE` reshaped into the
 * 26 character DB2 timestamp `YYYY-MM-DD-HH.MM.SS.hh0000`, where `hh` is the
 * hundredths of a second COBOL reports and the trailing `0000` is literal.
 */

function pad(value: number, width = 2): string {
  return String(value).padStart(width, "0");
}

export function db2FormatTimestamp(date: Date): string {
  const hundredths = Math.floor(date.getMilliseconds() / 10);
  return [
    `${pad(date.getFullYear(), 4)}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}`,
    `${pad(date.getHours())}.${pad(date.getMinutes())}.${pad(date.getSeconds())}.${pad(hundredths)}0000`,
  ].join("-");
}
