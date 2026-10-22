/**
 * Port of app/cbl/CSUTLDTC.cbl: validates a date against a format mask via
 * CEEDAYS and returns the 80-byte WS-MESSAGE plus RETURN-CODE (= severity).
 *
 * WS-MESSAGE layout (80 bytes):
 *   sev(4) 'Mesg Code:'(11) msgno(4) ' ' result(15) ' ' 'TstDate:'(9)
 *   date(10) ' ' 'Mask used:'(10) mask(10) ' ' '   '
 */
import { ceedays, type CeedaysFeedback } from './ceedays.js';

export interface CsutldtcResult {
  /** LS-RESULT, exactly 80 characters. */
  result: string;
  /** RETURN-CODE (CEEDAYS severity). */
  returnCode: number;
  /** WS-SEVERITY, 4 digits ('0000', '0003'). */
  severity: string;
  /** WS-MSG-NO, 4 digits ('0000', '2513', ...). */
  msgNo: string;
  /** WS-RESULT, 15 characters ('Date is valid  ', 'Unsupp. Range  ', ...). */
  message: string;
  /** Lillian day number from CEEDAYS (0 if invalid). */
  lillian: number;
}

const RESULT_TEXT: Record<number, string> = {
  0: 'Date is valid',
  2507: 'Insufficient',
  2508: 'Datevalue error',
  2509: 'Invalid Era',
  2513: 'Unsupp. Range',
  2517: 'Invalid month',
  2518: 'Bad Pic String',
  2520: 'Nonnumeric data',
  2521: 'YearInEra is 0',
};

const fit = (s: string, n: number): string => s.padEnd(n, ' ').slice(0, n);
const digits4 = (n: number): string => String(Math.abs(n) % 10000).padStart(4, '0');

/**
 * CALL 'CSUTLDTC' USING LS-DATE (X(10)), LS-DATE-FORMAT (X(10)), LS-RESULT (X(80)).
 * `ceedaysImpl` can be injected to reproduce specific LE feedback codes in tests.
 */
export function csutldtc(
  lsDate: string,
  lsDateFormat: string,
  ceedaysImpl: (date: string, fmt: string) => { lillian: number; feedback: CeedaysFeedback } = ceedays,
): CsutldtcResult {
  const date = fit(lsDate, 10);
  const fmt = fit(lsDateFormat, 10);
  const { lillian, feedback } = ceedaysImpl(date, fmt);

  // MOVE WS-DATE-TO-TEST TO WS-DATE moves the whole variable-length group,
  // i.e. the S9(4) BINARY length prefix (X'000A') followed by the date text,
  // truncated to X(10). This is what the original program displays.
  const wsDate = fit(String.fromCharCode(0x00, date.length) + date, 10);
  const severity = digits4(feedback.severity);
  const msgNo = digits4(feedback.msgNo);
  // WHEN OTHER -> 'Date is invalid'. Note FC-INVALID-DATE is the all-zero token (success).
  const known = feedback.severity === 0 && feedback.msgNo === 0 ? RESULT_TEXT[0] : feedback.severity === 3 ? RESULT_TEXT[feedback.msgNo] : undefined;
  const message = fit(known ?? 'Date is invalid', 15);

  const result = severity + fit('Mesg Code:', 11) + msgNo + ' ' + message + ' ' + fit('TstDate:', 9) + wsDate + ' ' + fit('Mask used:', 10) + fmt + ' ' + '   ';
  return { result, returnCode: feedback.severity, severity, msgNo, message, lillian };
}
