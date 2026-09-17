/**
 * `CORPT00C` — request a transaction report (map `CORPT0A` of `CORPT00`).
 *
 * The screen picks a monthly, yearly or custom date range and confirms it.
 * Where the COBOL then writes the `TRANREPT` JCL to the transient data queue
 * `JOBS` (the CICS internal reader), this port hands the same date range to a
 * {@link ReportRequestQueue}; see `report-queue.ts` for the substitution.
 *
 * `SEND-TRNRPT-SCREEN` ends with `GO TO RETURN-TO-CICS`, so the first failing
 * edit ends the task and the handler returns on the first message too.
 */

import type { CardDemoCommarea } from "./commarea.js";
import { emptyCommarea, isReenter } from "./commarea.js";
import { isBlank, isNumericField, isValidDate, numvalC } from "./cobol.js";
import type { OnlineContext } from "./context.js";
import {
  INVALID_KEY,
  confirmReport,
  invalidConfirmValue,
  reportMessages,
  reportSubmitted,
} from "./messages.js";
import type { ReportType } from "./report-queue.js";
import type { MessageColor, OnlineRequest, OnlineResponse, ScreenHeader } from "./screen.js";
import { AidKey, headerFor } from "./screen.js";
import { MENU_PROGRAM, SIGNON_PROGRAM } from "./transaction-list.js";

export const REPORT_PROGRAM = "CORPT00C";
export const REPORT_TRANID = "CR00";

/** The job `SUBMIT-JOB-TO-INTRDR` would have started. */
export const REPORT_JOB_NAME = "TRANREPT";

export interface ReportRequestScreen {
  /** `MONTHLY` PIC X(01). */
  readonly monthly: string;
  /** `YEARLY` PIC X(01). */
  readonly yearly: string;
  /** `CUSTOM` PIC X(01). */
  readonly custom: string;
  /** `SDTMM` PIC X(02). */
  readonly startMonth: string;
  /** `SDTDD` PIC X(02). */
  readonly startDay: string;
  /** `SDTYYYY` PIC X(04). */
  readonly startYear: string;
  /** `EDTMM` PIC X(02). */
  readonly endMonth: string;
  /** `EDTDD` PIC X(02). */
  readonly endDay: string;
  /** `EDTYYYY` PIC X(04). */
  readonly endYear: string;
  /** `CONFIRM` PIC X(01). */
  readonly confirm: string;
  readonly errMsg: string;
}

export const emptyReportRequestScreen = (): ReportRequestScreen => ({
  monthly: "",
  yearly: "",
  custom: "",
  startMonth: "",
  startDay: "",
  startYear: "",
  endMonth: "",
  endDay: "",
  endYear: "",
  confirm: "",
  errMsg: "",
});

export function handleReportRequest(
  request: OnlineRequest<ReportRequestScreen>,
  context: OnlineContext,
): OnlineResponse<ReportRequestScreen> {
  const header = headerFor(REPORT_TRANID, REPORT_PROGRAM, context.now());

  if (request.commarea === undefined) {
    return transfer(emptyReportRequestScreen(), emptyCommarea(), SIGNON_PROGRAM, header);
  }

  const commarea = request.commarea;

  if (!isReenter(commarea)) {
    return send(emptyReportRequestScreen(), commarea, header, "MONTHLY");
  }

  switch (request.aid) {
    case AidKey.enter:
      return processEnterKey(request.screen, commarea, context, header);
    case AidKey.pf3:
      return transfer(request.screen, commarea, MENU_PROGRAM, header);
    default:
      return send({ ...request.screen, errMsg: INVALID_KEY }, commarea, header, "MONTHLY");
  }
}

export interface DateRange {
  readonly reportName: string;
  readonly reportType: ReportType;
  readonly startDate: string;
  readonly endDate: string;
}

/** `PROCESS-ENTER-KEY`. */
function processEnterKey(
  screen: ReportRequestScreen,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<ReportRequestScreen> {
  if (!isBlank(screen.monthly)) {
    return submit(screen, monthlyRange(context.now()), commarea, context, header);
  }
  if (!isBlank(screen.yearly)) {
    return submit(screen, yearlyRange(context.now()), commarea, context, header);
  }
  if (!isBlank(screen.custom)) {
    const custom = customRange(screen);
    if ("message" in custom) {
      return send({ ...custom.screen, errMsg: custom.message }, commarea, header, custom.cursor);
    }
    return submit(custom.screen, custom.range, commarea, context, header);
  }
  return send(
    { ...screen, errMsg: reportMessages.selectReportType },
    commarea,
    header,
    "MONTHLY",
  );
}

/** First of the current month to the last day of the current month. */
export function monthlyRange(now: Date): DateRange {
  const year = now.getFullYear();
  const month = now.getMonth();
  const end = new Date(year, month + 1, 0);
  return {
    reportName: "Monthly",
    reportType: "monthly",
    startDate: isoDate(year, month + 1, 1),
    endDate: isoDate(end.getFullYear(), end.getMonth() + 1, end.getDate()),
  };
}

/** January 1st to December 31st of the current year. */
export function yearlyRange(now: Date): DateRange {
  const year = now.getFullYear();
  return {
    reportName: "Yearly",
    reportType: "yearly",
    startDate: isoDate(year, 1, 1),
    endDate: isoDate(year, 12, 31),
  };
}

interface CustomFailure {
  readonly message: string;
  readonly cursor: string;
  readonly screen: ReportRequestScreen;
}

interface CustomRange {
  readonly range: DateRange;
  readonly screen: ReportRequestScreen;
}

/** The `WHEN CUSTOMI` branch: required fields, ranges and `CSUTLDTC`. */
function customRange(screen: ReportRequestScreen): CustomRange | CustomFailure {
  const required: ReadonlyArray<readonly [string, string, string]> = [
    [screen.startMonth, reportMessages.startMonthEmpty, "SDTMM"],
    [screen.startDay, reportMessages.startDayEmpty, "SDTDD"],
    [screen.startYear, reportMessages.startYearEmpty, "SDTYYYY"],
    [screen.endMonth, reportMessages.endMonthEmpty, "EDTMM"],
    [screen.endDay, reportMessages.endDayEmpty, "EDTDD"],
    [screen.endYear, reportMessages.endYearEmpty, "EDTYYYY"],
  ];
  for (const [value, message, cursor] of required) {
    if (isBlank(value)) {
      return { message, cursor, screen };
    }
  }

  // `COMPUTE WS-NUM-99 = FUNCTION NUMVAL-C(...)` then the move back, which
  // zero fills the field: `7` becomes `07`.
  const normalized: ReportRequestScreen = {
    ...screen,
    startMonth: editNumeric(screen.startMonth, 2),
    startDay: editNumeric(screen.startDay, 2),
    startYear: editNumeric(screen.startYear, 4),
    endMonth: editNumeric(screen.endMonth, 2),
    endDay: editNumeric(screen.endDay, 2),
    endYear: editNumeric(screen.endYear, 4),
  };

  const ranges: ReadonlyArray<readonly [string, number, string, string]> = [
    [normalized.startMonth, 12, reportMessages.startMonthInvalid, "SDTMM"],
    [normalized.startDay, 31, reportMessages.startDayInvalid, "SDTDD"],
    [normalized.startYear, Number.POSITIVE_INFINITY, reportMessages.startYearInvalid, "SDTYYYY"],
    [normalized.endMonth, 12, reportMessages.endMonthInvalid, "EDTMM"],
    [normalized.endDay, 31, reportMessages.endDayInvalid, "EDTDD"],
    [normalized.endYear, Number.POSITIVE_INFINITY, reportMessages.endYearInvalid, "EDTYYYY"],
  ];
  for (const [value, max, message, cursor] of ranges) {
    if (!isNumericField(value) || Number(value) > max) {
      return { message, cursor, screen: normalized };
    }
  }

  const startDate = `${normalized.startYear}-${normalized.startMonth}-${normalized.startDay}`;
  const endDate = `${normalized.endYear}-${normalized.endMonth}-${normalized.endDay}`;

  if (!isValidDate(startDate)) {
    return { message: reportMessages.startDateInvalid, cursor: "SDTMM", screen: normalized };
  }
  if (!isValidDate(endDate)) {
    return { message: reportMessages.endDateInvalid, cursor: "EDTMM", screen: normalized };
  }

  return {
    range: { reportName: "Custom", reportType: "custom", startDate, endDate },
    screen: normalized,
  };
}

/** `SUBMIT-JOB-TO-INTRDR`, with the report request in place of the JCL. */
function submit(
  screen: ReportRequestScreen,
  range: DateRange,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<ReportRequestScreen> {
  const confirm = screen.confirm.trim();
  if (confirm === "") {
    return send(
      { ...screen, errMsg: confirmReport(range.reportName) },
      commarea,
      header,
      "CONFIRM",
    );
  }
  if (confirm === "N" || confirm === "n") {
    return send(emptyReportRequestScreen(), commarea, header, "MONTHLY");
  }
  if (confirm !== "Y" && confirm !== "y") {
    return send(
      { ...screen, errMsg: invalidConfirmValue(confirm) },
      commarea,
      header,
      "CONFIRM",
    );
  }

  const submission = context.reports.submit({
    reportName: range.reportName,
    reportType: range.reportType,
    startDate: range.startDate,
    endDate: range.endDate,
    userId: commarea.userId,
    submittedAt: context.now(),
    jobName: REPORT_JOB_NAME,
  });

  if (!submission.accepted) {
    return send(
      { ...screen, errMsg: reportMessages.writeQueueFailed },
      commarea,
      header,
      "MONTHLY",
    );
  }

  return send(
    { ...emptyReportRequestScreen(), errMsg: reportSubmitted(range.reportName) },
    commarea,
    header,
    "MONTHLY",
    "green",
  );
}

const isoDate = (year: number, month: number, day: number): string =>
  `${String(year).padStart(4, "0")}-${String(month).padStart(2, "0")}-${String(day).padStart(2, "0")}`;

const editNumeric = (value: string, width: number): string =>
  String(Math.trunc(Math.abs(numvalC(value))) % 10 ** width).padStart(width, "0");

const send = (
  screen: ReportRequestScreen,
  commarea: CardDemoCommarea,
  header: ScreenHeader,
  cursor: string,
  messageColor: MessageColor = "red",
): OnlineResponse<ReportRequestScreen> => ({
  screen,
  header,
  commarea: { ...commarea, pgmContext: 1 },
  nextProgram: REPORT_PROGRAM,
  transfer: false,
  cursor,
  messageColor,
});

const transfer = (
  screen: ReportRequestScreen,
  commarea: CardDemoCommarea,
  toProgram: string,
  header: ScreenHeader,
): OnlineResponse<ReportRequestScreen> => ({
  screen,
  header,
  commarea: {
    ...commarea,
    fromTranId: REPORT_TRANID,
    fromProgram: REPORT_PROGRAM,
    toProgram,
    pgmContext: 0,
  },
  nextProgram: toProgram,
  transfer: true,
  messageColor: "red",
});
