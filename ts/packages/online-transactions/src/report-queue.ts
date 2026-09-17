/**
 * Substitution for the CICS internal reader used by `CORPT00C`.
 *
 * On the mainframe the program builds the `TRANREPT` JCL in `WS-JOB-LINES`
 * and writes it line by line to the transient data queue `JOBS`, which is
 * defined against the internal reader, so JES schedules the batch report job.
 * There is no internal reader off-platform, so the port keeps the same
 * decision points — report name, start date, end date, confirmation — and
 * records them as a report request instead of emitting JCL. Whatever backs
 * {@link ReportRequestQueue} (a table, a queue, a batch scheduler API) is
 * responsible for running the report; the screen behaviour, including the
 * `Unable to Write TDQ (JOBS)...` path, is unchanged.
 */

export type ReportType = "monthly" | "yearly" | "custom";

export interface ReportRequest {
  /** `WS-REPORT-NAME`: `Monthly`, `Yearly` or `Custom`. */
  readonly reportName: string;
  readonly reportType: ReportType;
  /** `PARM-START-DATE`, `YYYY-MM-DD`. */
  readonly startDate: string;
  /** `PARM-END-DATE`, `YYYY-MM-DD`. */
  readonly endDate: string;
  /** `CDEMO-USER-ID` of the operator who submitted it. */
  readonly userId: string;
  readonly submittedAt: Date;
  /** The job the internal reader would have started. */
  readonly jobName: string;
}

export interface ReportSubmission {
  /** `false` drives the `Unable to Write TDQ (JOBS)...` path. */
  readonly accepted: boolean;
  /** Identifier of the recorded request, in place of a JES job id. */
  readonly requestId?: string;
}

export interface ReportRequestQueue {
  submit(request: ReportRequest): ReportSubmission;
}

/** Records the requests in memory, the default for tests and the API. */
export class InMemoryReportRequestQueue implements ReportRequestQueue {
  private readonly requests: ReportRequest[] = [];

  submit(request: ReportRequest): ReportSubmission {
    this.requests.push(request);
    return { accepted: true, requestId: `REQ${String(this.requests.length).padStart(6, "0")}` };
  }

  /** The requests recorded so far, oldest first. */
  toArray(): readonly ReportRequest[] {
    return [...this.requests];
  }
}
