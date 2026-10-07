-- UNT51-21: transaction report requests submitted online (CORPT00C replacement, ADR-0021). One row per confirmed
-- request; the tranrept job stream it launches asynchronously writes its job and step rows to batch_run and the
-- report to batch_output_file. The row links the three: report_request_id is the execution id the API returns.

CREATE TABLE report_request (
    report_request_id        BIGINT GENERATED ALWAYS AS IDENTITY,
    job_stream               VARCHAR(100)  NOT NULL,
    report_name              VARCHAR(10)   NOT NULL,
    start_date               DATE          NOT NULL,
    end_date                 DATE          NOT NULL,
    run_date                 DATE          NOT NULL,
    encoding                 VARCHAR(10)   NOT NULL,
    requested_by             VARCHAR(8)    NOT NULL,
    status                   VARCHAR(10)   NOT NULL,
    return_code              SMALLINT,
    job_execution_ids        VARCHAR(500),
    report_job_execution_id  BIGINT,
    output_file_id           BIGINT,
    message                  VARCHAR(2500),
    submitted_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    started_at               TIMESTAMP WITH TIME ZONE,
    ended_at                 TIMESTAMP WITH TIME ZONE,
    CONSTRAINT pk_report_request PRIMARY KEY (report_request_id),
    CONSTRAINT ck_report_request_status CHECK (status IN ('QUEUED', 'RUNNING', 'COMPLETED', 'FAILED')),
    CONSTRAINT ck_report_request_name CHECK (report_name IN ('Monthly', 'Yearly', 'Custom')),
    CONSTRAINT ck_report_request_window CHECK (start_date <= end_date)
);

CREATE INDEX ix_report_request_requested_by ON report_request (requested_by, report_request_id);
