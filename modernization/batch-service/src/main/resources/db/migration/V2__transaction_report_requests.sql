-- Created in the Flyway default schema (batch). CORPT00C report submissions: one row per POST /api/v1/reports/transactions, polled by report_id.
-- Replaces the job the screen wrote to the CICS internal reader (TDQ JOBS) and JES's job status.
CREATE TABLE transaction_report_request (
    report_id        VARCHAR(36)  PRIMARY KEY,
    report_type      VARCHAR(10)  NOT NULL,
    start_date       DATE         NOT NULL,
    end_date         DATE         NOT NULL,
    status           VARCHAR(10)  NOT NULL,
    submitted_by     VARCHAR(8)   NOT NULL,
    submitted_at     TIMESTAMP WITH TIME ZONE NOT NULL,
    started_at       TIMESTAMP WITH TIME ZONE,
    completed_at     TIMESTAMP WITH TIME ZONE,
    job_execution_id BIGINT,
    return_code      INTEGER,
    output_dir       VARCHAR(512),
    message          VARCHAR(200)
);
