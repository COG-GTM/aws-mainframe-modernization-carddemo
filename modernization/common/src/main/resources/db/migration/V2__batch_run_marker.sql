-- One row per completed date-keyed batch run (e.g. INTCALC PARM-DATE), written in the job's own transaction.
-- The primary key also serialises concurrent runs for the same date.
CREATE TABLE batch_run_marker (
    job_name     VARCHAR(20) NOT NULL,
    run_date     DATE        NOT NULL,
    completed_at TIMESTAMP   NOT NULL,
    PRIMARY KEY (job_name, run_date)
);
