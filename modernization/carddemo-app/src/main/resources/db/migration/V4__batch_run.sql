-- UNT51-11: one row per batch job run and one per step run, with the JCL-style condition code (ADR-0015).
-- Written by com.carddemo.batch.harness.BatchRunLog after every job, whoever launched it (CLI, scheduler, tests);
-- a launch that never started (unknown job, invalid parameters) is a job row without job_execution_id.

CREATE TABLE batch_run (
    batch_run_id       BIGINT GENERATED ALWAYS AS IDENTITY,
    job_execution_id   BIGINT,
    step_execution_id  BIGINT,
    job_name           VARCHAR(100)  NOT NULL,
    step_name          VARCHAR(100),
    run_date           DATE,
    status             VARCHAR(10)   NOT NULL,
    exit_code          VARCHAR(2500) NOT NULL,
    return_code        SMALLINT      NOT NULL,
    read_count         BIGINT        NOT NULL DEFAULT 0,
    write_count        BIGINT        NOT NULL DEFAULT 0,
    skip_count         BIGINT        NOT NULL DEFAULT 0,
    filter_count       BIGINT        NOT NULL DEFAULT 0,
    start_time         TIMESTAMP,
    end_time           TIMESTAMP,
    parameters         VARCHAR(2500),
    message            VARCHAR(2500),
    created_at         TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT batch_run_pk PRIMARY KEY (batch_run_id),
    CONSTRAINT batch_run_return_code_ck CHECK (return_code IN (0, 4, 8, 12, 16)),
    CONSTRAINT batch_run_counts_ck CHECK (read_count >= 0 AND write_count >= 0 AND skip_count >= 0
                                          AND filter_count >= 0),
    CONSTRAINT batch_run_step_needs_execution_ck CHECK (step_name IS NULL OR step_execution_id IS NOT NULL),
    CONSTRAINT batch_run_job_execution_fk FOREIGN KEY (job_execution_id)
        REFERENCES batch_job_execution (job_execution_id),
    CONSTRAINT batch_run_step_execution_fk FOREIGN KEY (step_execution_id)
        REFERENCES batch_step_execution (step_execution_id)
);

CREATE UNIQUE INDEX batch_run_job_uk ON batch_run (job_execution_id) WHERE step_name IS NULL;
CREATE UNIQUE INDEX batch_run_step_uk ON batch_run (step_execution_id) WHERE step_name IS NOT NULL;
CREATE INDEX batch_run_job_name_ix ON batch_run (job_name, run_date);

COMMENT ON TABLE batch_run IS 'Job and step runs with JCL condition codes (ADR-0015); step_name NULL = job row';
COMMENT ON COLUMN batch_run.return_code IS 'JCL condition code: 0 ok, 4 warning, 8/12/16 failure (16 = abend)';
