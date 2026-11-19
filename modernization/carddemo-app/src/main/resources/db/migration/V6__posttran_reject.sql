-- POSTTRAN reject staging (BND-003, D-012): the 430-byte DALYREJS image as raw bytes in the input encoding
-- (never text, so CP037 LOW-VALUES survive), inserted in the record's transaction so a restart keeps them.
CREATE TABLE batch.posttran_reject (
    job_instance_id  BIGINT NOT NULL,
    input_seq        BIGINT NOT NULL CHECK (input_seq > 0),
    record           BYTEA  NOT NULL CHECK (octet_length(record) = 430),
    PRIMARY KEY (job_instance_id, input_seq)
);
