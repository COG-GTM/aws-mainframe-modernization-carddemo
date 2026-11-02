-- Optimistic locking column; replaces the CICS record lock of READ ... UPDATE (COUSR02C, COUSR03C).
ALTER TABLE users ADD COLUMN version BIGINT NOT NULL DEFAULT 0;
