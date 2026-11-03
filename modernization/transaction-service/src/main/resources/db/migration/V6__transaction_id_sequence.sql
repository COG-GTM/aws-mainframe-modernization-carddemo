-- TRAN-ID allocation for online adds (COTRN02C ADD-TRANSACTION) and bill payments (COBIL00C).
-- The COBOL reads the highest TRAN-ID (STARTBR HIGH-VALUES + READPREV) and adds 1 into a PIC 9(16)
-- field. The sequence keeps the ids numeric and in key order while working with several service
-- replicas; TransactionIdGenerator moves it past the highest numeric id already stored (for example
-- DALYTRAN ids written by POSTTRAN), so an allocated id stays "last id + 1" or higher.
CREATE SEQUENCE tran_id_seq AS BIGINT MINVALUE 1 MAXVALUE 9999999999999999 NO CYCLE;
