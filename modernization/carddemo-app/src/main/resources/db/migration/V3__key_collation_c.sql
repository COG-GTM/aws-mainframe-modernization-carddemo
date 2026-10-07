-- s3.2: VSAM keys collate by byte value. Keyset browses (ADR-0011) order on these columns, so they must not depend on
-- the database's locale (en_US.UTF-8 under glibc sorts 'a b' after 'ab' and ignores case on the first pass).
-- COLLATE "C" orders by the bytes of the ASCII key: the order of the GnuCOBOL baseline's indexed files (loaded from
-- app/data/ASCII, see scripts/baseline/README.md), which the golden set compares against. This is not IBM037 order
-- (lowercase < uppercase < digits); the shipped keys (numeric card/account/tran ids, ADMINnnn/USERnnnn) sort the same
-- in both.
ALTER TABLE user_security ALTER COLUMN usr_id TYPE VARCHAR(8) COLLATE "C";
ALTER TABLE card ALTER COLUMN card_num TYPE VARCHAR(16) COLLATE "C";
ALTER TABLE card_xref ALTER COLUMN card_num TYPE VARCHAR(16) COLLATE "C";
ALTER TABLE transaction_type ALTER COLUMN tran_type_cd TYPE VARCHAR(2) COLLATE "C";
ALTER TABLE transaction_category ALTER COLUMN tran_type_cd TYPE VARCHAR(2) COLLATE "C";
ALTER TABLE disclosure_group
    ALTER COLUMN acct_group_id TYPE VARCHAR(10) COLLATE "C",
    ALTER COLUMN tran_type_cd TYPE VARCHAR(2) COLLATE "C";
ALTER TABLE tran_cat_balance ALTER COLUMN tran_type_cd TYPE VARCHAR(2) COLLATE "C";
ALTER TABLE transaction
    ALTER COLUMN tran_id TYPE VARCHAR(16) COLLATE "C",
    ALTER COLUMN proc_ts TYPE VARCHAR(26) COLLATE "C";
ALTER TABLE daily_transaction ALTER COLUMN tran_id TYPE VARCHAR(16) COLLATE "C";
