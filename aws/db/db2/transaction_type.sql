-- DB2 CARDDEMO.TRANSACTION_TYPE (TRNTYPE.ddl, index XTRAN_TYPE in XTRNTYPE.ddl) and
-- CARDDEMO.TRANSACTION_TYPE_CATEGORY (TRNTYCAT.ddl, index X_TRAN_TYPE_CATG in XTRNTYCAT.ddl)
-- from app/app-transaction-type-db2/ddl/.
--
-- Reconciliation (see ../README.md "DB2 vs VSAM reference data"):
--   The DB2 tables and the VSAM files TRANTYPE/TRANCATG hold the same entities; on the mainframe the
--   DB2 sub-app is the master and TRANEXTR.jcl re-extracts the VSAM files from DB2 (DSNTIAUL).
--   On AWS both collapse into ONE set of tables created by schema.sql:
--       CARDDEMO.TRANSACTION_TYPE           -> carddemo.transaction_type
--       CARDDEMO.TRANSACTION_TYPE_CATEGORY  -> carddemo.transaction_category
--   carddemo.transaction_type / transaction_category are the single authoritative store (the
--   maintenance API writes them directly, TRANEXTR becomes unnecessary). Seed data comes from the
--   EBCDIC VSAM files (the TRANEXTR output format); the DB2 seed statements (DB2LTTYP.ctl/DB2LTCAT.ctl)
--   contain the same keys but upper-case descriptions and are not loaded.
--
-- Column mapping:
--   TR_TYPE CHAR(2)                -> transaction_type.type_cd CHAR(2)          (PK)
--   TR_DESCRIPTION VARCHAR(50)     -> transaction_type.description VARCHAR(50)
--   TRC_TYPE_CODE CHAR(2)          -> transaction_category.type_cd CHAR(2)      (PK part 1, FK ON DELETE RESTRICT)
--   TRC_TYPE_CATEGORY CHAR(4)      -> transaction_category.cat_cd SMALLINT      (PK part 2; '0001' -> 1)
--   TRC_CAT_DATA VARCHAR(50)       -> transaction_category.description VARCHAR(50)
--   UNIQUE INDEX XTRAN_TYPE (TR_TYPE)                             -> pk_transaction_type
--   UNIQUE INDEX X_TRAN_TYPE_CATG (TRC_TYPE_CODE, TRC_TYPE_CATEGORY) -> pk_transaction_category
--
-- The views below expose the tables with the DB2 names and types so SQL ported literally from
-- COTRTLIC/COTRTUPC/COBTUPDT (and DB2 unload/compare scripts) keeps working during the transition.
-- db2_transaction_type is a simple view and fully updatable. db2_transaction_type_category is read-only
-- for trc_type_category (computed from the SMALLINT key): ported INSERTs and key changes must target
-- carddemo.transaction_category directly; only trc_cat_data can be updated through the view.
-- Idempotent. Requires schema.sql.

CREATE OR REPLACE VIEW carddemo.db2_transaction_type AS
SELECT type_cd     AS tr_type,
       description AS tr_description
  FROM carddemo.transaction_type;

CREATE OR REPLACE VIEW carddemo.db2_transaction_type_category AS
SELECT type_cd                            AS trc_type_code,
       lpad(cat_cd::text, 4, '0')::char(4) AS trc_type_category,
       description                        AS trc_cat_data
  FROM carddemo.transaction_category;

COMMENT ON VIEW carddemo.db2_transaction_type IS
    'DB2 CARDDEMO.TRANSACTION_TYPE compatibility view over transaction_type (authoritative)';
COMMENT ON VIEW carddemo.db2_transaction_type_category IS
    'DB2 CARDDEMO.TRANSACTION_TYPE_CATEGORY compatibility view over transaction_category (authoritative)';
