-- CUST-FICO-CREDIT-SCORE is PIC 9(03): the record holds 000-999. 300-850 is the COACTUPC screen
-- edit (88 FICO-RANGE-IS-VALID on ACUP-NEW-CUST-FICO-SCORE), applied to updates only; 22 of the 50
-- customers in app/data/ASCII/custdata.txt score below 300, so the column check follows the picture.
ALTER TABLE customers DROP CONSTRAINT ck_customers_fico;
ALTER TABLE customers
    ADD CONSTRAINT ck_customers_fico CHECK (fico_credit_score IS NULL OR fico_credit_score BETWEEN 0 AND 999);
