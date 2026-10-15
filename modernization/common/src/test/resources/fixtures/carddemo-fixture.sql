-- Shared parity-test fixture. Users all have password PASSWORD (BCrypt), matching DUSRSECJ seed users.
DELETE FROM daily_transaction_reject;
DELETE FROM daily_transaction;
DELETE FROM report_request;
DELETE FROM card_transaction;
DELETE FROM tran_cat_balance;
DELETE FROM card_xref;
DELETE FROM card;
DELETE FROM account;
DELETE FROM customer;
DELETE FROM disclosure_group;
DELETE FROM transaction_category;
DELETE FROM transaction_type;
DELETE FROM user_security;

INSERT INTO user_security (user_id, first_name, last_name, password_hash, user_type) VALUES
 ('ADMIN001', 'MARGARET', 'GOLD', '$2a$10$1oTTDss99erj6vjJNzg9.Oe25EdyLpxWbQxkWJ6h3jqAH7VdlBaZa', 'A'),
 ('USER0001', 'LAWRENCE', 'THOMAS', '$2a$10$1oTTDss99erj6vjJNzg9.Oe25EdyLpxWbQxkWJ6h3jqAH7VdlBaZa', 'U'),
 ('USER0002', 'AJITH', 'KUMAR', '$2a$10$1oTTDss99erj6vjJNzg9.Oe25EdyLpxWbQxkWJ6h3jqAH7VdlBaZa', 'U');

INSERT INTO transaction_type (type_cd, description) VALUES
 ('01', 'Purchase'), ('02', 'Payment'), ('03', 'Credit'), ('04', 'Authorization'), ('05', 'Refund'),
 ('06', 'Reversal'), ('07', 'Adjustment');

INSERT INTO transaction_category (type_cd, cat_cd, description) VALUES
 ('01', 1, 'Regular Sales Draft'), ('01', 2, 'Regular Cash Advance'), ('01', 3, 'Convenience Check Debit'),
 ('01', 4, 'ATM Cash Advance'), ('01', 5, 'Interest Amount'), ('02', 1, 'Cash payment'),
 ('02', 2, 'Electronic payment'), ('02', 3, 'Check payment'), ('03', 1, 'Credit to Account');

INSERT INTO disclosure_group (acct_group_id, type_cd, cat_cd, int_rate) VALUES
 ('A000000000', '01', 1, 15.00), ('A000000000', '01', 2, 25.00),
 ('DEFAULT', '01', 1, 15.00), ('DEFAULT', '01', 2, 25.00), ('DEFAULT', '02', 1, 0.00),
 ('ZEROAPR', '01', 1, 0.00);

INSERT INTO customer (cust_id, first_name, middle_name, last_name, addr_line_1, addr_line_2, addr_line_3,
                      addr_state_cd, addr_country_cd, addr_zip, phone_num_1, phone_num_2, ssn, govt_issued_id, dob,
                      eft_account_id, pri_card_holder_ind, fico_credit_score) VALUES
 (1, 'Immanuel', 'Madeline', 'Kessler', '618 Deshaun Route', 'Apt. 802', 'Altenwerthshire', 'NC', 'USA', '12546',
  '(908)119-8310', '(373)693-8684', '020973888', '00000000000049368437', '1961-06-08', '0053581756', 'Y', 274),
 (2, 'Enrico', 'April', 'Rosenbaum', '4917 Myrna Flats', 'Apt. 453', 'West Bernita', 'IN', 'USA', '22770',
  '(429)706-9510', '(744)950-5272', '587518382', '00000000000005956121', '1961-10-08', '0069194009', 'Y', 268),
 (3, 'Larry', 'Cody', 'Homenick', '362 Esta Parks', 'Apt. 390', 'New Gladyce', 'GA', 'USA', '19852',
  '(950)396-9024', '(685)168-8826', '317460867', '00000000000081704354', '1987-11-30', '0011251856', 'Y', 616);

INSERT INTO account (acct_id, active_status, curr_bal, credit_limit, cash_credit_limit, open_date, expiration_date,
                     reissue_date, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id) VALUES
 (1, 'Y', 194.00, 2020.00, 1020.00, '2014-11-20', '2099-05-20', '2025-05-20', 0.00, 0.00, '12546', 'A000000000'),
 (2, 'Y', 0.00, 1000.00, 500.00, '2013-06-19', '2099-12-31', '2024-08-11', 0.00, 0.00, '22770', NULL),
 (3, 'Y', 450.00, 500.00, 100.00, '2010-01-01', '2020-01-01', '2019-01-01', 400.00, 0.00, '19852', 'ZEROAPR');

INSERT INTO card (card_num, acct_id, cvv_cd, embossed_name, expiration_date, active_status) VALUES
 ('4111111111111111', 1, '747', 'Immanuel Kessler', '2099-03-09', 'Y'),
 ('4111111111111112', 1, '128', 'Immanuel Kessler', '2099-03-09', 'N'),
 ('4222222222222222', 2, '567', 'Enrico Rosenbaum', '2099-07-13', 'Y'),
 ('4333333333333333', 3, '011', 'Larry Homenick', '2019-12-31', 'Y');

INSERT INTO card_xref (card_num, cust_id, acct_id) VALUES
 ('4111111111111111', 1, 1), ('4111111111111112', 1, 1), ('4222222222222222', 2, 2), ('4333333333333333', 3, 3);

INSERT INTO tran_cat_balance (acct_id, type_cd, cat_cd, balance) VALUES
 (1, '01', 1, 100.00), (1, '01', 2, 50.00), (2, '01', 1, 200.00), (3, '01', 1, 300.00);
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000001', '01', 1, 'POS TERM', 'Purchase at Store 01', 10.01, 800000001, 'Store 01', 'Springfield', '12345', '4111111111111111', '2022-06-01 10:00:00', '2022-06-01 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000002', '01', 1, 'POS TERM', 'Purchase at Store 02', 20.02, 800000002, 'Store 02', 'Springfield', '12345', '4111111111111111', '2022-06-02 10:00:00', '2022-06-02 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000003', '01', 1, 'POS TERM', 'Purchase at Store 03', 30.03, 800000003, 'Store 03', 'Springfield', '12345', '4111111111111111', '2022-06-03 10:00:00', '2022-06-03 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000004', '01', 1, 'POS TERM', 'Purchase at Store 04', 40.04, 800000004, 'Store 04', 'Springfield', '12345', '4111111111111111', '2022-06-04 10:00:00', '2022-06-04 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000005', '01', 1, 'POS TERM', 'Purchase at Store 05', 50.05, 800000005, 'Store 05', 'Springfield', '12345', '4111111111111111', '2022-06-05 10:00:00', '2022-06-05 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000006', '01', 1, 'POS TERM', 'Purchase at Store 06', 60.06, 800000006, 'Store 06', 'Springfield', '12345', '4111111111111111', '2022-06-06 10:00:00', '2022-06-06 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000007', '01', 1, 'POS TERM', 'Purchase at Store 07', 70.07, 800000007, 'Store 07', 'Springfield', '12345', '4111111111111111', '2022-06-07 10:00:00', '2022-06-07 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000008', '01', 1, 'POS TERM', 'Purchase at Store 08', 80.08, 800000008, 'Store 08', 'Springfield', '12345', '4111111111111111', '2022-06-08 10:00:00', '2022-06-08 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000009', '01', 1, 'POS TERM', 'Purchase at Store 09', 90.09, 800000009, 'Store 09', 'Springfield', '12345', '4111111111111111', '2022-06-09 10:00:00', '2022-06-09 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000010', '01', 1, 'POS TERM', 'Purchase at Store 10', 100.10, 800000000, 'Store 10', 'Springfield', '12345', '4111111111111111', '2022-06-10 10:00:00', '2022-06-10 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000011', '01', 1, 'POS TERM', 'Purchase at Store 11', 110.11, 800000001, 'Store 11', 'Springfield', '12345', '4222222222222222', '2022-06-11 10:00:00', '2022-06-11 10:00:00');
INSERT INTO card_transaction (tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts) VALUES ('0000000000000012', '01', 1, 'POS TERM', 'Purchase at Store 12', 120.12, 800000002, 'Store 12', 'Springfield', '12345', '4222222222222222', '2022-06-12 10:00:00', '2022-06-12 10:00:00');
ALTER SEQUENCE tran_id_seq RESTART WITH 13;
