INSERT INTO customers (cust_id, first_name, middle_name, last_name, addr_line_1, addr_line_2,
                       addr_state_cd, addr_country_cd, addr_zip, phone_num_1, ssn, govt_issued_id,
                       date_of_birth, eft_account_id, pri_card_holder_ind, fico_credit_score)
VALUES (100000001, 'JOHN', 'A', 'DOE', '123 MAIN ST', 'APT 4', 'IL', 'USA', '60601',
        '(312)555-0101', 123456789, 'IL-DL-9912345', '1980-04-12', '1234567890', 'Y', 720),
       (100000002, 'ALICE', 'B', 'ROE', '9 BROADWAY', NULL, 'NY', 'USA', '10001',
        '(212)555-0102', 223456789, 'NY-DL-8812345', '1975-11-30', '2234567890', 'Y', 680),
       (100000003, 'BOB', 'C', 'POE', '500 MARKET ST', NULL, 'CA', 'USA', '94105',
        '(415)555-0103', 323456789, 'CA-DL-7712345', '1990-02-05', '3234567890', 'N', 610);
