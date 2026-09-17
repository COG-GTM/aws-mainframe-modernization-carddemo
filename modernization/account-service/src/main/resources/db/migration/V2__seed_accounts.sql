-- Demo rows matching the account ids used by the sample VSAM data in app/data.
INSERT INTO accounts (acct_id, active_status, curr_bal, credit_limit, cash_credit_limit,
                      open_date, expiration_date, reissue_date, curr_cyc_credit, curr_cyc_debit,
                      addr_zip, group_id)
VALUES (11111111111, 'Y', 1250.00, 10000.00, 2000.00, '2020-01-01', '2030-01-01', '2025-01-01',
        0.00, 0.00, '60601', 'ZEROAPR'),
       (22222222222, 'Y', 430.75, 5000.00, 1000.00, '2019-06-15', '2029-06-15', '2024-06-15',
        0.00, 0.00, '10001', 'DEFAULT'),
       (33333333333, 'N', 0.00, 2500.00, 500.00, '2018-03-01', '2023-03-01', '2021-03-01',
        0.00, 0.00, '94105', 'DEFAULT');
