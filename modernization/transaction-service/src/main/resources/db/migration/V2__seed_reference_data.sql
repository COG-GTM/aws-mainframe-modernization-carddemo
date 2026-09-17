-- Reference data matching the sample TRANTYPE, TRANCATG and DISCGRP files of the demo.
INSERT INTO transaction_types (type_cd, type_desc)
VALUES ('01', 'Purchase'),
       ('02', 'Payment'),
       ('03', 'Credit'),
       ('04', 'Authorization'),
       ('05', 'Refund');

INSERT INTO transaction_categories (type_cd, cat_cd, cat_type_desc)
VALUES ('01', 1, 'Regular Sales Draft'),
       ('01', 5, 'Interest charged'),
       ('02', 2, 'Bill payment'),
       ('03', 1, 'Merchant credit'),
       ('05', 1, 'Refund to card');

INSERT INTO disclosure_groups (acct_group_id, type_cd, cat_cd, int_rate)
VALUES ('DEFAULT', '01', 1, 19.99),
       ('DEFAULT', '01', 5, 19.99),
       ('DEFAULT', '02', 2, 0.00),
       ('ZEROAPR', '01', 1, 0.00),
       ('ZEROAPR', '01', 5, 0.00);
