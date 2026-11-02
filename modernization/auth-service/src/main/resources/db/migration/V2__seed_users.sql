-- Local demo accounts only, replacing the sample USRSEC records of the mainframe demo.
-- Both hashes are BCrypt of the throwaway demo password 'Password1'; rotate before any
-- deployment outside a developer laptop or a demo environment.
INSERT INTO users (user_id, first_name, last_name, password_hash, user_type)
VALUES ('ADMIN001', 'Admin', 'User',
        '$2b$10$84oZFZ2mGUczwCTJa9MN8uw5rTxs0W24a6MJ6k9Zcnx3PXcz/mfd.', 'A'),
       ('USER0001', 'Regular', 'User',
        '$2b$10$84oZFZ2mGUczwCTJa9MN8uw5rTxs0W24a6MJ6k9Zcnx3PXcz/mfd.', 'U');
