-- USRSEC VSAM KSDS (copybook CSUSR01Y, record length 80), key SEC-USR-ID.
-- SEC-USR-PWD PIC X(08) held a clear text password; it becomes a BCrypt hash column.
CREATE TABLE users (
    user_id       VARCHAR(8)   NOT NULL,
    first_name    VARCHAR(20),
    last_name     VARCHAR(20),
    password_hash VARCHAR(100) NOT NULL,
    user_type     CHAR(1)      NOT NULL,
    CONSTRAINT pk_users PRIMARY KEY (user_id),
    CONSTRAINT ck_users_type CHECK (user_type IN ('A', 'U'))
);
