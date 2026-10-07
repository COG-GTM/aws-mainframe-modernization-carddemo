-- s6.4 / ADR-0023: one-way hash of the USRSEC password next to the legacy SEC-USR-PWD column.
-- NULL until the user's first successful sign-on (or a COUSR01C/02C write) stores it; `password` keeps the
-- 8-byte copybook value so cbexport/unload and the golden set stay byte-identical.
ALTER TABLE user_security ADD COLUMN password_hash VARCHAR(100);

COMMENT ON COLUMN user_security.password_hash IS
    'BCrypt hash of SEC-USR-PWD as compared by COSGN00C (ADR-0023); not part of the VSAM record';
