# ADR-0018: USRSEC passwords stay plain text (COBOL-compatible compare); hashing is a follow-up

- Status: Accepted (UNT51-17, 2026-10-07)
- Applies to: `com.carddemo.user.signon.SignOnService`, `user_security.password`; later COUSR01C/COUSR02C

## Context
`SEC-USR-PWD PIC X(08)` holds the password in clear text. COSGN00C upper-cases the input (`FUNCTION UPPER-CASE`)
and compares it with `=` against the stored field, so the comparison is case-insensitive in effect and blanks pad
to 8 bytes. The migrated `user_security` rows (initial load of `AWS.M2.CARDDEMO.USRSEC.PS`) are clear text too, and
the parity requirement (`d-verify`) is that the same credentials sign on with the same messages.

## Decision
- Keep the stored passwords as they are and compare like COBOL: input upper-cased (ASCII letters only), both values
  compared as space-padded 8-byte fields; input longer than 8 is rejected by the API (`@Size(max = 8)`) and could
  never match. Wrong password and unknown user stay distinguishable (`Wrong Password. Try again ...` vs
  `User not found. Try again ...`), as COBOL shows them.
- Passwords are never logged or returned: `LoginRequest.toString()` masks it; the response carries only the token.

## Follow-up (not in this step)
1. Add a `password_hash` column (Flyway) holding `{bcrypt}`/`{argon2}` values via Spring Security's
   `DelegatingPasswordEncoder`; keep `{noop}`-style plain text as the legacy id during transition.
2. On each successful sign-on, re-hash the upper-cased password and clear the plain-text column (upgrade on login);
   COUSR01C/COUSR02C write hashes only.
3. Once all rows are hashed, drop the plain-text column and the export of `SEC-USR-PWD` (cbexport) or export a
   placeholder; decide then whether the case-insensitive rule is kept.
4. Consider rate limiting / lockout for repeated wrong passwords and constant-time responses for unknown users —
   both deliberately absent now because COBOL has neither.

## Consequences
The sample database contains usable clear-text credentials (`ADMIN001`/`PASSWORD`, `USER0001`/`PASSWORD`); treat any
non-local database as sensitive until the follow-up ships.
