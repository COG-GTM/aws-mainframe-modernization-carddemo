# ADR-0023: BCrypt `password_hash` next to `SEC-USR-PWD`, upgraded on first successful sign-on

- Status: Accepted (UNT51-26, 2026-10-07); supersedes the "Decision" compare rule and "Follow-up" items 1–2 of
  ADR-0018 (the COBOL comparison semantics of ADR-0018 stay)
- Applies to: `user_security.password_hash` (Flyway V6), `com.carddemo.user.UserPasswords`,
  `user.signon.SignOnService`, `user.admin.UserAddService` / `UserUpdateService`, `web.security.CurrentAdminAuthorization`

## Context
ADR-0018 kept USRSEC passwords in clear text and compared them like COSGN00C (upper-cased, space-padded
`PIC X(08)`), with hashing as a follow-up. The golden set (`d-verify`), `cbexport`/`unload` and the EBCDIC re-encode
tests need `SEC-USR-PWD` to round-trip byte for byte, so the 8-byte field cannot be replaced in this step.

## Decision
1. **Additional column.** `user_security.password_hash VARCHAR(100)` (nullable) holds a Spring Security
   `DelegatingPasswordEncoder` value (`{bcrypt}$2a$10$...`). It is not part of the copybook record:
   `UserSecurity.toRecord()` and the copybook column map are unchanged, so `unload`/`cbexport` and the golden set are
   byte-identical (`UserPasswordWriteTest`, `PasswordAndAdminHardeningIT`).
2. **What is hashed.** The COBOL comparison value: the upper-cased input padded to 8 bytes
   (`UserPasswords.pic8`), so sign-on semantics (case-insensitive, trailing blanks) do not change.
3. **Upgrade on first successful sign-on (not at `initial-load`).** While `password_hash` is null, sign-on compares
   in plain text as before; after a match, `UserSecurityRepository.storePasswordHash` writes the hash with a guarded
   `UPDATE ... WHERE password_hash IS NULL AND password = :password` (a concurrent password change wins). Once set,
   only the hash is compared. A failed hash write is logged (`WARN`, no password) and does not fail a correct sign-on.
   Chosen over hashing at `initial-load` because `initial-load` and `repro` are byte-faithful VSAM loads used by the
   equivalence runs, and rows loaded later (REPRO, USRSEC restore) would otherwise stay unhashed.
4. **COUSR01C / COUSR02C** write both the 8-byte field and a fresh hash on add, and on update when the password
   changed or the row has no hash yet.
5. **Admin checks are current.** `/api/v1/users/**` and the admin menu require the JWT `ADMIN` role **and** a
   `user_security` lookup on every request that still finds the user with type `A`; demoted, deleted or unreadable
   → 403 `NOTAUTH` (`No access - Admin Only option...`) immediately, not at JWT expiry.

## Consequences
- The clear-text column remains, so the database is still sensitive (ADR-0018 Consequences stand). Dropping it is a
  cutover decision (`11-handover.md`): it ends `SEC-USR-PWD` export parity and needs the golden set changed.
- Sign-on of a never-signed-on user is unchanged; the first sign-on costs one BCrypt hash (~70 ms at strength 10).
- No lockout / rate limiting yet (COBOL has none); listed in `docs/validation/hardening/review-findings.md`.
