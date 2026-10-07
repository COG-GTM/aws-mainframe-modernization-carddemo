# Sign-on: "first password entry rejected once" (s6.4 item 1.7)

**Observation (s6.3, UNT51-25 recording):** the first password entry on the sign-on page was rejected once, the
second identical entry signed on. No cause was found then.

**Conclusion: not reproduced, no defect found in the app; no code change.** Evidence below, against the compose stack
of this branch (`docker compose up -d --build --wait` in `modernization/`, app + nginx UI + PostgreSQL 16, fresh
`initial-load`, 2026-10-07).

## API (`POST /api/v1/auth/login`, through the nginx UI proxy and directly)

| # | Request | Result |
| --- | --- | --- |
| 1 | USER0001 / `PASSWORD`, very first login (no `password_hash` yet) | 200, token, `toProgram=COMEN01C`; `password_hash` now `{bcrypt}…` |
| 2 | same again (compared against the hash) | 200, `COMEN01C` |
| 3 | `user0001` / `password` (lower case) | 200 — COSGN00C upper-cases both (R-7) |
| 4 | USER0001 / `WRONGPWD` | 401 `WRONG_PASSWORD` |
| 5 | USER0001 / `PASS` | 401 `WRONG_PASSWORD` (padded to 8, not a prefix match) |
| 6 | USER0001 / `PASSWORD ` (9 characters) | 400 `INVREQ` (field longer than `PIC X(08)`) |
| 7 | ADMIN001 / `PASSWORD` twice | 200, 200, `COADM01C` |
| 8 | USER0002 / `PASSWORD` × 5 **in parallel**, all before the first hash exists | 5 × 200; one hash stored (guarded `UPDATE … WHERE password_hash IS NULL`) |

No warning or error in the app log for any of these.

## UI (Playwright, Chromium, `modernization/carddemo-ui/e2e/signon-first-entry.spec.ts`)

Each case opens `/signon` in a **new browser context** (empty sessionStorage), enters the credentials once and
expects the main menu (`COMEN01C`) on the first attempt; run with `--repeat-each=5`:

| Case | Attempts | First-attempt failures |
| --- | --- | --- |
| `fill` both fields + Enter immediately after navigation (before the screen header request returns) | 15 | 0 |
| typed at 40 ms/key in lower case, Tab between the fields, Enter | 15 | 0 |
| `fill` + click the `ENTER` PF button | 15 | 0 |
| USER0001 signs on, F3 sign-off, ADMIN001 signs on in the same tab | 5 | 0 |

`20 passed (33.2s)`. The spec stays in the e2e suite (CI `compose` job), so a regression would fail CI.

## Why the app cannot reject a correct first entry

- `SignonPage` keeps both fields in React state and submits the current values (the PF-key handler reads the
  latest `pfKeys` through a ref; the form submit uses the render's closure, which includes the latest state).
- The server path is stateless: COSGN00C R-5..R-8 upper-case, pad to 8, compare; the first-login hash upgrade runs
  **after** a successful compare and a failure to store the hash only logs a warning (ADR-0023), it never turns a
  correct password into a rejection.

## Most likely explanation of the s6.3 observation (inference, not verified)

The recording was driven through the desktop browser by keystrokes: the password field has `maxLength=8` and the
User ID field auto-focuses, so keystrokes sent before focus moved (or browser autofill of the User ID field) give a
wrong value for one attempt; the app answered `Wrong Password. Try again ...` exactly as COSGN00C would. The final
s6.4 recording is used to re-check this.
