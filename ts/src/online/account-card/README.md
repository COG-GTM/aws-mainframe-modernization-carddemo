# Online account & card programs (Session 7)

TypeScript ports of the CICS programs `COACTVWC` (account view, CAVW), `COACTUPC` (account update, CAUP),
`COCRDLIC` (card list, CCLI), `COCRDSLC` (card detail, CCDL) and `COCRDUPC` (card update, CCUP).
Each module exports an `OnlineProgram`; they are registered in `src/online/areas/account-card.ts`.

- `common.ts` - local helpers (2000-byte WS-COMMAREA build, program commarea extraction, `WS-FILE-ERROR-MESSAGE`,
  ABEND-ROUTINE, zoned "nibble" value of unedited input). Candidates for upstreaming into `framework/helpers.ts`.
- Program-private commareas are carried after CARDDEMO-COMMAREA exactly as in COBOL
  (COACTVWC/COCRDSLC 12 bytes, COCRDLIC 254 incl. the 7 screen rows, COCRDUPC 329, COACTUPC 873).
- Tests: `test/online/account-card/` (navigation, PF keys, paging, selection, validation, seed traps, READ UPDATE/REWRITE).

## LIMITATIONS
- CICS programs cannot run under GnuCOBOL; expected values come from tracing the COBOL, not golden files.
- Bug-for-bug behaviours kept: CDEMO-FROM-PROGRAM is tested on fresh WORKING-STORAGE before DFHCOMMAREA is copied;
  COACTVWC/COACTUPC continue to the customer read after an account NOTFND (the `DID-NOT-FIND-*` 88s are never set);
  COCRDLIC re-reads from the first page on a selection error and reports "NO PREVIOUS PAGES" after PF7 lands on page 1;
  COCRDSLC/COCRDUPC read CARDDAT by card number only; COACTUPC treats a CUSTDAT lock failure as success;
  COACTUPC's ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the group id is rewritten into the ZIP slot and GROUP-ID is cleared;
  COCRDUPC never fills CCUP-NEW-CVV-CD, so the rewritten CVV is `'  0'` (sign-nibble repair of spaces).
- `MOVE DFHBMDAR TO xxxC` (attribute byte into a colour slot) is modelled as "default colour".
- FILLER areas that INITIALIZE does not touch (update-record FILLER, fresh WS) are modelled as spaces.
- Numeric tests on unedited input (`CC-ACCT-ID-N = ZEROS`, VALID-MONTH, etc.) use the CP037 low nibble of each byte.
- CSUTLDTC is called with the 8-byte CCYYMMDD date padded to X(10) and format 'YYYYMMDD' (the COBOL passes the
  adjacent WS bytes by reference); only reached after all CSUTLDPY edits pass.
