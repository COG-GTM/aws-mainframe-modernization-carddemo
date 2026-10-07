# ADR-0020: Card numbers masked in lists and logs, opaque card references, USER card scope

- Status: Accepted (UNT51-19, 2026-10-07); extends ADR-0017 (JWT key material) and ADR-0019 (`NOTAUTH`)
- Applies to: `com.carddemo.web.card`, `com.carddemo.card.online`, `com.carddemo.common.PanMask`, every log line that
  names a card

## Context
`CARD-NUM` is a primary account number (PAN). The COBOL programs mask nothing: COCRDLIC shows all 16 digits on every
list row (`CRDNUMn`), the COMMAREA carries `CDEMO-CARD-NUM` in clear, and abend/file-error messages may include it.
On a 3270 that exposure is limited to the terminal; a JSON API is cached, logged, proxied and stored by browsers,
which brings it into PCI DSS scope (requirement 3.4: render PAN unreadable wherever it is stored or displayed, show at
most the last four digits when the full number is not needed).

COCRDLIC's header says it lists "all cards if no context passed and admin user, only the ones associated with ACCT in
COMMAREA if user is not admin", but no paragraph tests `CDEMO-USRTYP-*` and the COMMAREA account is never used as a
filter (rules doc COCRDLIC R-2, R-19): every signed-on user can list every card. CardDemo has no user → account link
(USRSEC holds id, names, password and type only).

## Decision
1. **Masking (deliberate improvement over the COBOL).** `PanMask.mask` keeps the last four digits
   (`0500024453765740` → `************5740`). It is applied to
   - every `GET /api/v1/cards` row (`cardNumber`) and the echoed card filter;
   - every log line and exception message produced by the card services (e.g. the optimistic-lock identifier).
2. **Full PAN where the COBOL screen shows it on a single-card screen.** `GET /api/v1/cards/{cardNumber}` and the
   `PUT` response return all 16 digits in `cardNumber`, as CCRDSLA/CCRDUPA display `CARDSID`. The client already had
   to send the number (or a reference) to get there.
3. **Opaque card references.** List rows carry `cardRef`: the card number encrypted with AES-256-GCM under a key
   derived from `CARDDEMO_JWT_SECRET` (SHA-256 of a purpose label + secret), nonce = HMAC-SHA256 of the number
   (deterministic: the same card always has the same reference; tampering fails the GCM tag). Every card endpoint
   accepts a reference wherever it accepts a card number (path `{cardNumber}`, `cardNumber` filter, `after`/`before`
   cursors), so a client can page, select, view and update without ever handling a list PAN. Rotating the secret
   invalidates references, like tokens. The selection response (`POST /api/v1/cards/selection`) returns the
   `NavigationContext` with `acctId` and **no** `cardNum`, plus the `cardRef`.
4. **USER scope = "the account in context".** The account in context is the account the request names
   (`CDEMO-ACCT-ID`: the `accountId` filter / search key). Implementing the header's intent:
   - `GET /api/v1/cards`: an ADMIN without an account sees every card (COBOL behaviour); a USER without an account
     gets 403 `NOTAUTH` and with one only sees that account's cards.
   - `GET`/`PUT /api/v1/cards/{cardNumber}`: for a USER the card must belong to the account given, else 404 NOTFND
     (`Did not find cards for this search condition`), so other accounts' cards are indistinguishable from missing
     ones. An ADMIN keeps the COBOL behaviour (typed account not cross-checked, COCRDSLC R-16 / COCRDUPC R-25).
   - `POST /api/v1/cards/selection`: a USER sends the list's `accountId` (403 `NOTAUTH` without it); a selected card
     of another account is 404 NOTFND, so a reference obtained elsewhere does not reveal its account.
   Ownership of an account by a user cannot be enforced until USRSEC (or a successor) links users to accounts; this
   ADR scopes, it does not authorise per account.

## Consequences
- Lists are not byte-for-byte the COBOL screen: `cardNumber` differs by design; the order, page size, filters and
  messages are unchanged and are what the rules tests and the curl transcript assert.
- Clients must use `cardRef` (or keep the number they typed) for follow-up requests.
- Follow-up when a user → account relation exists: replace the "account named in the request" with an authorisation
  check in `CardController`.
- UNT51-20 (transactions, bill payment): COTRN00C/01C/02C and COBIL00C check no account ownership, so their
  endpoints do not scope by account either; the transaction list (COTRN0A) has no card number column, and the
  detail (COTRN1A) shows the full number as the map does.
