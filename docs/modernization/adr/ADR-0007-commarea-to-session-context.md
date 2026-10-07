# ADR-0007: COMMAREA → authenticated session context

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
`CARDDEMO-COMMAREA` (`COCOM01Y`) carries, between pseudo-conversational tasks: signed-on user id and type,
from/to program and transaction ids, the program-enter flag, and the selected customer/account/card. Program-specific
extensions follow it (e.g. paging keys in `COCRDLIC`, the old/new record images in `COACTUPC`).

## Decision
- Sign-on (`COSGN00C`) authenticates against `USRSEC` and issues a server-side session (Spring Security). The
  principal holds `CDEMO-USER-ID` and `CDEMO-USER-TYPE` (enum, ADR-0006); admin-only programs (`COADM01C`,
  `COUSR0*C`) require role `ADMIN`.
- Selected entity ids (`CDEMO-ACCT-ID`, `CDEMO-CARD-NUM`, `CDEMO-CUST-ID`) travel in the URL/path or request body,
  not in hidden session state, so pages are bookmarkable and requests are stateless apart from the principal.
- `CDEMO-FROM-PROGRAM`/`CDEMO-TO-PROGRAM` become navigation (return URL) handled by the UI; the server does not
  replay them.
- Program-specific COMMAREA extensions: paging keys → query parameters (`?startKey=`); old record images used to
  detect concurrent change → the entity `@Version` (ADR-0010).
- Never trust client-supplied user id or type; always take them from the authenticated principal.
