# ADR-0008: `EXEC CICS XCTL` → controller dispatch; `EXEC CICS LINK` / `CALL` → service call

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
- `XCTL PROGRAM(x) COMMAREA(...)` transfers control and never returns. In Java it is navigation: the controller
  returns a redirect/next-page descriptor to the UI (one page per BMS map, decision `d-ui`), carrying ids per
  ADR-0007. Server-side code never invokes another program's controller.
- Menu dispatch tables (`COMEN02Y`, `COADM02Y`: option → program) become a static list of options with target routes,
  filtered by user type exactly as the COBOL filtered them.
- `LINK PROGRAM(x)` and static/dynamic `CALL 'x'` return to the caller: they become a Spring service method call in
  the owning domain, honouring ADR-0001 dependencies. (The core online programs use no `LINK` today; batch
  `CALL`s are to `CEE3ABD`, ADR-0013, and date utilities that become `java.time`.)
- `RETURN TRANSID(...) COMMAREA(...)` (pseudo-conversation end) is the HTTP response; `RETURN` without TRANSID back
  to the sign-on screen is sign-out.
- Transaction ids (`CC00`, `CM00`, `CA00`, `CAVW`, `CAUP`, `CCLI`, `CCDL`, `CCUP`, `CT00`-`CT02`, `CB00`, `CR00`,
  `CU00`-`CU03`) are kept as route names/labels for traceability, not as dispatch keys.
