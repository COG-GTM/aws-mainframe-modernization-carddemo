# ADR-0022: React + Vite SPA per BMS map, served by nginx in front of the API

- Status: Accepted (UNT51-22, 2026-10-07); builds on ADR-0017 (JWT), ADR-0019 (error body, `NavigationContext`), ADR-0020 (PAN masking)
- Applies to: `modernization/carddemo-ui/`, the `carddemo-ui` compose service, the `ui` CI job

## Context
The 17 BMS maps in `app/bms` were the only user interface; phase 5 exposed every online program as a REST endpoint
(`/api/v1`). Decision `d-ui` fixed a modern web UI with one page per map (React + Vite, not a terminal emulation, not
Swagger only); `d-decomp` keeps one Spring Boot application, so the UI must be a separate static front end that only
talks to that API. The prior `devin/1790618699-frontend-react` branch was rated "harvest": its screen components were
reusable, its mocked API client was not.

## Decision
1. **Stack.** React 18, TypeScript (strict), Vite, react-router; one route per map, registered by program id in
   `src/programs.ts`. The field inventory of a page is its symbolic map `app/cpy-bms/<MAP>.CPY` (each input element
   carries `data-bms`, enforced by `bmsFields.test.ts`); the header shows the transaction id and program like the COBOL
   `ScreenHeader`; PF keys are buttons with F-key shortcuts; the message area shows the API's message text.
2. **The API is the rule engine.** The browser enforces only what the map enforces (lengths, numeric-only, upper
   case); every edit, message and navigation decision comes from the API. Pages route by `NavigationContext.toProgram`
   (never by menu option number) and F3 follows the API's exit context or `fromProgram`, falling back to the menu of
   the role. An `ApiError` (`code`/`field`/`message`) is shown in the message area and focuses `field`.
3. **Auth storage.** The JWT, user id/type/role and the current `NavigationContext` (COMMAREA replacement) live in
   React state mirrored to `sessionStorage` (per tab, cleared when the tab closes; never `localStorage` or cookies).
   Every call sends `Authorization: Bearer`; a 401 `SIGNON_REQUIRED` clears the session and returns to sign-on.
   Admin routes are hidden for a USER and show `No access - Admin Only option...`; the API remains the enforcement
   point (403 `NOTAUTH`).
4. **PAN masking.** Lists show the API's masked PAN; selection and detail/update calls use the opaque `cardRef`,
   so a full PAN is only on the single-card screens the API returns it for, and never in URLs or `sessionStorage`.
5. **Serving and proxying.** Production: multi-stage image (`npm ci && npm run build` on Node 20 → nginx) serving
   the static build and reverse-proxying `/api/`, `/v3/` and `/swagger-ui*` to `carddemo-app`, so the browser sees
   one origin (no CORS configuration in the API). Compose publishes it on `CARDDEMO_UI_PORT` (default 8085) on
   `CARDDEMO_BIND_ADDRESS`. Development: the Vite dev server proxies the same paths to `CARDDEMO_API_URL`
   (default `http://localhost:8084`).
6. **Tests.** Vitest + React Testing Library with a fetch stub of the real endpoints (routing, PF keys, field focus,
   role gating, BMS field coverage); Playwright (`npm run e2e`) drives the USER and ADMIN flows against the compose
   stack. No MSW mock API is kept.

## Consequences
- The token is readable by scripts in the page (XSS would expose it for up to its 1 h lifetime); mitigated by React
  escaping, no third-party scripts and a same-origin deployment. An httpOnly cookie session is the follow-up if the
  UI is exposed beyond the internal network.
- UI and API deploy together behind one origin; a separately hosted UI would need CORS on the API.
- A screen change in the API (new field, new message) needs no UI rule change; a new map needs a page and a row in
  `docs/modernization/08-ui-map.md`.
