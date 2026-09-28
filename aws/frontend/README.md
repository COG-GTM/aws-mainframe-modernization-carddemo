# CardDemo frontend (BMS/3270 → React)

React 18 + TypeScript + Vite + React Router replacement for the CardDemo CICS/BMS screens
(`app/bms/*.bms`, `app/cpy-bms/`). It calls the online-services REST API defined in
`aws/contracts/api.md` and ships a Mock Service Worker (MSW) implementation of that contract so it runs
standalone.

## Run

```bash
npm ci
cp .env.example .env.local     # VITE_USE_MOCKS=true, VITE_API_BASE_URL empty
npm run dev                    # http://localhost:5173 (MSW mocks when VITE_USE_MOCKS=true)
npm run dev:mock               # same, forcing mocks on
```

Mock sign-on (from `app/jcl/DUSRSECJ.jcl`): `ADMIN001` / `PASSWORD` (admin menu), `USER0001` / `PASSWORD`
(main menu). Mock data is regenerated from `app/data/ASCII/` with `npm run seed`; it is in-memory and resets on
reload.

| Variable | Meaning |
|---|---|
| `VITE_API_BASE_URL` | Origin of the backend; the client appends `/api/v1`. Empty = same origin. |
| `VITE_USE_MOCKS` | `true` starts MSW (`public/mockServiceWorker.js`) before rendering. |

## Scripts

`npm run lint` · `npm run typecheck` · `npm run test` (Vitest + React Testing Library + MSW node server) ·
`npm run build` (static bundle in `dist/`).

## 3270 conventions

Every screen renders the BMS header (tran ID, program, title, date/time), the info line, the `ERRMSG`
message line and a PF-key bar. Keys work from the keyboard and as buttons: `Enter` submit, `F3` back/exit,
`F4` clear, `F5` save/confirm, `F7`/`F8` page backward/forward, `F12` cancel. Field edits and message texts
follow the COBOL programs (e.g. `COACTUPC` paragraph order; only the first failing edit is reported). Menus
come from `GET /api/v1/menus/main` / `GET /api/v1/menus/admin` and are filtered by the JWT `role` claim (`ADMIN` → `/admin`, `USER` → `/menu`).

## BMS map → route

| BMS mapset / map | Program | Tran | Route |
|---|---|---|---|
| COSGN00 / COSGN0A | COSGN00C | CC00 | `/login` |
| COMEN01 / COMEN1A | COMEN01C | CM00 | `/menu` |
| COADM01 / COADM1A | COADM01C | CA00 | `/admin` |
| COACTVW / CACTVWA | COACTVWC | CAVW | `/accounts/view` |
| COACTUP / CACTUPA | COACTUPC | CAUP | `/accounts/update` |
| COCRDLI / CCRDLIA | COCRDLIC | CCLI | `/cards` |
| COCRDSL / CCRDSLA | COCRDSLC | CCDL | `/cards/view` |
| COCRDUP / CCRDUPA | COCRDUPC | CCUP | `/cards/update` |
| COTRN00 / COTRN0A | COTRN00C | CT00 | `/transactions` |
| COTRN01 / COTRN1A | COTRN01C | CT01 | `/transactions/view` |
| COTRN02 / COTRN2A | COTRN02C | CT02 | `/transactions/new` |
| COBIL00 / COBIL0A | COBIL00C | CB00 | `/bill-payment` |
| CORPT00 / CORPT0A | CORPT00C | CR00 | `/reports` |
| COUSR00 / COUSR0A | COUSR00C | CU00 | `/admin/users` |
| COUSR01 / COUSR1A | COUSR01C | CU01 | `/admin/users/new` |
| COUSR02 / COUSR2A | COUSR02C | CU02 | `/admin/users/:userId/edit` |
| COUSR03 / COUSR3A | COUSR03C | CU03 | `/admin/users/:userId/delete` |

Optional sub-app maps (`COPAU00/01`, `COTRTLI/UP`) are not built; their menu options are shown as
*not installed* (see `aws/migration-inventory.md` §9).

## Container

```bash
docker build -t carddemo-frontend --build-arg VITE_API_BASE_URL=https://api.example.com .
docker run -p 8080:8080 carddemo-frontend      # nginx static, SPA fallback, /healthz
```
