# @carddemo/terminal-ui

The 3270 terminal of the CardDemo online programs, as a React + Vite app. It
renders the BMS maps in `app/bms` on a fixed 24x80 green screen and talks to the
migrated online programs through a single screen exchange call.

## Generated screen definitions

`scripts/generate-screens.ts` parses the `DFHMSD`/`DFHMDI`/`DFHMDF` macros of
every map and writes a typed `ScreenDefinition` per map into `src/generated`.
The definitions are committed, so nothing at build time depends on the COBOL
sources; regenerate them after a map changes:

```bash
npm run generate -w @carddemo/terminal-ui
```

Each field keeps its BMS geometry and attributes — position of the attribute
byte, length, protected/unprotected, numeric, intensity, colour, highlight,
`INITIAL=`, `PICIN`/`PICOUT`, `VALIDN=(MUSTFILL)` and `JUSTIFY=(RIGHT)` — which
is what the renderer places on the grid. `POS` is the attribute byte, so the
field data starts one column to its right.

## Rendering

`TerminalScreen` draws one map: literals as text, unprotected fields as inputs
in BMS order (the 3270 tab order), the `IC` field focused, `DRK` fields hidden
and the AID keys the programs act on:

| Key | AID | Used for |
| :-- | :-- | :------- |
| Enter | `ENTER` | submit |
| F3 | `PF3` | back / exit |
| F4 | `PF4` | clear the input fields |
| F5 | `PF5` | save |
| F7 / F8 | `PF7` / `PF8` | page backward / forward |
| Esc | `CLEAR` | clear the screen |

## Talking to the backend

Every interaction is one call:

```ts
const response = await client.exchange({
  program: "COSGN00C",
  screenFields: { USERID: "USER0001", PASSWD: "PASSWORD" },
  aidKey: "ENTER",
  state: null,
});
// -> { program, screenFields, message, state }
```

`createHttpScreenClient(baseUrl)` posts that to `POST {baseUrl}/screen` on the
Express API that hosts the migrated programs. `createMockScreenClient()` answers
the signon, both menus and the transaction list in the browser, so the package
runs and is tested standalone.

## Development

```bash
npm run dev -w @carddemo/terminal-ui   # http://localhost:5173 against the mock
npm run bundle -w @carddemo/terminal-ui
```

The dev server proxies `/api` to `http://localhost:3000`, where the Express API
runs.
