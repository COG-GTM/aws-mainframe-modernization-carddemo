# @carddemo/online-usradmin

The CICS user administration transactions, ported from `app/cbl`:

| Program    | Transaction | Map       | Handler            |
| ---------- | ----------- | --------- | ------------------ |
| `COUSR00C` | `CU00`      | `COUSR00` | `handleUserList`   |
| `COUSR01C` | `CU01`      | `COUSR01` | `handleUserAdd`    |
| `COUSR02C` | `CU02`      | `COUSR02` | `handleUserUpdate` |
| `COUSR03C` | `CU03`      | `COUSR03` | `handleUserDelete` |

Each handler takes the map fields, the AID key and the COMMAREA, and returns
the map fields to send, the COMMAREA to return and the program to transfer to,
mirroring `COCOM01Y` and the `XCTL` / `RETURN TRANSID` pairs in the COBOL.
Validations, message text, `FILE STATUS` handling (`22` duplicate, `23` not
found) and the PF-key table follow the programs line by line.

`createUserAdminRouter` / `createUserAdminApp` expose them over HTTP; the
transport only decodes the body and serialises the response.

```ts
const file = openUserSecurityFile(writeUsrsecAscii("/tmp/usrsec.txt"));
file.openFile();
createUserAdminApp(file).listen(3000);
```

## Data

`USRSEC` ships only as `app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS`; there is no
`app/data/ASCII/usrsec.txt`. `writeUsrsecAscii` transcodes it (code page 037)
into a writable ASCII copy, which is how the tests run against the real
sign-on users without touching the repository data.

## COMMAREA types

`@carddemo/online-signon` was not available when this package was written, so
`src/commarea.ts` and `src/screen.ts` hold local equivalents of the shared
COMMAREA and screen state. They should be consolidated into the shared package
once it lands.
