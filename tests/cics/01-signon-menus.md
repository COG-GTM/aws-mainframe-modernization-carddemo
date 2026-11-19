# Sign-on and menus (CC00 / CM00 / CA00)

Programs: `COSGN00C` (map COSGN00), `COMEN01C` (COMEN01, options in
`COMEN02Y.cpy`), `COADM01C` (COADM01, options in `COADM02Y.cpy`).
Users: `ADMIN001` (type A) and `USER0001` (type U), password `PASSWORD`.

| ID | Steps | Expected |
|---|---|---|
| SGN-01 | CC00; User ID = USER0001, Password = PASSWORD; ENTER | XCTL to COMEN01C, main menu shown, COMMAREA user type U |
| SGN-02 | CC00; ADMIN001 / PASSWORD; ENTER | XCTL to COADM01C, admin menu shown |
| SGN-03 | User ID blank; ENTER | `Please enter User ID ...`, cursor on User ID |
| SGN-04 | USER0001, Password blank; ENTER | `Please enter Password ...` |
| SGN-05 | USER0001 / WRONGPWD; ENTER | `Wrong Password. Try again ...`, stays on sign-on |
| SGN-06 | NOUSER01 / PASSWORD; ENTER | `User not found. Try again ...` |
| SGN-07 | lower-case `user0001` / `password` | Accepted (input upper-cased) |
| SGN-08 | PF3 on sign-on | Thank-you message, transaction ends |
| MNU-01 | Signed on as USER0001; option = 1..11; ENTER | XCTL to the program listed for that option in COMEN02Y |
| MNU-02 | option = 99 or non-numeric | "Please enter a valid option number" style error, no XCTL |
| MNU-03 | option of an admin-only entry as USER0001 | Access-denied message, no XCTL |
| MNU-04 | PF3 | Return to sign-on (COSGN00C) |
| ADM-01 | Signed on as ADMIN001; option = 1..6; ENTER | XCTL to COUSR00C/01C/02C/03C/COTRTLIC/COTRTUPC per COADM02Y |
| ADM-02 | option = 7 (beyond count 6) | Invalid option message |
| ADM-03 | PF3 | Return to sign-on |
