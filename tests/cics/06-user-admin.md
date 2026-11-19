# User administration (CU00 - CU03, admin only)

Programs: `COUSR00C` list, `COUSR01C` add, `COUSR02C` update, `COUSR03C`
delete. File: USRSEC (layout `CSUSR01Y.cpy`, 80 bytes).

| ID | Steps | Expected |
|---|---|---|
| USL-01 | CU00 as ADMIN001 | First page of users |
| USL-02 | PF7/PF8 | Paging |
| USL-03 | `U` / `D` next to a user | XCTL to COUSR02C / COUSR03C with user id |
| USA-01 | CU01; First, Last, User ID TEST0001, Password, Type U; ENTER | Record added; "User TEST0001 has been added" style message |
| USA-02 | Any mandatory field blank | Field-specific "can NOT be empty" style error |
| USA-03 | Existing user id | "User ID already exist" style error |
| USA-04 | Type other than A/U | Validation error |
| USU-01 | CU02; fetch TEST0001; change last name; PF5 | Record updated |
| USU-02 | Fetch then PF5 without changes | "Please modify to update" style message |
| USU-03 | Unknown user id | User not found error |
| USD-01 | CU03; fetch TEST0001; PF5 | Record deleted; sign-on with TEST0001 now fails (SGN-06) |
| USD-02 | Unknown user id | User not found error |
| SEC-01 | Signed on as USER0001, start CU00 directly | Must not allow admin function (expected: redirected / denied) |
