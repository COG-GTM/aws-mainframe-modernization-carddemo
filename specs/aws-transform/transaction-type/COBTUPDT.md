# Program overview - COBTUPDT (batch, JCL MNTTRDB2)

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

## Purpose
Applies add, update and delete maintenance records to `CARDDEMO.TRANSACTION_TYPE` from a sequential file (`INPFILE`, RECFM F).

## Record layout (BR-TT-070)
`INPUT-REC-TYPE X(1)` + `INPUT-REC-NUMBER X(2)` + `INPUT-REC-DESC X(50)` = 53 bytes.

## Flow
1. Open the file and DISPLAY `OPEN FILE OK`, then read every record until EOF. Each record is echoed with `PROCESSING` (BR-TT-076).
2. EVALUATE action (BR-TT-071):
   - `A`: INSERT. Success gives `RECORD INSERTED SUCCESSFULLY`.
   - `U`: UPDATE description. Success gives `RECORD UPDATED SUCCESSFULLY`; +100 gives `No records found.` (BR-TT-073).
   - `D`: DELETE. Success gives `RECORD DELETED SUCCESSFULLY`; +100 gives `No records found.` (BR-TT-073).
   - `*`: comment, `IGNORING COMMENTED LINE`.
   - anything else: `ERROR: TYPE NOT VALID` (BR-TT-072).
3. A negative SQLCODE gives `Error accessing: TRANSACTION_TYPE table. SQLCODE:` + code (BR-TT-074).
4. `9999-ABEND` DISPLAYs the message and sets `RETURN-CODE = 4`. It does **not** stop the run: the loop goes on to the next record, and rc 4 stays at the end (BR-TT-075).

## Notes
- The batch program does no field validation: there are no numeric or alphanumeric edits on the code or description.
- The description host variable is X(50) moved into the VARCHAR, so the stored value keeps its trailing blanks.
