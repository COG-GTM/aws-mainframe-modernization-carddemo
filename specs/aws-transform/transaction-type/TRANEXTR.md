# Program overview - TRANEXTR (JCL, DSNTIAUL-style unload)

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

Unloads the Db2 tables to the 60-byte sequential files the VSAM-based CardDemo programs use.
- STEP40 to TRANTYPE.PS: `CAST(CONCAT(CONCAT(TR_TYPE, CAST(TR_DESCRIPTION AS CHAR(50))), REPEAT('0',8)) AS CHAR(60))` ORDER BY TR_TYPE (BR-TT-080).
- STEP50 to TRANCATG.PS: type || category || CHAR(50) description || '0000' ORDER BY type, category (BR-TT-081).
