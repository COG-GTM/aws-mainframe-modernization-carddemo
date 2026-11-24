# GAP-REVIEW - AUTHFRDS: legacy COBOL vs spec vs Java

> **Provenance:** authored by **Devin**. Compares `app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl` (every paragraph: `MAIN-PARA`, `FRAUD-UPDATE`) and the COPAUS1C paragraphs that reach it (`MAIN-PARA`, `PROCESS-ENTER-KEY`, `MARK-AUTH-FRAUD`, `PROCESS-PF8-KEY`, `READ-AUTH-RECORD`, `READ-NEXT-AUTH-RECORD`, `UPDATE-AUTH-DETAILS`, `TAKE-SYNCPOINT`, `ROLL-BACK`, `SCHEDULE-PSB`) against `BUSINESS-RULES.md` and `modernization/authfrds-service`. "Tested" = a unit/MockMvc test and/or a real-Db2 IT (`AuthFrdsDb2IT`) asserts it.

## SQLCODE handling
| Legacy behaviour | Spec | Java + test | Verdict |
|---|---|---|---|
| INSERT 0 -> `ADD SUCCESS` | BR-AF-004 | `FraudRecordService.record`; unit + IT (real Db2) | captured |
| INSERT -803 -> FRAUD-UPDATE (not an error) | BR-AF-005 | unit + IT (real -803 asserted from JCC) | captured |
| FRAUD-UPDATE 0 -> `UPDT SUCCESS` | BR-AF-006 | unit + IT | captured |
| FRAUD-UPDATE != 0 (incl. +100) -> ` UPDT ERROR DB2: CODE:+000100, STATE: +000002000` | BR-AF-007 | unit (text), IT (real +100 from searched UPDATE) | captured. *Deviation:* JCC returns update count 0, not SQLCODE +100, so the adapter synthesises +100/02000 (what the SQLCA holds on z/OS). |
| INSERT other -> ` SYSTEM ERROR DB2: CODE:<+9(06)>, STATE: <+9(09)>` | BR-AF-008 | unit; IT with real SQLCODE -20448 / SQLSTATE 22007 (month 13) | captured |
| WS-SQLSTATE is PIC +9(09) - alphanumeric SQLSTATE moved into a numeric edited field | BR-AF-007/008 | `AuthFraudRules.sqlStateEdited` (`+` + zero-filled digits) | captured (unit asserts `+000022007`; IT asserts it from a real SQLSTATE). *Intentional deviation:* alphanumeric states (e.g. `2200G`) are an *intentional deviation*: Java keeps the characters; legacy behaviour for non-digits is compiler dependent. |
| WS-FRD-ACT-MSG X(50) truncation | BR-AF-009 | `actMsg`; unit | captured (` SYSTEM ERROR DB2: CODE:-000911, STATE: +000040001` is exactly 50 chars, asserted) |
| -532 FK restrict | - | - | not applicable: AUTHFRDS has no foreign keys and no DELETE |
| Db2 unreachable | - | `ApiExceptionHandler` -> 503 | *intentional deviation* (CICS would abend; no legacy message) |

## Paging / PF keys / cursors
| Legacy behaviour | Spec | Java + test | Verdict |
|---|---|---|---|
| COPAUS2C has no cursor; AUTHFRDS is write-only in legacy | BR-AF-030 | `GET /api/v1/fraud-records` paged (5) in XAUTHFRD order, filters card/acct/flag; MockMvc + IT | *intentional deviation* (added inspection read so writes can be verified) |
| PF5 toggle F<->R | BR-AF-020 | `AuthorizationFraudService.toggleFraud`; unit, MockMvc, IT | captured |
| PF8 next / `Already at the last Authorization...` | BR-AF-027 | `/next`; unit + MockMvc | captured (IMS stub) |
| ENTER requires `CDEMO-CPVD-PAU-SELECTED` not SPACES/LOW-VALUES | BR-AF-026 | `AuthKey.parse`; unit | captured |
| PF3 return to COPAUS0C | - | - | not applicable (navigation; no screen) |
| Other AID -> `Invalid key pressed. Please see below...` | - | - | not applicable (REST has no AID keys) |

## Validation
| Legacy behaviour | Spec | Java + test | Verdict |
|---|---|---|---|
| No validation in COPAUS2C; PIC sizes of commarea bound the data | BR-AF-011 | Bean Validation on `FraudRequest` / `AuthorizationDetail`; MockMvc (3 field errors) | *intentional deviation* (REST input is untrusted; CICS commarea was not) |
| `TIMESTAMP_FORMAT` rejects bad dates | BR-AF-008 | IT (real -20448) | captured |
| `YY` pivot: Db2 LUW resolves `99-...` to **2099** (observed: `TIMESTAMP_FORMAT('99-10-29 ...')` = `2099-10-29-00.12.52.555000`) | BR-AF-001 | same SQL in Java | **MISSED -> documented** (two-digit-year risk inherited from the legacy `YY` format; was not in the first spec draft, added to BR-AF-001). Not fixed on purpose - Java reproduces the legacy SQL. |
| MARK-AUTH-FRAUD does not check READ-AUTH-RECORD status before toggling | BR-AF-025 | Java stops with 404/500 if the read fails | *intentional deviation* (legacy would toggle a stale buffer) |

| No authentication in COPAUS1C/COPAUS2C (CICS signon + RACF upstream) | BR-AF-040 | `SecurityConfig` HTTP Basic, READER/MAINTAINER; `SecurityTest` (401/403/200), `ApiCredentialsTest` (fail-fast) | *intentional deviation*: REST has no CICS signon, so the service enforces it itself |
| IMS stub test hooks | BR-AF-041 | `ImsStubTestController` `@Profile("testing")`; `SecurityTest` 404 without the profile | *intentional deviation* (test-only, off by default) |

## Confirmation flows
| Legacy behaviour | Spec | Java + test | Verdict |
|---|---|---|---|
| No confirm step: PF5 acts immediately | BR-AF-020 | POST `/fraud` acts immediately | captured (no confirmation exists in legacy) |
| COPAUS2C failure -> show WS-FRD-ACT-MSG + SYNCPOINT ROLLBACK, no IMS REPL | BR-AF-023 | unit | captured |
| IMS REPL failure -> ROLLBACK (undoes the Db2 row) + ` System error while FRAUD Tagging, ROLLBACK||<status>` | BR-AF-024 | `@Transactional`; unit, MockMvc, IT proves the real Db2 INSERT is rolled back | captured |
| LINK EIBRESP != NORMAL -> ROLLBACK, no message | - | - | not applicable (in-process call, no CICS LINK) |
| SYNCPOINT after successful REPL | BR-AF-022 | Spring commit | captured |

## Batch record actions
| Legacy behaviour | Spec | Java | Verdict |
|---|---|---|---|
| None for AUTHFRDS (CBPAUP0C purges IMS only; no Db2 batch on AUTHFRDS) | - | - | not applicable |
| DELETE | - | no endpoint | not applicable: no legacy program deletes AUTHFRDS; tests clean their own rows with SQL |

## Extracts
| Legacy behaviour | Spec | Java | Verdict |
|---|---|---|---|
| No Db2 unload/extract of AUTHFRDS; `DISPLAY 'RPT DT: '` debug line in UPDATE-AUTH-DETAILS | - | - | not applicable (DISPLAY is a CICS log line, not an extract) |
| IMS unload `DBPAUTP0.dat` (LOADPADB.JCL loads IMS, not Db2) | DATA-MODEL | `tools/decode_ims_unload.py` -> IMS stub seed (22 roots / 202 details) | captured (IMS **stub**, not IMS) |

## COBOL literal diff
All 12 message literals of COPAUS2C/COPAUS1C in scope (`grep -oE "'[^']{3,}'"`) appear verbatim in `AuthFraudRules` (`ADD SUCCESS`, `UPDT SUCCESS`, ` SYSTEM ERROR DB2: CODE:`, ` UPDT ERROR DB2: CODE:`, `, STATE: `, `AUTH MARKED FRAUD...`, `AUTH FRAUD REMOVED...`, ` System error while FRAUD Tagging, ROLLBACK||`, ` System error while reading Auth Summary: Code:`, ` System error while reading Auth Details: Code:`, ` System error while reading next Auth: Code:`, `Already at the last Authorization...`), plus `YY-MM-DD HH24.MI.SSNNNNNN`. ` System error while scheduling PSB: Code:` is not implemented (no PSB in the stub) - not applicable.

## Spec corrections made during review
- BR-AF-002/011 named the commarea ids `WS-FRD-ACCT-ID/CUST-ID`; in COPAUS2C they are `WS-ACCT-ID`/`WS-CUST-ID` (COPAUS1C's names). **FIXED** in the spec.
