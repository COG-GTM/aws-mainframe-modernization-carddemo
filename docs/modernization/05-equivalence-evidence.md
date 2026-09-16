# 5. Equivalence evidence — what was verified, how, and what was not

## 5.1 What "verified" means in this document

Three different levels of evidence exist in this pass. They are not interchangeable.

| Level | Meaning | Available here? |
|---|---|---|
| **A. Source syntax** | The three COBOL programs compile under GnuCOBOL 3.1.2 `-fsyntax-only -std=ibm`. | Yes — all three: 0 errors. Proves the source is what we read, nothing about runtime. |
| **B. Java behaviour vs documented COBOL behaviour** | JUnit tests assert that the Java produces the outputs that `02-module-*.md` says the COBOL produces, for the cases below. The oracle is our *reading* of the COBOL, not a COBOL execution. | Yes — 81 tests, all passing. |
| **C. Java output vs actual mainframe output** | Byte comparison of `TRANSACT`, `DALYREJS`, `SYSTRAN`, `TRANREPT`, `ACCTDATA`, `TCATBALF` after running both implementations on the same input. | **No.** There is no mainframe, CICS, VSAM or Enterprise COBOL runtime in this environment and the repository ships no output datasets. |

Every claim below is level B unless marked otherwise. **Level C equivalence is not claimed.**

## 5.2 Test matrix (level B)

Run: `export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 && cd modernization/posttran-cycle && mvn test`
Result at time of writing: `Tests run: 81, Failures: 0, Errors: 0, Skipped: 0` across 15 classes.

### Record layouts and encoding (`carddemo-recordio`)

| Case | Test | COBOL reference |
|---|---|---|
| Every shipped EBCDIC dataset decodes into exactly N records of the copybook LRECL and re-encodes byte-identically (DALYTRAN 300×350, ACCTDATA 50×300, CARDXREF 50×50, TCATBALF 50×50, DISCGRP 51×50, TRANTYPE 7×60, TRANCATG 18×60) | `ShippedDatasetRoundTripTest` | copybooks; `app/data/EBCDIC/*` |
| ASCII twins decode to the same field values as EBCDIC | `ShippedDatasetRoundTripTest.asciiCopyDecodesToTheSameValuesAsEbcdic` | `app/data/ASCII/*` |
| Zoned decimal: unsigned drops sign, `S9(n)V99` truncates low-order and high-order digits without rounding, non-numeric bytes rejected | `CobolNumericTest` | COBOL MOVE/COMPUTE rules |
| Malformed input: file length not a multiple of LRECL, wrong record length, non-numeric bytes in `TRAN-AMT`, duplicate key on `WRITE` (status 22) | `MalformedRecordTest` | file-status semantics |

### Posting (`CBTRN02C`)

| Case | Test | Lines |
|---|---|---|
| Unknown card → 100, account never read | `TransactionValidatorTest.unknownCardIsReason100AndAccountIsNeverConsulted` | 380-392 |
| Card present, account missing → 101 | `cardWithoutAccountIsReason101` | 393-399 |
| Exposure == limit posts; +0.01 → 102 | `amountExactlyAtCreditLimitPosts_oneCentOverIsReason102` | 403-412 |
| `ACCT-CURR-BAL` not part of the check | `currentBalanceIsNotPartOfTheCreditCheck` | 403-405 |
| Negative (debit) amount always passes the credit check | `negativeDebitAlwaysPassesTheCreditCheck` | 403-407 |
| `WS-TEMP-BAL` `S9(09)V99` narrower than operands | `wsTempBalIsNarrowerThanItsOperands_highOrderDigitIsLost` | 79, 403 |
| Origination on expiry date posts; day after → 103 | `transactionOnExpiryDatePosts_dayAfterIsReason103` | 414-420 |
| Both over-limit and expired → 103 (overwrite) | `expiryReasonOverwritesOverLimitReasonWhenBothFail` | 407-420 |
| `TRAN-PROC-TS` = `YYYY-MM-DD-HH.MM.SS.mmm000` | `TransactionPostingServiceTest.postedTransactionCarriesDb2StyleProcessingTimestamp` | 692-705 |
| Existing TCATBAL row incremented; missing row created with the amount | `existingCategoryBalanceIsIncremented`, `missingCategoryBalanceIsCreatedWithTheAmount` | 467-544 |
| `>= 0` → cycle credit; `< 0` → added to cycle debit *without negation* | `positiveAmountGoesToCycleCredit_negativeIsAddedToCycleDebitWithoutNegation` | 548-552 |
| Second transaction sees the first's cycle totals | `laterTransactionSeesEarlierOnesCycleTotals` | 403, 545-560 |
| Reject touches no master | `rejectDoesNotTouchAnyMaster` | 211-216 |
| Rewritten ACCTDATA record keeps its 300 bytes and untouched filler | `rewrittenAccountKeepsUntouchedBytesAndLength` | REWRITE semantics |
| Reject = 430 bytes = original 350 + `9(04)` code + `X(76)` text | `rejectRecordIs430BytesWithCodeAndDescriptionTrailer` | 128-139, 446-449 |
| Reject keeps input bytes the layout does not decode (the `X(20)` filler) | `rejectRecordKeepsUndecodedInputBytesSuchAsFiller` | 446-449 (group MOVE) |
| Amounts stored with truncation not rounding | `amountsAreStoredWithCobolTruncationNotRounding` | COMPUTE/ADD rules |
| End-to-end on shipped data: 300 in → N posted + M rejected, all outputs fixed-width, exit `COMPLETED_WITH_REJECTS` when M>0 | `PostingJobTest` | whole program |
| Unwritable output dataset → job `FAILED` (COBOL: CLOSE/WRITE error abends 999), never `COMPLETED` | `PostingJobOutputFailureTest` | 9000-*-CLOSE, 9999-ABEND |

### Interest (`CBACT04C`)

| Case | Test | Lines |
|---|---|---|
| `(bal × rate) / 1200`, truncated to 2 dp, negative balance, zero rate, high-order truncation into `S9(09)V99` | `InterestCalculatorTest` (5) | 464-467, 168 |
| Group rate wins; missing group → `DEFAULT`; blank group (as shipped) → `DEFAULT`; missing `DEFAULT` fatal | `InterestRateLookupTest` (4) | 415-460 |
| Control break adds total to `ACCT-CURR-BAL`, zeroes both cycle buckets | `InterestPostingRunTest.controlBreakAppliesTotalInterestAndResetsCycleBuckets` | 350-371 |
| One transaction per balance row; ID = PARM-DATE + 6-digit suffix; type 01 / cat 5 / `System` / `Int. for a/c ` + id / merchant 0 / first card of account | `oneSystemTransactionPerCategoryBalance…` | 473-516 |
| Non-zero rate with zero balance still writes a (zero) transaction | `zeroInterestStillWritesATransaction` | 214-216 |
| Unknown account fatal | `unknownAccountIsFatal` | 372-392 |
| Empty input → no output, no account touched (documented deviation, OQ-12) | `emptyInputProducesNoOutputAndTouchesNoAccount` | 221-222 |
| End-to-end on shipped data: 50 balance rows → 50 SYSTRAN records, accounts rewritten with reset cycles | `InterestJobTest` | whole program |

### Reporting (`TRANREPT` SORT + `CBTRN03C`)

| Case | Test | Lines |
|---|---|---|
| Every line 133 chars; detail line field positions match CVTRA07Y | `TransactionReportWriterTest.everyLineIs133CharactersAndTheDetailLineMatchesCvtra07y` | CVTRA07Y |
| EOF branch counts the last amount twice (page + grand) | `singleRecordRunCountsTheLastAmountTwiceInPageAndGrandTotal` | 198-206 |
| Account total on card change; none for the last card | `cardChangeWritesAccountTotalBeforeNextDetailButNeverForTheLastCard` | 181-188, 198-206 |
| Page break every 20 *lines* (headers and totals count) | `pageBreaksEveryTwentyLinesCountingHeadersAndTotals` | 282-285, 131 |
| Out-of-range record stops the run (default) or is skipped (policy) | `recordsOutsideDateParmRangeStopTheRunLikeTheCobolOrAreSkippedByPolicy` | 173-178 |
| Start and end dates inclusive | `boundaryDatesAreInclusive` | 173-174 |
| Missing card / type / category fatal | `unknownCardTypeOrCategoryIsFatal` | 484-513 |
| SORT step: inclusive date window on bytes 305-314, ascending card | `sortStepFiltersByProcessingDateAndOrdersByCardNumber` | TRANREPT.jcl 40-50 |
| `-ZZZ,ZZZ,ZZZ.ZZ` / `+ZZZ,ZZZ,ZZZ.ZZ` editing: floating sign, 15 chars, zero → spaces, truncation | `CobolEditedAmountTest` (5) | CVTRA07Y |
| End-to-end: synthesized TRANSACT from shipped DALYTRAN (proc-ts stamped in-window) → EBCDIC 133-byte report with 300 details, headers, grand total, and the intermediate sorted file | `ReportJobTest` | whole job |
| Empty DATEPARM → job completes with an empty TRANREPT (EOF on the single read sets END-OF-FILE) | `ReportJobEmptyDateParmTest` | 220-243 |

## 5.3 What is *not* proven, and what would prove it

| Gap | Why it cannot be closed here | Exactly what is needed |
|---|---|---|
| Byte-identical `TRANSACT`, `DALYREJS`, `ACCTDATA`, `TCATBALF` after POSTTRAN on the shipped DALYTRAN | No COBOL runtime; no expected-output files in the repo. Timestamps also differ by construction. | (1) A **Zowe CLI** profile to the customer's z/OS LPAR with read access to `AWS.M2.CARDDEMO.*`; (2) a **compile-and-run unit**: compile `CBTRN02C` with Enterprise COBOL and run `POSTTRAN.jcl` against a copy of the sample data; (3) `zowe files download` of the six output datasets in binary; (4) a comparison harness that masks `TRAN-PROC-TS`. |
| Same for INTCALC and TRANREPT | Same. | Same three items for `INTCALC.jcl` (with its `PARM`) and `TRANREPT.jcl`, plus the `DATEPARM` dataset content, which is not in the repo. |
| Enterprise COBOL semantics we inferred from the standard rather than observed: alphanumeric `'05'` → `9(04)`; `NEXT SENTENCE` target inside an in-line `PERFORM`; record-area contents after an EOF `READ`; edited-picture rendering of zero; numeric vs alphanumeric compare in `TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM` | Compiler/runtime behaviour, not in the source. GnuCOBOL syntax check does not execute. | A compile-and-run unit with small hand-built inputs for each case, or the customer's existing production outputs (`TRANREPT` GDG generations, `DALYREJS` generations) as **sample production output files** to compare against. |
| `RETURN-CODE 4` → process exit 4 | Tested at `JobExecution.exitStatus` level only. | Run the packaged jar from a shell and check `$?`. |
| Scheduler cycle | Repo shows INTCALC monthly and no TRANREPT entry. | The customer's live CA-7/Control-M definitions. |
| Performance / volume | Sample is 300 transactions. | Production-sized extracts. |

## 5.4 Lint / static analysis

The repository has no Java lint configuration (no Checkstyle, Spotless, PMD, or Error Prone in
any `pom.xml`, no `.editorconfig` for Java, no pre-commit hooks — `.pre-commit-config.yaml` and
`.husky/` do not exist). The equivalent of a type check is `mvn compile` with
`-Xlint:all` warnings visible in the build output; the build passes with `maven.compiler.release=21`.
Adding a linter is a project decision and was not made unilaterally.
