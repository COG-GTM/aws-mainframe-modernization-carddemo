package com.carddemo.programs.cbpaup0c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;
import com.carddemo.programs.cbpaup0c.ims.DliCall;
import com.carddemo.programs.cbpaup0c.ims.InMemoryPautDb;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class Cbpaup0cTest {

    /** 2023-11-20 = YYDDD 23324. */
    static final LocalDate TODAY = LocalDate.of(2023, 11, 20);

    final InMemoryPautDb db = new InMemoryPautDb(CobolCodec.EBCDIC);
    final List<String> out = new ArrayList<>();

    static int date9c(int yyddd) {
        return 99999 - yyddd;
    }

    static PendingAuthSummary summary(long acct, int approvedCnt, String approvedAmt, int declinedCnt, String declinedAmt) {
        return new PendingAuthSummary().accountId(acct).customerId("000000001")
                .approvedAuthCount(approvedCnt).approvedAuthAmount(new BigDecimal(approvedAmt))
                .declinedAuthCount(declinedCnt).declinedAuthAmount(new BigDecimal(declinedAmt));
    }

    static PendingAuthDetails detail(int yyddd, String resp, String txnAmt, String apprAmt) {
        return PendingAuthDetails.of(date9c(yyddd), 999999999L - yyddd, resp, new BigDecimal(txnAmt), new BigDecimal(apprAmt));
    }

    Cbpaup0c program(String sysin, LocalDate today) {
        Clock clock = Clock.fixed(today.atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC);
        return new Cbpaup0c(db, sysin, clock, out::add);
    }

    int run(String sysin) {
        return program(sysin, TODAY).run();
    }

    List<String> totals() {
        int i = out.indexOf("# TOTAL SUMMARY READ  :" + out.stream()
                .filter(l -> l.startsWith("# TOTAL SUMMARY READ  :")).findFirst().orElse("").substring(23));
        return i < 0 ? List.of() : out.subList(i, i + 4);
    }

    @Nested
    class Initialize1000 {

        @Test
        void acceptsDateAndDayAndEchoesParm() {
            Cbpaup0c p = program("15,00002,00003,N", TODAY);
            assertEquals(0, p.run());
            assertEquals("231120", p.currentDate());
            assertEquals(23324, p.currentYyddd());
            assertEquals(List.of("STARTING PROGRAM CBPAUP0C::", "*-------------------------------------*",
                    "CBPAUP0C PARM RECEIVED :15,00002,00003,N ", "TODAYS DATE            :23324", " "),
                    out.subList(0, 5));
        }

        @Test
        void dayOfYearIsZeroPaddedToThreeDigits() {
            Cbpaup0c p = program("", LocalDate.of(2024, 1, 2));
            p.run();
            assertEquals(24002, p.currentYyddd());
            assertTrue(out.contains("TODAYS DATE            :24002"));
        }

        @Test
        void numericExpiryDaysIsUsed() {
            Cbpaup0c p = program("00,00001,00001,Y", TODAY);
            p.run();
            assertEquals(0, p.expiryDays());
        }

        @Test
        void nonNumericExpiryDaysDefaultsToFive() {
            for (String card : new String[] {"XX,00001,00001,N", "1 ,00001,00001,N", "  ", ""}) {
                Cbpaup0c p = program(card, TODAY);
                p.run();
                assertEquals(5, p.expiryDays(), card);
            }
        }

        @Test
        void checkpointFrequencyDefaultsForSpacesZeroOrLowValues() {
            for (String freq : new String[] {"     ", "0    ", "\u0000\u0000\u0000\u0000\u0000"}) {
                Cbpaup0c p = program("05," + freq + "," + freq + ",N", TODAY);
                p.run();
                assertEquals("5    ", p.prmInfo().chkpFreq(), "freq [" + freq + "]");
                assertEquals("10   ", p.prmInfo().chkpDisFreq(), "dis freq [" + freq + "]");
            }
        }

        @Test
        void zeroFilledFrequencyIsNotTheLiteralZeroAndIsKept() {
            Cbpaup0c p = program("05,00000,00000,N", TODAY);
            p.run();
            assertEquals("00000", p.prmInfo().chkpFreq());
            assertEquals("00000", p.prmInfo().chkpDisFreq());
        }

        @Test
        void debugFlagOtherThanYBecomesN() {
            Cbpaup0c p = program("05,00001,00001,x", TODAY);
            p.run();
            assertEquals("N", p.prmInfo().debugFlag());
            assertTrue(p.prmInfo().isDebugOff());
            Cbpaup0c q = program("05,00001,00001,Y", TODAY);
            q.run();
            assertTrue(q.prmInfo().isDebugOn());
        }

        @Test
        void sysinLongerThanPrmInfoIsTruncatedTo17Bytes() {
            program("05,00001,00001,N,EXTRA-STUFF", TODAY).run();
            assertEquals("CBPAUP0C PARM RECEIVED :05,00001,00001,N,", out.get(2));
        }

        @Test
        void missingSysinIsTreatedAsSpaces() {
            assertEquals(0, program(null, TODAY).run());
            assertEquals("CBPAUP0C PARM RECEIVED :" + " ".repeat(17), out.get(2));
        }
    }

    @Nested
    class MainPara {

        @Test
        void emptyDatabaseTakesFinalCheckpointAndDisplaysZeroTotals() {
            assertEquals(0, run("05,00001,00001,N"));
            assertEquals(List.of("RMAD0000"), db.checkpointIds());
            assertEquals(List.of(" ", "*-------------------------------------*",
                    "# TOTAL SUMMARY READ  :+00000000", "# SUMMARY REC DELETED :+00000000",
                    "# TOTAL DETAILS READ  :+00000000", "# DETAILS REC DELETED :+00000000",
                    "*-------------------------------------*", " "), out.subList(5, 13));
            assertEquals(13, out.size());
        }

        @Test
        void processesEveryRootAndItsChildren() {
            db.addSummary(summary(1, 2, "30.00", 0, "0.00"))
                    .addDetail(detail(23300, "00", "10.00", "10.00"))
                    .addDetail(detail(23320, "00", "20.00", "20.00"));
            db.addSummary(summary(2, 1, "5.00", 0, "0.00"))
                    .addDetail(detail(23310, "00", "5.00", "5.00"));

            assertEquals(0, run("05,00001,00001,N"));

            assertEquals(List.of("# TOTAL SUMMARY READ  :+00000002", "# SUMMARY REC DELETED :+00000001",
                    "# TOTAL DETAILS READ  :+00000003", "# DETAILS REC DELETED :+00000002"), totals());
            assertEquals(1, db.remainingSummaries().size());
            assertEquals(1, db.remainingSummaries().get(0).accountId());
            assertEquals(1, db.remainingDetails(1).size());
            assertEquals(date9c(23320), db.remainingDetails(1).get(0).authDate9c());
        }

        @Test
        void summaryIsDeletedWhenApprovedCountAlreadyZeroEvenWithUnexpiredChildren() {
            // The COBOL tests PA-APPROVED-AUTH-CNT twice; declined counts and unexpired children are ignored
            // and DLET of the root cascades to its dependents.
            db.addSummary(summary(9, 0, "0.00", 1, "7.00"))
                    .addDetail(detail(23324, "05", "7.00", "0.00"));

            run("05,00001,00001,N");

            assertTrue(db.remainingSummaries().isEmpty());
            assertEquals(List.of("# TOTAL SUMMARY READ  :+00000001", "# SUMMARY REC DELETED :+00000001",
                    "# TOTAL DETAILS READ  :+00000001", "# DETAILS REC DELETED :+00000000"), totals());
        }

        @Test
        void summaryWithPositiveApprovedCountIsKeptEvenIfAllDetailsDeleted() {
            db.addSummary(summary(3, 5, "50.00", 0, "0.00"))
                    .addDetail(detail(23300, "00", "10.00", "10.00"));

            run("05,00001,00001,N");

            assertEquals(1, db.remainingSummaries().size());
            assertTrue(db.remainingDetails(3).isEmpty());
        }

        @Test
        void inLoopCheckpointUsesAlphanumericComparison() {
            // P-CHKP-FREQ '5    ' vs WS-AUTH-SMRY-PROC-CNT '0000000n': '0' < '5', so never greater.
            for (int i = 1; i <= 12; i++) {
                db.addSummary(summary(i, 1, "1.00", 0, "0.00"));
            }
            run("05,     ,     ,N");
            assertEquals(1, db.checkpointIds().size());
        }

        @Test
        void inLoopCheckpointFiresEverySummaryWhenFrequencyIsZeroFilled() {
            for (int i = 1; i <= 3; i++) {
                db.addSummary(summary(i, 1, "1.00", 0, "0.00"));
            }
            run("05,00000,00000,N");
            assertEquals(4, db.checkpointIds().size());
            assertTrue(db.checkpointIds().stream().allMatch("RMAD0000"::equals));
            assertEquals(List.of("CHKP SUCCESS: AUTH COUNT - +00000001, APP ID - 00000000001",
                    "CHKP SUCCESS: AUTH COUNT - +00000002, APP ID - 00000000002",
                    "CHKP SUCCESS: AUTH COUNT - +00000003, APP ID - 00000000003",
                    "CHKP SUCCESS: AUTH COUNT - +00000003, APP ID - 00000000003"),
                    out.stream().filter(l -> l.startsWith("CHKP")).toList());
        }

        @Test
        void debugTraceLines() {
            db.addSummary(summary(48, 1, "1.00", 0, "0.00")).addDetail(detail(23300, "00", "1.00", "1.00"));
            run("05,00001,00001,Y");
            assertEquals(List.of("DEBUG: AUTH SMRY READ : +00000000", "DEBUG: AUTH DTL READ : +00000000",
                    "DEBUG: AUTH DTL DLET : +00000000048", "DEBUG: AUTH DTL READ : +00000001",
                    "DEBUG: AUTH SMRY DLET : +00000000048", "DEBUG: AUTH SMRY READ : +00000001"),
                    out.stream().filter(l -> l.startsWith("DEBUG")).toList());
        }
    }

    @Nested
    class CheckIfExpired4000 {

        @Test
        void dayDiffEqualToExpiryQualifies() {
            db.addSummary(summary(1, 2, "2.00", 0, "0.00"))
                    .addDetail(detail(23319, "00", "1.00", "1.00"))   // diff 5
                    .addDetail(detail(23320, "00", "1.00", "1.00"));  // diff 4
            run("05,00001,00001,N");
            assertEquals(1, db.remainingDetails(1).size());
            assertEquals(date9c(23320), db.remainingDetails(1).get(0).authDate9c());
        }

        @Test
        void futureDatedAuthorizationHasNegativeDiffAndIsKept() {
            db.addSummary(summary(1, 1, "1.00", 0, "0.00")).addDetail(detail(24096, "00", "1.00", "1.00"));
            run("00,00001,00001,N");
            assertEquals(1, db.remainingDetails(1).size());
        }

        @Test
        void yyDddArithmeticAcrossYearEndOvercountsDays() {
            // 2024-01-01 (24001) minus 2023-12-31 (23365) = 636, not 1.
            db.addSummary(summary(1, 1, "1.00", 0, "0.00")).addDetail(detail(23365, "00", "1.00", "1.00"));
            program("99,00001,00001,N", LocalDate.of(2024, 1, 1)).run();
            assertTrue(db.remainingDetails(1).isEmpty());
        }

        @Test
        void dayDiffIsTruncatedToFourDigits() {
            // CURRENT-YYDDD 26330 - WS-AUTH-DATE 12000 = 14330 -> S9(4) keeps 4330 (TRUNC(STD)).
            // 99 days expiry: 4330 >= 99 qualifies. Contrast: 26330 - 16331 = 9999 qualifies too; use 99999-auth
            // date of 0 -> WS-AUTH-DATE 99999 -> diff -73669 -> -3669 which does not qualify.
            db.addSummary(summary(1, 2, "2.00", 0, "0.00"))
                    .addDetail(detail(12000, "00", "1.00", "1.00"))
                    .addDetail(PendingAuthDetails.of(0, 1, "00", BigDecimal.ONE, BigDecimal.ONE));
            program("99,00001,00001,N", LocalDate.of(2026, 11, 26)).run();
            assertEquals(1, db.remainingDetails(1).size());
            assertEquals(0, db.remainingDetails(1).get(0).authDate9c());
        }

        @Test
        void negativeAuthDate9cMakesUnsignedAuthDateTruncateToZero() {
            // 99999 - (-1) = 100000 -> PIC 9(5) keeps 00000 -> diff = CURRENT-YYDDD -> qualifies.
            db.addSummary(summary(1, 1, "1.00", 0, "0.00"))
                    .addDetail(PendingAuthDetails.of(-1, 1, "00", BigDecimal.ONE, BigDecimal.ONE));
            run("99,00001,00001,N");
            assertTrue(db.remainingDetails(1).isEmpty());
        }

        @Test
        void approvedDetailAdjustsApprovedCountAndAmountDeclinedAdjustsDeclined() {
            // Approved: count 2 -> 1 (kept). Declined detail with resp '05' decrements declined count only.
            db.addSummary(summary(1, 2, "30.00", 1, "4.00"))
                    .addDetail(detail(23300, "00", "99.00", "10.00"))
                    .addDetail(detail(23300, "05", "4.00", "0.00"));
            run("05,00001,00001,N");
            assertEquals(1, db.remainingSummaries().size(), "approved count 1 > 0 keeps summary");
            // The adjusted counters live only in working storage (no REPL): the stored root is unchanged.
            assertEquals(2, db.remainingSummaries().get(0).approvedAuthCount());
        }

        @Test
        void approvedCountReachingZeroDeletesSummary() {
            db.addSummary(summary(1, 1, "10.00", 0, "0.00")).addDetail(detail(23300, "00", "10.00", "10.00"));
            run("05,00001,00001,N");
            assertTrue(db.remainingSummaries().isEmpty());
        }

        @Test
        void declinedExpiryDoesNotLowerApprovedCount() {
            db.addSummary(summary(1, 1, "10.00", 1, "3.00")).addDetail(detail(23300, "51", "3.00", "0.00"));
            run("05,00001,00001,N");
            assertEquals(1, db.remainingSummaries().size());
            assertTrue(db.remainingDetails(1).isEmpty());
        }
    }

    @Nested
    class AbendPaths {

        @Test
        void summaryReadFailureAbendsWithReturnCode16() {
            db.addSummary(summary(1, 1, "1.00", 0, "0.00"));
            db.injectStatus(DliCall.GN_PAUTSUM0, 2, "AI");
            assertEquals(16, run("05,00001,00001,N"));
            assertEquals(List.of("AUTH SUMMARY READ FAILED  :AI", "SUMMARY READ BEFORE ABEND :+00000001",
                    "CBPAUP0C ABENDING ..."), out.subList(out.size() - 3, out.size()));
            assertTrue(db.checkpointIds().isEmpty(), "GOBACK skips the final checkpoint");
            assertFalse(out.stream().anyMatch(l -> l.startsWith("# TOTAL")));
        }

        @Test
        void detailReadFailureAbends() {
            db.addSummary(summary(7, 1, "1.00", 0, "0.00")).addDetail(detail(23300, "00", "1.00", "1.00"));
            db.injectStatus(DliCall.GNP_PAUTDTL1, 2, "GP");
            assertEquals(16, run("05,00001,00001,N"));
            assertEquals(List.of("AUTH DETAIL READ FAILED  :GP", "SUMMARY AUTH APP ID      :+00000000007",
                    "DETAIL READ BEFORE ABEND :+00000001", "CBPAUP0C ABENDING ..."),
                    out.subList(out.size() - 4, out.size()));
        }

        @Test
        void detailEndOfDatabaseStatusEndsChildLoop() {
            db.addSummary(summary(7, 1, "1.00", 0, "0.00"));
            db.injectStatus(DliCall.GNP_PAUTDTL1, 1, "GB");
            assertEquals(0, run("05,00001,00001,N"));
        }

        @Test
        void detailDeleteFailureAbends() {
            db.addSummary(summary(7, 1, "1.00", 0, "0.00")).addDetail(detail(23300, "00", "1.00", "1.00"));
            db.injectStatus(DliCall.DLET_PAUTDTL1, 1, "DJ");
            assertEquals(16, run("05,00001,00001,N"));
            assertEquals(List.of("AUTH DETAIL DELETE FAILED :DJ", "AUTH APP ID               :+00000000007",
                    "CBPAUP0C ABENDING ..."), out.subList(out.size() - 3, out.size()));
        }

        @Test
        void summaryDeleteFailureAbends() {
            db.addSummary(summary(8, 0, "0.00", 0, "0.00"));
            db.injectStatus(DliCall.DLET_PAUTSUM0, 1, "DX");
            assertEquals(16, run("05,00001,00001,N"));
            assertEquals(List.of("AUTH SUMMARY DELETE FAILED :DX", "AUTH APP ID                :+00000000008",
                    "CBPAUP0C ABENDING ..."), out.subList(out.size() - 3, out.size()));
        }

        @Test
        void checkpointFailureAbends() {
            db.addSummary(summary(12, 1, "1.00", 0, "0.00"));
            db.injectStatus(DliCall.CHKP, 1, "XX");
            assertEquals(16, run("05,00001,00001,N"));
            assertEquals(List.of("CHKP FAILED: DIBSTAT - XX, REC COUNT - +00000001, APP ID - 00000000012",
                    "CBPAUP0C ABENDING ..."), out.subList(out.size() - 2, out.size()));
        }

        @Test
        void fwIsNotTreatedAsSuccessByTheProgram() {
            // STATUS-OK includes 'FW' but the program tests DIBSTAT = '  ' directly.
            db.addSummary(summary(1, 1, "1.00", 0, "0.00"));
            db.injectStatus(DliCall.GN_PAUTSUM0, 1, "FW");
            assertEquals(16, run("05,00001,00001,N"));
            assertTrue(ImsReturnCode.STATUS_OK.matches("FW"));
        }
    }
}
