package com.carddemo.programs.copaus0c;

import com.carddemo.programs.copaus0c.copybook.CardDemoCommarea;
import com.carddemo.programs.copaus0c.copybook.Copau0aInput;
import com.carddemo.programs.copaus0c.copybook.Copau0aScreen;
import com.carddemo.programs.copaus0c.copybook.Dfh;
import com.carddemo.programs.copaus0c.copybook.Dfh.Aid;
import com.carddemo.programs.copaus0c.inmemory.InMemoryPendingAuthDb.Call;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Copaus0cServiceTest {

    private static final String ACCT = "00000000001";
    private TestData data;

    @BeforeEach
    void setUp() {
        data = new TestData();
    }

    private static String msg(String text) {
        return String.format("%-78.78s", text); // WS-MESSAGE X(80) -> ERRMSGO X(78)
    }

    private static String low(int n) {
        return "\0".repeat(n);
    }

    private Copaus0cResult enter(Copau0aInput input, CardDemoCommarea ca) {
        return data.service.execute(Aid.ENTER, ca, input);
    }

    /** Enter account 1 on the re-entered screen; returns page 1. */
    private Copaus0cResult firstPage() {
        CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
        return enter(Copau0aInput.of(ACCT), ca);
    }

    @Nested
    class MainPara {
        @Test
        void firstTimeWithoutCommareaSendsEmptyScreenAndReturnsCpvs() {
            Copaus0cResult r = data.service.execute(Aid.ENTER, null, null);
            assertEquals("CPVS", r.returnTransid());
            assertNull(r.xctlProgram());
            assertEquals(1, r.screens().size());
            Copau0aScreen s = r.screen();
            assertTrue(s.erase());
            assertEquals("ACCTID", s.cursorField());
            assertEquals(low(11), s.acctid());
            assertEquals(msg(""), s.errmsg());
            assertEquals("COPAUS0C", r.commarea().cdemoToProgram);
            assertTrue(r.commarea().isCdemoPgmReenter());
            assertEquals("      ", r.commarea().cdemoUserId.substring(0, 6));
        }

        @Test
        void populateHeaderInfoFormatsTitlesDateAndTime() {
            Copau0aScreen s = data.service.execute(Aid.ENTER, null, null).screen();
            assertEquals("CPVS", s.trnname());
            assertEquals("COPAUS0C", s.pgmname());
            assertEquals("11/26/26", s.curdate());
            assertEquals("10:15:30", s.curtime());
            assertEquals("      AWS Mainframe Modernization       ", s.title01());
            assertEquals("              CardDemo                  ", s.title02());
        }

        @Test
        void entryFromOtherProgramWithNumericAccountGathersDetails() {
            Copaus0cResult r = data.service.execute(Aid.ENTER, TestData.fromMenu(ACCT), null);
            assertEquals(1, r.screens().size());
            assertEquals(ACCT, r.screen().acctid());
            assertEquals("TXN000000000001 ", r.screen().row(1).trnid());
            assertTrue(r.commarea().isCdemoPgmReenter());
            assertTrue(r.screen().erase());
        }

        @Test
        void entryFromOtherProgramWithNonNumericAccountShowsBlankAccount() {
            Copaus0cResult r = data.service.execute(Aid.ENTER, TestData.fromMenu("ABC        "), null);
            assertEquals(" " + " ".repeat(10), r.screen().acctid());
            assertEquals(low(16), r.screen().row(1).trnid());
            assertTrue(data.ims.callLog().isEmpty());
        }

        @Test
        void invalidKeyShowsCommonMessage() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = data.service.execute(Aid.PF5, ca, Copau0aInput.of(ACCT));
            assertEquals(msg("Invalid key pressed. Please see below..."), r.screen().errmsg());
            assertEquals(ACCT, r.screen().acctid());
            assertEquals("CPVS", r.returnTransid());
        }

        @Test
        void pf3TransfersToMenu() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = data.service.execute(Aid.PF3, ca, Copau0aInput.none());
            assertEquals("COMEN01C", r.xctlProgram());
            assertNull(r.returnTransid());
            assertTrue(r.screens().isEmpty());
            assertEquals("CPVS", r.commarea().cdemoFromTranid);
            assertEquals("COPAUS0C", r.commarea().cdemoFromProgram);
            assertEquals(0, r.commarea().cdemoPgmContext);
        }

        @Test
        void callerCommareaIsNotMutated() {
            CardDemoCommarea ca = TestData.fromMenu(ACCT);
            data.service.execute(Aid.ENTER, ca, null);
            assertEquals(CardDemoCommarea.CDEMO_PGM_ENTER, ca.cdemoPgmContext);
        }
    }

    @Nested
    class ProcessEnterKey {
        @Test
        void blankAccountIdIsRejected() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = enter(Copau0aInput.of("           "), ca);
            assertEquals(msg("Please enter Acct Id..."), r.screen().errmsg());
            assertEquals(" ".repeat(11), r.screen().acctid());
            assertEquals("ACCTID", r.screen().cursorField());
        }

        @Test
        void missingAccountIdIsRejected() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = enter(Copau0aInput.none(), ca);
            assertEquals(msg("Please enter Acct Id..."), r.screen().errmsg());
        }

        @Test
        void nonNumericAccountIdIsRejected() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = enter(Copau0aInput.of("12345ABC"), ca);
            assertEquals(msg("Acct Id must be Numeric ..."), r.screen().errmsg());
            assertEquals(" ".repeat(11), r.screen().acctid());
            assertTrue(data.ims.callLog().isEmpty());
        }

        @Test
        void validAccountIsStoredInCommarea() {
            Copaus0cResult r = firstPage();
            assertEquals(ACCT, r.commarea().cdemoAcctId);
            assertEquals("000000007", r.commarea().cdemoCustId);
            assertEquals("4111111111111111", r.commarea().cdemoCardNum);
            assertEquals(msg(""), r.screen().errmsg());
        }

        @Test
        void selectionSTransfersToDetailProgramWithSelectedKey() {
            CardDemoCommarea ca = firstPage().commarea();
            Copaus0cResult r = enter(Copau0aInput.of(ACCT).withSelection(3, "S"), ca);
            assertEquals("COPAUS1C", r.xctlProgram());
            assertEquals("S", r.commarea().cdemoCpvsPauSelFlg);
            assertEquals(TestData.key(3), r.commarea().cdemoCpvsPauSelected);
            assertEquals("COPAUS1C", r.commarea().cdemoToProgram);
            assertEquals("CPVS", r.commarea().cdemoFromTranid);
            assertEquals("COPAUS0C", r.commarea().cdemoFromProgram);
            assertEquals(CardDemoCommarea.CDEMO_PGM_ENTER, r.commarea().cdemoPgmContext);
            assertTrue(r.screens().isEmpty());
        }

        @Test
        void lowerCaseSelectionIsAccepted() {
            CardDemoCommarea ca = firstPage().commarea();
            Copaus0cResult r = enter(Copau0aInput.of(ACCT).withSelection(1, "s"), ca);
            assertEquals("COPAUS1C", r.xctlProgram());
            assertEquals(TestData.key(1), r.commarea().cdemoCpvsPauSelected);
        }

        @Test
        void firstNonBlankSelectionWins() {
            CardDemoCommarea ca = firstPage().commarea();
            Copaus0cResult r = enter(Copau0aInput.of(ACCT).withSelection(2, "S").withSelection(4, "X"), ca);
            assertEquals(TestData.key(2), r.commarea().cdemoCpvsPauSelected);
        }

        @Test
        void invalidSelectionShowsMessageAndRefreshesList() {
            CardDemoCommarea ca = firstPage().commarea();
            Copaus0cResult r = enter(Copau0aInput.of(ACCT).withSelection(2, "X"), ca);
            assertNull(r.xctlProgram());
            assertEquals(msg("Invalid selection. Valid value is S"), r.screen().errmsg());
            assertEquals("X", r.commarea().cdemoCpvsPauSelFlg);
            assertEquals("TXN000000000001 ", r.screen().row(1).trnid());
        }

        @Test
        void noSelectionClearsSelectionFields() {
            CardDemoCommarea ca = firstPage().commarea();
            ca.cdemoCpvsPauSelFlg = "S";
            Copaus0cResult r = enter(Copau0aInput.of(ACCT), ca);
            assertEquals(" ", r.commarea().cdemoCpvsPauSelFlg);
            assertEquals("        ", r.commarea().cdemoCpvsPauSelected);
        }

        @Test
        void selectionOnEmptyRowIsIgnored() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            ca.initialize();
            ca.cdemoPgmContext = CardDemoCommarea.CDEMO_PGM_REENTER;
            Copaus0cResult r = enter(Copau0aInput.of(ACCT).withSelection(1, "S"), ca);
            assertNull(r.xctlProgram());
            assertEquals(msg(""), r.screen().errmsg());
        }
    }

    @Nested
    class GatherAccountDetails {
        @Test
        void formatsCustomerAndAccountSummary() {
            Copau0aScreen s = firstPage().screen();
            assertEquals("000000007", s.custid());
            assertEquals("Jane Q Doe" + low(15), s.cname());
            assertEquals("1 Main,Unit 5" + low(12), s.addr001());
            assertEquals("Springfield,IL,62701" + low(5), s.addr002());
            assertEquals("(217)555-0100", s.phone1());
            assertEquals("     2020.00", s.credlim());
            assertEquals("  1020.00", s.cashlim());
        }

        @Test
        void formatsAuthSummaryCountsAndAmounts() {
            Copau0aScreen s = firstPage().screen();
            assertEquals("004", s.apprcnt());
            assertEquals("003", s.declcnt());
            assertEquals("-     123.45", s.credbal());
            assertEquals("    12.50", s.cashbal());
            assertEquals("   456.78 ", s.appramt());
            assertEquals(" 34567.89 ", s.declamt()); // -zzzz9.99 truncates high-order digits
        }

        @Test
        void missingAuthSummaryZeroFillsTotalsAndShowsNoRows() {
            data.ims.force(Call.GU, "GE");
            Copaus0cResult r = firstPage();
            Copau0aScreen s = r.screen();
            assertEquals("000", s.apprcnt());
            assertEquals("000", s.declcnt());
            assertEquals("000000000000", s.credbal());
            assertEquals("000000000", s.cashbal());
            assertEquals("0000000000", s.appramt());
            assertEquals("0000000000", s.declamt());
            assertEquals(" ".repeat(16), s.row(1).trnid());
            assertEquals(Dfh.DFHBMPRO, s.row(1).selAttr());
            assertEquals(msg(""), s.errmsg());
        }

        @Test
        void xrefNotFoundCascadesThroughAcctAndCustReads() {
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = enter(Copau0aInput.of("00000000099"), ca);
            assertEquals(4, r.screens().size());
            assertEquals(msg("Account:00000000099 not found in XREF file. Resp:000000013 Reas:000000080"),
                    r.screens().get(0).errmsg());
            assertEquals(msg("Account:00000000000 not found in ACCT file. Resp:000000013 Reas:000000080"),
                    r.screens().get(1).errmsg());
            // STRING does not pad: the 73rd character of the previous (longer) message survives
            assertEquals(msg("Customer:000000000 not found in CUST file. Resp:000000013 Reas:0000000800"),
                    r.screens().get(2).errmsg());
            assertEquals(r.screens().get(2).errmsg(), r.screen().errmsg());
            assertEquals("00000000099", r.screen().acctid());
        }

        @Test
        void xrefSystemErrorSetsErrorFlagAndSkipsPaging() {
            data.xref.forceResponse(22, 1);
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            Copaus0cResult r = enter(Copau0aInput.of(ACCT), ca);
            assertTrue(r.screens().get(0).errmsg()
                    .equals(msg("Account:00000000001 System error while reading XREF file. Resp:000000022 Reas:000000001")));
            assertTrue(data.ims.callLog().stream().noneMatch(c -> c.startsWith("GNP")));
        }

        @Test
        void acctSystemErrorMessage() {
            data.accounts.forceResponse(12, 0);
            Copaus0cResult r = firstPage();
            assertEquals(msg("Account:00000000001 System error while reading ACCT file. Resp:000000012 Reas:000000000"),
                    r.screens().get(0).errmsg());
        }

        @Test
        void custSystemErrorMessage() {
            data.customers.forceResponse(19, 2);
            Copaus0cResult r = firstPage();
            assertEquals(msg("Customer:000000007 System error while reading CUST file. Resp:000000019 Reas:000000002"),
                    r.screens().get(0).errmsg());
        }

        @Test
        void custNotFoundMessage() {
            data.customers.forceResponse(13, 80);
            Copaus0cResult r = firstPage();
            assertEquals(msg("Customer:000000007 not found in CUST file. Resp:000000013 Reas:000000080"),
                    r.screens().get(0).errmsg());
            assertEquals("TXN000000000001 ", r.screen().row(1).trnid());
        }
    }

    @Nested
    class Ims {
        @Test
        void schedulePsbRetriesAfterTermWhenScheduledMoreThanOnce() {
            data.ims.markPsbScheduled();
            Copaus0cResult r = firstPage();
            assertEquals(msg(""), r.screen().errmsg());
            assertEquals("SCHD PSBPAUTB", data.ims.callLog().get(0));
            assertEquals("TERM", data.ims.callLog().get(1));
            assertEquals("SCHD PSBPAUTB", data.ims.callLog().get(2));
        }

        @Test
        void schedulePsbFailureMessage() {
            data.ims.force(Call.SCHD, "TE");
            Copaus0cResult r = firstPage();
            assertEquals(msg(" System error while scheduling PSB: Code:TE"), r.screens().get(0).errmsg());
        }

        @Test
        void authSummaryReadErrorMessage() {
            data.ims.force(Call.GU, "BA");
            Copaus0cResult r = firstPage();
            assertEquals(msg(" System error while reading AUTH Summary: Code:BA"), r.screens().get(0).errmsg());
            assertTrue(data.ims.callLog().stream().noneMatch(c -> c.startsWith("GNP")));
        }

        @Test
        void authDetailReadErrorMessage() {
            data.ims.force(Call.GNP, "GP");
            Copaus0cResult r = firstPage();
            assertEquals(msg(" System error while reading AUTH Details: Code:GP"), r.screen().errmsg());
            assertEquals(2, r.screens().size());
        }

        @Test
        void sendScreenSyncpointsScheduledPsb() {
            firstPage();
            assertEquals("SYNCPOINT", data.ims.callLog().get(data.ims.callLog().size() - 1));
            assertTrue(!data.ims.isPsbScheduled());
        }

        @Test
        void statusOkPredicates() {
            assertTrue(ImsStatus.of("  ").isStatusOk());
            assertTrue(ImsStatus.of("FW").isStatusOk());
            assertEquals(ImsStatus.END_OF_DB, ImsStatus.of("GB"));
            assertEquals(ImsStatus.OTHER, ImsStatus.of("AI"));
            assertTrue(ImsStatus.isRetryCondition("FH"));
        }
    }

    @Nested
    class Paging {
        @Test
        void processPageForwardFillsFiveRowsAndSetsNextPage() {
            Copaus0cResult r = firstPage();
            Copau0aScreen s = r.screen();
            for (int i = 1; i <= 5; i++) {
                assertEquals("TXN00000000000" + i + " ", s.row(i).trnid());
                assertEquals(Dfh.DFHBMUNP, s.row(i).selAttr());
                assertEquals(TestData.key(i), r.commarea().authKey(i));
            }
            assertEquals(1, r.commarea().cdemoCpvsPageNum);
            assertEquals(TestData.key(1), r.commarea().paukeyPrevPg(1));
            assertEquals(TestData.key(5), r.commarea().cdemoCpvsPaukeyLast);
            assertTrue(r.commarea().isNextPageYes());
        }

        @Test
        void populateAuthListFormatsDateTimeStatusAndAmount() {
            Copau0aScreen s = firstPage().screen();
            assertEquals("11/21/26", s.row(1).pdate());
            assertEquals("10:15:11", s.row(1).ptime());
            assertEquals("SALE", s.row(1).ptype());
            assertEquals("A", s.row(1).paprv());
            assertEquals("D", s.row(2).paprv());
            assertEquals("P", s.row(1).pstat());
            assertEquals("       10.25", s.row(1).pamt());
            assertEquals("       20.25", s.row(2).pamt());
        }

        @Test
        void pf8ShowsNextPageAndClearsUnusedRows() {
            CardDemoCommarea ca = firstPage().commarea();
            Copaus0cResult r = data.service.execute(Aid.PF8, ca, Copau0aInput.of(ACCT));
            Copau0aScreen s = r.screen();
            assertTrue(!s.erase());
            assertEquals("TXN000000000006 ", s.row(1).trnid());
            assertEquals("TXN000000000007 ", s.row(2).trnid());
            assertEquals(" ".repeat(16), s.row(3).trnid());
            assertEquals(Dfh.DFHBMPRO, s.row(3).selAttr());
            assertEquals(2, r.commarea().cdemoCpvsPageNum);
            assertEquals(TestData.key(6), r.commarea().paukeyPrevPg(2));
            // COBOL quirk: hitting EOF inside the 5-row loop skips the look-ahead, so NEXT-PAGE stays 'Y'
            assertTrue(r.commarea().isNextPageYes());
        }

        @Test
        void pf8PastLastRowShowsEmptyPageWithoutMessage() {
            CardDemoCommarea ca = firstPage().commarea();
            ca = data.service.execute(Aid.PF8, ca, Copau0aInput.of(ACCT)).commarea();
            Copaus0cResult r = data.service.execute(Aid.PF8, ca, Copau0aInput.of(ACCT));
            assertEquals(msg(""), r.screen().errmsg());
            assertEquals(" ".repeat(16), r.screen().row(1).trnid());
            assertEquals(2, r.commarea().cdemoCpvsPageNum);
            assertEquals(low(8), r.commarea().cdemoCpvsPaukeyLast);
        }

        @Test
        void pf8WhenNoNextPageShowsBottomMessage() {
            CardDemoCommarea ca = exactlyFiveAuthorizations().commarea();
            Copaus0cResult r = data.service.execute(Aid.PF8, ca, Copau0aInput.of("00000000002"));
            assertEquals(msg("You are already at the bottom of the page..."), r.screen().errmsg());
            assertTrue(!r.screen().erase());
        }

        @Test
        void pf7ReturnsToPreviousPage() {
            CardDemoCommarea ca = firstPage().commarea();
            ca = data.service.execute(Aid.PF8, ca, Copau0aInput.of(ACCT)).commarea();
            Copaus0cResult r = data.service.execute(Aid.PF7, ca, Copau0aInput.of(ACCT));
            for (int i = 1; i <= 5; i++) {
                assertEquals("TXN00000000000" + i + " ", r.screen().row(i).trnid());
            }
            // COBOL quirk: PF7 decrements the page number, then PROCESS-PAGE-FORWARD increments it again
            assertEquals(2, r.commarea().cdemoCpvsPageNum);
            assertTrue(r.commarea().isNextPageYes());
            assertTrue(data.ims.callLog().contains("GNP PAUTDTL1 WHERE PAUT9CTS"));
        }

        @Test
        void pf7OnFirstPageShowsTopMessage() {
            CardDemoCommarea ca = firstPage().commarea();
            Copaus0cResult r = data.service.execute(Aid.PF7, ca, Copau0aInput.of(ACCT));
            assertEquals(msg("You are already at the top of the page..."), r.screen().errmsg());
            assertTrue(!r.screen().erase());
        }

        @Test
        void repositionErrorMessageOnPf8() {
            CardDemoCommarea ca = firstPage().commarea();
            data.ims.force(Call.GNP_WHERE, "BA");
            Copaus0cResult r = data.service.execute(Aid.PF8, ca, Copau0aInput.of(ACCT));
            assertEquals(msg(" System error while repos. AUTH Details: Code:BA"), r.screens().get(0).errmsg());
        }

        @Test
        void exactlyFiveAuthorizationsHasNoNextPage() {
            Copaus0cResult r = exactlyFiveAuthorizations();
            assertTrue(!r.commarea().isNextPageYes());
            assertEquals("TXN000000000005 ", r.screen().row(5).trnid());
        }

        private Copaus0cResult exactlyFiveAuthorizations() {
            data.ims.addSummary(new com.carddemo.programs.copaus0c.copybook.PendingAuthSummary(2, 7, "A",
                    java.util.List.of("  ", "  ", "  ", "  ", "  "), java.math.BigDecimal.ZERO,
                    java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, 0, 0,
                    java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO));
            for (int i = 1; i <= 5; i++) {
                data.ims.addDetail(2, TestData.detail(i, "00", java.math.BigDecimal.ONE));
            }
            data.xref.add(new com.carddemo.programs.copaus0c.copybook.CardXrefRecord("4222222222222222", 7, 2));
            CardDemoCommarea ca = data.service.execute(Aid.ENTER, null, null).commarea();
            return enter(Copau0aInput.of("00000000002"), ca);
        }
    }
}
