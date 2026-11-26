package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.AuthViewResult.ReturnTransid;
import com.carddemo.programs.copaus1c.AuthViewResult.Xctl;
import com.carddemo.programs.copaus1c.copybook.AidKey;
import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.BmsColor;
import com.carddemo.programs.copaus1c.copybook.Cobol;
import com.carddemo.programs.copaus1c.copybook.Copau1aMap.Field;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.gateway.SyncpointGateway;
import com.carddemo.programs.copaus1c.inmemory.InMemoryPendingAuthDb;
import com.carddemo.programs.copaus1c.inmemory.InMemoryPendingAuthDb.Op;
import com.carddemo.programs.copaus1c.inmemory.RecordingFraudLink;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static com.carddemo.programs.copaus1c.Fixtures.ACCT;
import static com.carddemo.programs.copaus1c.Fixtures.KEY1;
import static com.carddemo.programs.copaus1c.Fixtures.KEY2;
import static com.carddemo.programs.copaus1c.Fixtures.KEY3;
import static com.carddemo.programs.copaus1c.Fixtures.req;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Copaus1cServiceTest {

    private InMemoryPendingAuthDb db;
    private RecordingFraudLink link;
    private Copaus1cService service;

    @BeforeEach
    void setUp() {
        db = Fixtures.db();
        link = RecordingFraudLink.succeeding("03/04/26");
        service = new Copaus1cService(db, db, link, Fixtures.CLOCK);
    }

    private static String msg(String text) {
        return Cobol.x(text, 78);
    }

    private static String low(int n) {
        return Cobol.lowValues(n);
    }

    @Nested
    class MainPara {

        @Test
        void noCommareaXctlsToSummaryWithInitializedCommarea() {
            AuthViewResult r = service.execute(AuthViewRequest.noCommarea());
            assertEquals(new Xctl("COPAUS0C"), r.control());
            assertTrue(r.screens().isEmpty());
            assertEquals("CPVD", r.commarea().getFromTranid());
            assertEquals("COPAUS1C", r.commarea().getFromProgram());
            assertEquals("COPAUS0C", r.commarea().getToProgram());
            assertTrue(r.commarea().isPgmEnter());
            assertEquals(Cobol.spaces(8), r.commarea().getUserId());
            assertTrue(db.calls().isEmpty());
        }

        @Test
        void firstEntrySetsReenterShowsDetailAndReturnsTransid() {
            AuthViewRequest in = new AuthViewRequest(Fixtures.commarea(KEY1, false), AidKey.DFHENTER);
            AuthViewResult r = service.execute(in);
            assertEquals(new ReturnTransid("CPVD"), r.control());
            assertTrue(r.commarea().isPgmReenter());
            assertEquals(1, r.screens().size());
            AuthViewScreen s = r.lastScreen();
            assertEquals("4111222233334444", s.cardnum());
            assertTrue(s.erase());
            assertEquals(Field.CARDNUM, s.cursorField());
            assertEquals(msg(""), s.errmsg());
            assertEquals(1, db.syncpointCount());
            assertFalse(db.isPsbScheduled());
        }

        @Test
        void fraudDataInCommareaIsBlankedOnEveryTask() {
            Copaus1cCommarea c = Fixtures.commarea(KEY1, true);
            c.cpvd().setFraudData("STALE");
            AuthViewResult r = service.execute(new AuthViewRequest(c, AidKey.DFHENTER));
            assertEquals(Cobol.spaces(100), r.commarea().cpvd().getFraudData());
        }

        @Test
        void inputCommareaIsNotMutated() {
            Copaus1cCommarea c = Fixtures.commarea(KEY1, false);
            service.execute(new AuthViewRequest(c, AidKey.DFHENTER));
            assertTrue(c.isPgmEnter());
        }

        @Test
        void pf3XctlsBackToSummaryProgram() {
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF3));
            assertEquals(new Xctl("COPAUS0C"), r.control());
            assertTrue(r.screens().isEmpty());
            assertEquals("COPAUS0C", r.commarea().getToProgram());
            assertEquals("CPVD", r.commarea().getFromTranid());
            assertEquals("COPAUS1C", r.commarea().getFromProgram());
            assertTrue(r.commarea().isPgmEnter());
            assertEquals(KEY1, r.commarea().cpvd().getPauSelected());
        }

        @Test
        void invalidKeyRedisplaysDetailWithInvalidKeyMessage() {
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF7));
            AuthViewScreen s = r.lastScreen();
            assertEquals(msg("Invalid key pressed. Please see below..."), s.errmsg());
            assertEquals("4111222233334444", s.cardnum());
            assertEquals(new ReturnTransid("CPVD"), r.control());
        }

        @Test
        void enterOnReenterRedisplaysSelectedAuth() {
            AuthViewResult r = service.execute(req(KEY2, AidKey.DFHENTER));
            assertEquals("D", r.lastScreen().authrsp());
            assertEquals(KEY2, r.commarea().cpvd().getPauSelected());
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void nonNumericAccountSetsErrorAndSkipsImsAndDetails() {
            Copaus1cCommarea c = Fixtures.commarea(KEY1, true);
            c.setAcctId("ABC        ");
            AuthViewResult r = service.execute(new AuthViewRequest(c, AidKey.DFHENTER));
            AuthViewScreen s = r.lastScreen();
            assertEquals(low(16), s.cardnum());
            assertEquals(low(10), s.authdt());
            assertEquals(msg(""), s.errmsg());
            assertTrue(db.calls().isEmpty());
        }

        @Test
        void spacesSelectedKeySetsError() {
            AuthViewResult r = service.execute(req(AuthorizationKey.SPACES, AidKey.DFHENTER));
            assertEquals(low(16), r.lastScreen().cardnum());
            assertTrue(db.calls().isEmpty());
        }

        @Test
        void lowValuesSelectedKeySetsError() {
            AuthViewResult r = service.execute(req(AuthorizationKey.LOW_VALUES, AidKey.DFHENTER));
            assertEquals(low(16), r.lastScreen().cardnum());
            assertTrue(db.calls().isEmpty());
        }

        @Test
        void readsSummaryThenQualifiedDetailAndTakesSyncpoint() {
            service.execute(req(KEY2, AidKey.DFHENTER));
            assertEquals(List.of("SCHD PSBPAUTB", "GU PAUTSUM0 1", "GNP PAUTDTL1 " + KEY2, "SYNCPOINT"), db.calls());
        }

        @Test
        void unknownAccountLeavesDetailFieldsAsLowValuesFromUninitialisedSegment() {
            Copaus1cCommarea c = Fixtures.commarea(KEY1, true);
            c.setAcctId(99L);
            AuthViewResult r = service.execute(new AuthViewRequest(c, AidKey.DFHENTER));
            AuthViewScreen s = r.lastScreen();
            assertEquals(low(16), s.cardnum());
            assertEquals(Cobol.x(Cobol.lowValues(2) + "/" + Cobol.lowValues(2) + "/" + Cobol.lowValues(2), 10),
                    s.authdt());
            assertEquals(msg(""), s.errmsg());
        }
    }

    @Nested
    class PopulateAuthDetails {

        @Test
        void formatsDateTimeAmountAndApprovedResponse() {
            AuthViewScreen s = service.execute(req(KEY1, AidKey.DFHENTER)).lastScreen();
            assertEquals("10/27/23  ", s.authdt());
            assertEquals("04:12:52  ", s.authtm());
            assertEquals("        1.24", s.authamt());
            assertEquals("A", s.authrsp());
            assertEquals(BmsColor.DFHGREEN.code, s.authrspColor());
            assertEquals("0000-APPROVED       ", s.authrsn());
            assertEquals("003000", s.authcd());
            assertEquals("05  ", s.posemd());
            assertEquals("POS       ", s.authsrc());
            assertEquals("5411", s.mcccd());
            assertEquals("09/27", s.crdexp());
            assertEquals("MAN           ", s.authtyp());
            assertEquals("TXN000000000001", s.trnid());
            assertEquals("P", s.authmtc());
            assertEquals("-         ", s.authfrd());
            assertEquals(Cobol.x("CORNER GROCERY", 25), s.mername());
            assertEquals("MERCH0000000001", s.merid());
            assertEquals(Cobol.x("SPRINGFIELD", 25), s.mercity());
            assertEquals("IL", s.merst());
            assertEquals("62701     ", s.merzip());
        }

        @Test
        void declinedResponseIsRedWithReasonDescription() {
            AuthViewScreen s = service.execute(req(KEY2, AidKey.DFHENTER)).lastScreen();
            assertEquals("D", s.authrsp());
            assertEquals(BmsColor.DFHRED.code, s.authrspColor());
            assertEquals("4100-INSUFFICNT FUND", s.authrsn());
            assertEquals("-   12345.67", s.authamt());
        }

        @Test
        void unknownReasonCodeShowsErrorText() {
            AuthViewScreen s = service.execute(req(KEY3, AidKey.DFHENTER)).lastScreen();
            assertEquals("9999-ERROR          ", s.authrsn());
            assertEquals(" 23456789.99", s.authamt());
        }

        @Test
        void fraudConfirmedShowsStatusAndReportDate() {
            PendingAuthDetails d = db.findDetail(ACCT, KEY1);
            assertTrue(d != null);
            fraudify(KEY1, 'F', "10/28/23");
            AuthViewScreen s = service.execute(req(KEY1, AidKey.DFHENTER)).lastScreen();
            assertEquals("F-10/28/23", s.authfrd());
        }

        @Test
        void fraudRemovedShowsStatusAndReportDate() {
            fraudify(KEY1, 'R', "10/29/23");
            assertEquals("R-10/29/23", service.execute(req(KEY1, AidKey.DFHENTER)).lastScreen().authfrd());
        }
    }

    private void fraudify(AuthorizationKey key, char status, String date) {
        InMemoryPendingAuthDb fresh = new InMemoryPendingAuthDb();
        db.summaries().forEach(fresh::addSummary);
        for (PendingAuthDetails d : db.details(ACCT)) {
            if (d.getAuthorizationKey().equals(key)) {
                d.setAuthFraud(status);
                d.setFraudRptDate(date);
            }
            fresh.addDetail(ACCT, d);
        }
        db = fresh;
        service = new Copaus1cService(db, db, link, Fixtures.CLOCK);
    }

    @Nested
    class SendAuthviewScreenAndHeader {

        @Test
        void headerCarriesTitlesTransactionProgramDateAndTime() {
            AuthViewScreen s = service.execute(req(KEY1, AidKey.DFHENTER)).lastScreen();
            assertEquals("CPVD", s.trnname());
            assertEquals("COPAUS1C", s.pgmname());
            assertEquals("03/04/26", s.curdate());
            assertEquals("05:06:07", s.curtime());
            assertEquals(Cobol.x("      AWS Mainframe Modernization       ", 40), s.title01());
            assertEquals(Cobol.x("              CardDemo                  ", 40), s.title02());
        }
    }

    @Nested
    class ProcessPf8Key {

        @Test
        void advancesToNextAuthorizationAndUpdatesSelection() {
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF8));
            assertEquals(KEY2, r.commarea().cpvd().getPauSelected());
            AuthViewScreen s = r.lastScreen();
            assertEquals("04:12:51  ", s.authtm());
            assertTrue(s.erase());
            assertEquals(msg(""), s.errmsg());
            assertEquals(List.of("SCHD PSBPAUTB", "GU PAUTSUM0 1", "GNP PAUTDTL1 " + KEY1, "GNP PAUTDTL1",
                    "SYNCPOINT"), db.calls());
        }

        @Test
        void atLastAuthorizationShowsMessageWithoutErase() {
            AuthViewResult r = service.execute(req(KEY3, AidKey.DFHPF8));
            AuthViewScreen s = r.lastScreen();
            assertEquals(1, r.screens().size());
            assertEquals(msg("Already at the last Authorization..."), s.errmsg());
            assertFalse(s.erase());
            assertEquals(low(16), s.cardnum());
            assertEquals(KEY3, r.commarea().cpvd().getPauSelected());
        }

        @Test
        void readNextErrorShowsSystemErrorMessage() {
            db.forceNext(Op.GNP, "AI");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF8));
            assertEquals(2, r.screens().size());
            assertEquals(msg(" System error while reading next Auth: Code:AI"), r.lastScreen().errmsg());
        }
    }

    @Nested
    class ReadAuthRecordAndSchedulePsb {

        @Test
        void summaryReadErrorShowsMessageAndStillAttemptsDetailRead() {
            db.forceNext(Op.GU, "AI");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHENTER));
            assertEquals(3, r.screens().size());
            assertEquals(msg(" System error while reading Auth Summary: Code:AI"), r.screens().get(0).errmsg());
            assertEquals(msg(" System error while reading Auth Details: Code:GP"), r.lastScreen().errmsg());
            assertEquals(low(16), r.lastScreen().cardnum());
        }

        @Test
        void detailReadErrorShowsMessage() {
            db.forceNext(Op.GNP_WHERE, "GP");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHENTER));
            assertEquals(msg(" System error while reading Auth Details: Code:GP"), r.lastScreen().errmsg());
            assertEquals(low(16), r.lastScreen().cardnum());
        }

        @Test
        void endOfDbOnDetailIsTreatedAsNotFound() {
            db.forceNext(Op.GNP_WHERE, "GB");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHENTER));
            assertEquals(1, r.screens().size());
            assertEquals(msg(""), r.lastScreen().errmsg());
        }

        @Test
        void psbScheduledTwiceTerminatesAndReschedules() {
            db.forceNext(Op.SCHD, "TC");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHENTER));
            assertEquals(List.of("SCHD PSBPAUTB", "TERM", "SCHD PSBPAUTB"), db.calls().subList(0, 3));
            assertEquals("4111222233334444", r.lastScreen().cardnum());
        }

        @Test
        void psbScheduleFailureShowsMessage() {
            db.forceNext(Op.SCHD, "TE");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHENTER));
            assertEquals(4, r.screens().size());
            assertEquals(msg(" System error while scheduling PSB: Code:TE"), r.screens().get(0).errmsg());
            assertEquals(msg(" System error while reading Auth Summary: Code:TH"), r.screens().get(1).errmsg());
            assertEquals(msg(" System error while reading Auth Details: Code:TH"), r.lastScreen().errmsg());
            assertEquals(low(16), r.lastScreen().cardnum());
            assertEquals(0, db.syncpointCount());
        }

        @Test
        void stringIntoMessageOverlaysWithoutClearingRemainder() {
            db.forceNext(Op.GNP_WHERE, "GP");
            db.forceNext(Op.GNP, "AI");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF8));
            assertEquals(3, r.screens().size());
            assertEquals(msg(" System error while reading Auth Details: Code:GP"), r.screens().get(0).errmsg());
            assertEquals(msg(" System error while reading next Auth: Code:AI:GP"), r.lastScreen().errmsg());
        }
    }

    @Nested
    class MarkAuthFraud {

        @Test
        void reportsFraudUpdatesSegmentAndCommits() {
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            AuthViewScreen s = r.lastScreen();
            assertEquals(msg("AUTH MARKED FRAUD..."), s.errmsg());
            assertEquals("F-03/04/26", s.authfrd());
            RecordingFraudLink.Call call = link.calls().get(0);
            assertEquals("COPAUS2C", call.program());
            assertEquals('F', call.action());
            assertEquals("00000000001", call.acctId());
            assertEquals(1L, call.custId());
            assertEquals('F', call.fraudStatus());
            PendingAuthDetails stored = db.findDetail(ACCT, KEY1);
            assertEquals('F', stored.getAuthFraud());
            assertEquals("03/04/26", stored.getFraudRptDate());
            assertEquals(1, db.syncpointCount());
            assertEquals(List.of("RPT DT: 03/04/26"), r.displays());
            assertEquals(KEY1, r.commarea().cpvd().getPauSelected());
        }

        @Test
        void removesFraudWhenAlreadyConfirmed() {
            fraudify(KEY1, 'F', "10/28/23");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            assertEquals(msg("AUTH FRAUD REMOVED..."), r.lastScreen().errmsg());
            assertEquals('R', link.calls().get(0).action());
            assertEquals('R', db.findDetail(ACCT, KEY1).getAuthFraud());
        }

        @Test
        void linkedProgramFailureShowsItsMessageAndRollsBack() {
            link = new RecordingFraudLink(c -> {
                c.setUpdateFailed();
                c.setActMsg("UNABLE TO INSERT FRAUD RECORD");
                return true;
            });
            service = new Copaus1cService(db, db, link, Fixtures.CLOCK);
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            assertEquals(msg("UNABLE TO INSERT FRAUD RECORD"), r.lastScreen().errmsg());
            assertEquals(1, db.rollbackCount());
            assertEquals(' ', db.findDetail(ACCT, KEY1).getAuthFraud());
            assertFalse(db.calls().stream().anyMatch(c -> c.startsWith("REPL")));
            assertEquals("F-        ", r.lastScreen().authfrd());
        }

        @Test
        void linkNotNormalRollsBackWithoutMessage() {
            link = new RecordingFraudLink(c -> false);
            service = new Copaus1cService(db, db, link, Fixtures.CLOCK);
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            assertEquals(msg(""), r.lastScreen().errmsg());
            assertEquals(1, db.rollbackCount());
            assertEquals(0, db.syncpointCount());
        }

        @Test
        void missingDetailKeepsRequestedKeyInSegmentPassedToLinkAndSelection() {
            AuthorizationKey missing = AuthorizationKey.of(12345, 678L);
            AuthViewResult r = service.execute(req(missing, AidKey.DFHPF5));
            assertEquals(missing, r.commarea().cpvd().getPauSelected());
            assertEquals(1, link.calls().size());
            assertEquals(missing, link.calls().get(0).authKey());
            assertEquals(1, db.rollbackCount());
            assertEquals(0, db.syncpointCount());
        }

        @Test
        void abendDuringSyncpointBacksOutFraudUpdate() {
            SyncpointGateway failingCommit = new SyncpointGateway() {
                @Override
                public void syncpoint() {
                    throw new IllegalStateException("ASP1");
                }

                @Override
                public void rollback() {
                    db.rollback();
                }
            };
            service = new Copaus1cService(db, failingCommit, link, Fixtures.CLOCK);
            org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
                    () -> service.execute(req(KEY1, AidKey.DFHPF5)));
            assertEquals(1, link.calls().size());
            assertTrue(db.calls().stream().anyMatch(c -> c.startsWith("REPL PAUTDTL1")));
            assertEquals(1, db.rollbackCount());
            assertEquals(' ', db.findDetail(ACCT, KEY1).getAuthFraud());
        }

        @Test
        void forcedFwOnReplStillWritesTheFraudFlag() {
            db.forceNext(Op.REPL, "FW");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            assertEquals(msg("AUTH MARKED FRAUD..."), r.lastScreen().errmsg());
            assertEquals('F', db.findDetail(ACCT, KEY1).getAuthFraud());
            assertEquals(1, db.syncpointCount());
        }

        @Test
        void screenToStringMasksCardData() {
            AuthViewScreen s = service.execute(req(KEY1, AidKey.DFHENTER)).lastScreen();
            String card = s.cardnum();
            assertFalse(s.toString().contains(card));
            assertTrue(s.toString().contains(card.substring(12)));
            assertFalse(s.toString().contains(s.crdexp()));
        }

        @Test
        void scheduleFailureNeverCommitsFraudUpdate() {
            db.forceNext(Op.SCHD, "TE");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            assertEquals(msg(" System error while FRAUD Tagging, ROLLBACK||THTH"), r.lastScreen().errmsg());
            assertEquals(0, db.syncpointCount());
            assertEquals(1, db.rollbackCount());
            assertEquals(' ', db.findDetail(ACCT, KEY1).getAuthFraud());
        }

        @Test
        void replFailureRollsBackAndShowsSystemError() {
            db.forceNext(Op.REPL, "DX");
            AuthViewResult r = service.execute(req(KEY1, AidKey.DFHPF5));
            assertEquals(2, r.screens().size());
            assertEquals(msg(" System error while FRAUD Tagging, ROLLBACK||DX"), r.lastScreen().errmsg());
            assertEquals(low(16), r.lastScreen().cardnum());
            assertEquals(1, db.rollbackCount());
            assertEquals(' ', db.findDetail(ACCT, KEY1).getAuthFraud());
        }
    }

    @Test
    void editAmountMatchesPicMinusZzzzzzz9V99() {
        assertEquals("        0.00", Copaus1cService.editAmount(new BigDecimal("0.00")));
        assertEquals("        0.05", Copaus1cService.editAmount(new BigDecimal("0.05")));
        assertEquals("-       1.00", Copaus1cService.editAmount(new BigDecimal("-1")));
        assertEquals(" 99999999.99", Copaus1cService.editAmount(new BigDecimal("99999999.99")));
        assertEquals("        0.00", Copaus1cService.editAmount(new BigDecimal("100000000.00")));
        assertEquals("        1.23", Copaus1cService.editAmount(new BigDecimal("1.239")));
    }
}
